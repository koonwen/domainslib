[@@@warning "-27-32"]
module ChanBased = struct
  type 'a t = {
    chan : 'a Chan.t;
    size : int Atomic.t;
    batch_limit : int
  }

  (* Change batch limit to (N)processes *)
  let create ?(batch_limit=max_int) () = 
    {
      chan = Chan.make_unbounded ();
      size = Atomic.make 0;
      batch_limit;
    }
  let add t elt =
    let _ = Atomic.fetch_and_add t.size 1 in
    Chan.send t.chan elt
  let get t = 
    let batch_size = Atomic.exchange t.size 0 in
    let limit = min batch_size t.batch_limit in
    let topup = max (batch_size - limit) 0 in
    let _ = Atomic.fetch_and_add t.size topup in
    Array.init limit (fun _ -> Chan.recv t.chan)
  let size t = Atomic.get t.size 
end

(* include ChanBased *)

type 'a container = 'a option array 
type 'a t = {
  switching : bool Atomic.t;
  mutable primary : 'a container;
  mutable secondary : 'a container;
  batch_size : int;
  size : int Atomic.t
}

let create ?(batch_size=1000) () = {
  switching = Atomic.make false;
  primary = Array.make batch_size None;
  secondary =  Array.make batch_size None;
  batch_size;
  size = Atomic.make 0
}

let add pool (t : 'a t) elt = 
  (* Make sure switching process is not happening *)
  let is_done = ref false in
  let current_size = ref 0 in
  while not !is_done do
    while Atomic.get t.switching ||
          (current_size := Atomic.get t.size; !current_size >= t.batch_size)
    do Task.yield pool done;
    if Atomic.compare_and_set t.size !current_size (!current_size + 1) then
      begin
        t.primary.(!current_size) <- Some elt; 
        is_done := true
      end
  done

let rec get pool t = 
  while Atomic.get t.switching do
    Task.yield pool
  done;
  if Atomic.compare_and_set t.switching false true then
    begin
      let tmp = t.primary in
      t.primary <- t.secondary;
      t.secondary <- tmp;
      let size = Atomic.exchange t.size 0 in
      let res = Array.init size (fun i -> tmp.(i) |> Option.get) in
      Atomic.set t.switching false;
      res
    end
  else get pool t

let size t = Atomic.get t.size
