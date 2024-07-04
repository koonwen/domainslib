(* Simple, unbounded and unordered task queue *)
module SimpleTreiber = struct
  type 'a t = 'a list Atomic.t

  let empty () = Atomic.make []

  let rec push t v =
    let cur = Atomic.get t in
    if not (Atomic.compare_and_set t cur (v :: cur)) then
      push t v

  let pop_all t = Array.of_list (Atomic.exchange t [])
end

module StackBased = struct
  type 'a t = {
    stack : 'a SimpleTreiber.t;
    size : int Atomic.t;
    batch_limit : int
  }

  (* Change batch limit to (N)processes *)
  let create ?(batch_limit=max_int) () = 
    {
      stack = SimpleTreiber.empty ();
      size = Atomic.make 0;
      batch_limit;
    }

  let add t elt =
    ignore @@ Atomic.fetch_and_add t.size 1;
    SimpleTreiber.push t.stack elt

  let get t =
    let batch_size = Atomic.exchange t.size 0 in
    let arr = SimpleTreiber.pop_all t.stack in
    let topup = batch_size - Array.length arr in
    let _ = Atomic.fetch_and_add t.size topup in
    arr

  let size t = Atomic.get t.size 
end

include StackBased

(* module ChanBased = struct
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
    ignore @@ Atomic.fetch_and_add t.size 1;
    Chan.send t.chan elt
  let get t = 
    let batch_size = Atomic.exchange t.size 0 in
    let limit = min batch_size t.batch_limit in
    let topup = batch_size - limit in
    let _ = Atomic.fetch_and_add t.size topup in
    Array.init limit (fun _ -> Chan.recv t.chan)
  let size t = Atomic.get t.size 
end

include ChanBased *)

(* type container = ELT.t option array 
   type t = {
   switching : bool Atomic.t;
   primary : container Atomic.t;
   mutable secondary : container;
   size : int Atomic.t
   }

   let create ~batch_size () = {
   switching = Atomic.make false;
   primary = Atomic.make @@ Array.make batch_size None;
   secondary =  Array.make batch_size None;
   size = Atomic.make 0
   }

   let add (t : t) (elt : ELT.t) = 
   (* Make sure switching process is not happening *)
   while Atomic.get t.switching do () done;
   let slot = Atomic.fetch_and_add t.size 1 in
   t.container.(slot) <- Some elt

   let get t = 
   let size = Atomic.get t.size in
   let batch = Array.make t.size None in
   Array.blit t.container 0 batch 0 t.size;
   batch

   let size t = failwith "" *)
