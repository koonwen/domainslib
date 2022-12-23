module type DS = sig
  type t
  type batch_op                 (* Polymorphic Variant *)
  val create : unit -> t
  val bop : Task.pool -> [> `Null] array -> unit
end

module Make (DS : DS) : sig
  type 'a t
    
  val create : max_batch_size:int -> [> `Null] t
  val push : 'a t -> 'a -> unit
  val try_launch : Task.pool -> [> `Null] t -> unit
end = struct
  module Q = struct
    include Chan
    let pop = recv_poll
    let push = send
  end
  
  type 'a t = {
    running : bool Atomic.t;
    ds : DS.t;
    q : 'a Q.t;
    container : 'a array
  }

  let push t = Q.push t.q

  let create ~max_batch_size =
    { running = Atomic.make false;
      ds = DS.create ();
      q = Chan.make_unbounded ();
      container = Array.make max_batch_size `Null }
    
  let rec try_launch pool t =
    if Atomic.compare_and_set t.running false true then
      match Q.pop t.q with
      | Some op -> t.container.(0) <- op;
        (let i = ref 1 in
         while
           match Q.pop t.q with
           | Some op -> t.container.(!i) <- op; incr i; true
           | None -> false
         do () done;
         let batch = Array.init !i (fun i -> t.container.(i)) in
         DS.bop pool batch;
         Atomic.set t.running false;
         try_launch pool t)
      | None -> Atomic.set t.running false
end
