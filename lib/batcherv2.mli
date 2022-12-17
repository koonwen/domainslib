module Promises : sig
  type 'a task
  type message = ..
  type message += Work of (unit -> unit) | Quit
  type ds_chan = message Multi_channel.t
  type task_chan = message Multi_channel.t
  type !'a promise
  val fill : 'a promise -> 'a -> unit
end

module type DS = sig 
  type op 
  type res 
end

module Make (DS : DS) : sig
  type op = DS.op
  type res = DS.res
  type pool
  type bop = (op * res Promises.promise) list -> unit
  val stats : (int, int) Hashtbl.t
  val async : pool -> (unit -> 'a) -> 'a Promises.promise
  val await : pool -> 'a Promises.promise -> 'a
  val ds_op_wrapperv1 : pool -> bop -> op -> res
  val run : pool -> (unit -> 'a) -> 'a
  val setup_pool : ?name:string -> num_domains:int -> unit -> pool
  val teardown_pool : pool -> unit
  val lookup_pool : string -> pool option
  val get_num_domains : pool -> int
  val parallel_for :
    ?chunk_size:int ->
    start:int ->
    finish:int -> body:(int -> unit) -> pool -> unit
end
