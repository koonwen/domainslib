module Counter = struct
  open Domainslib.Batcher.Promises

  type op = Incr | Decr | Get [@@deriving show]
  type res = Unit | Res of int [@@deriving show]
  let t = Atomic.make 0

  let eval = function
  | Incr ->  Atomic.incr t; Unit
  | Decr -> Atomic.decr t; Unit
  | Get -> Res (Atomic.get t)

  let bop (op_res_ls : (op * res Domainslib.Batcher.Promises.promise) list) =
    (* Printf.printf "Starting BOP\n%!"; *)
    List.iter (fun (op, res_pr) -> 
      (* Printf.printf "Starting %s\n%!" @@ show_op op; *)
      fill res_pr (eval op)) op_res_ls;
    (* Unix.sleep 1; *)
    (* Printf.printf "Ending BOP\n%!" *)

end

module Counter_Batched = struct
  module BC = Domainslib.Batcher.Make (Counter)
  include BC
  let pool = setup_pool ~num_domains:6 ()
  let run f = run pool f; teardown_pool pool
  let increment () = match ds_op_wrapperv1 pool Incr with Unit -> () 
  | _ -> failwith "Error"
  let decrement () = match ds_op_wrapperv1 pool Decr with Unit -> ()
  | _ -> failwith "Error"
  let get () = match ds_op_wrapperv1 pool Get with Res i -> i
    | _ -> failwith "Error"
  let parallel_for = BC.parallel_for pool
  let async  (type a) (f: unit -> a) : a Domainslib.Batcher.Promises.promise =    async pool f
  let await (type a) (pr : a Domainslib.Batcher.Promises.promise) : a =
     await pool pr
end


let main () = 
  let open Counter_Batched in 
  parallel_for ~start:1 ~finish:100_000
  ~body:(fun _ -> Counter_Batched.increment ());
  (* let p1 = async (fun () -> increment ()) in
  let p2 = async (fun () -> increment ()) in
  let p3 = async (fun () -> increment ()) in
  List.iter await [p1;p2;p3]; *)
  get () |> Printf.printf "Result = %d\n"

let () = Counter_Batched.run main
