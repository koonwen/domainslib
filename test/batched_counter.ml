module Counter = struct
  type op = Incr | Decr | Get [@@deriving show]
  type res = Unit | Res of int [@@deriving show]

end

module Counter_Batched = struct
  include Domainslib.Batcher.Make (Counter)
  open Domainslib.Batcher.Promises  
  open Counter

  let t = Atomic.make 0

  let eval = function
    | Incr ->  Atomic.incr t; Unit
    | Decr -> Atomic.decr t; Unit
    | Get -> Res (Atomic.get t)
  let pool = setup_pool ~num_domains:3 ()
  let run f = run pool f; teardown_pool pool
  let parallel_for = parallel_for pool
  let async  (type a) (f: unit -> a) : a Domainslib.Batcher.Promises.promise =    async pool f
  let await (type a) (pr : a Domainslib.Batcher.Promises.promise) : a =
    await pool pr
  let bop (op_res_ls : (op * res promise) list) =
    (* Printf.printf "Starting BOP\n%!"; *)
    List.iter (fun (op, res_pr) ->
        (* Printf.printf "Starting %s\n%!" @@ show_op op; *)
        fill res_pr (eval op)) op_res_ls
  (* Unix.sleep 1; *)
  (* Printf.printf "Ending BOP\n%!" *)
  let increment () = match ds_op_wrapperv1 pool bop Incr with Unit -> () 
                                                            | _ -> failwith "Error"
  let decrement () = match ds_op_wrapperv1 pool bop Decr with Unit -> ()
                                                            | _ -> failwith "Error"
  let get () = match ds_op_wrapperv1 pool bop Get with Res i -> i
                                                     | _ -> failwith "Error"
end


let main () = 
  let open Counter_Batched in 
  parallel_for ~start:1 ~finish:100_000
    ~body:(fun _ -> Counter_Batched.increment ());
  get () |> Printf.printf "Result = %d\n";
  Hashtbl.iter (fun key value -> Printf.printf "key = %d, value = %d\n" key value) stats

let () = Counter_Batched.run main
