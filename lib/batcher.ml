open Effect
open Effect.Deep

module Types = struct
  type 'a task = unit -> 'a

  (* Need to make this an extensible variant *)
  type message = ..
  type message +=
    Work of (unit -> unit)
  | Quit

  type ds_chan = message Multi_channel.t
  type task_chan = message Multi_channel.t

  type 'a promise_state =
    Returned of 'a
  | Raised of exn * Printexc.raw_backtrace
  | Pending of (('a, unit) continuation * task_chan) list

  type 'a promise = 'a promise_state Atomic.t

  let fill pr v = Atomic.set pr (Returned v)

  type _ t += Wait : 'a promise * task_chan -> 'a t
end

module type DS = sig
  type op
  type res
  (* Should be an (op * 'a promise) array *)
  val bop : (op * res Types.promise) list -> unit
end

module Make (DS : DS) = struct

  open Types
  type message += DS_Op of DS.op * DS.res promise

  type pool_data = {
    domains : unit Domain.t array;
    task_chan : task_chan;
    ds_chan : ds_chan;
    name: string option;
    active_batch : bool Atomic.t;
    batch : (DS.op * DS.res promise) list ref
  }

  type pool = pool_data option Atomic.t

  let get_pool_data p =
    match Atomic.get p with
    | None -> invalid_arg "pool already torn down"
    | Some p -> p

  let cont v (k, c) = Multi_channel.send c (Work (fun _ -> continue k v))
  let discont e bt (k, c) = Multi_channel.send c (Work (fun _ ->
    discontinue_with_backtrace k e bt))

  let do_task (type a) (f : unit -> a) (p : a promise) : unit =
    let action, result =
      try
        let v = f () in
        cont v, Returned v
      with e ->
        let bt = Printexc.get_raw_backtrace () in
        discont e bt, Raised (e, bt)
    in
    match Atomic.exchange p result with
    | Pending l -> List.iter action l
    |  _ -> failwith "Task.do_task: impossible, can only set result of task once"

  let async pool f =
    let pd = get_pool_data pool in
    let p = Atomic.make (Pending []) in
    Multi_channel.send pd.task_chan (Work (fun _ -> do_task f p));
    p

  let await pool promise =
    let pd = get_pool_data pool in
    match Atomic.get promise with
    | Returned v -> v
    | Raised (e, bt) -> Printexc.raise_with_backtrace e bt
    | Pending _ -> perform (Wait (promise, pd.task_chan))

  (* let rec collect_and_launch_batch pd =
    match Multi_channel.recv_op pd.ds_chan with
    | Some DS_Op (op, p) -> 
      pd.batch <- (op, p) :: pd.batch;
      collect_and_launch_batch pd;
    | _ -> 
      DS.bop pd.batch; 
      Atomic.set pd.active_batch false *)
    
  let try_launch (type a) (promise : a promise) ds_chan batch active_batch : a = 
    match Atomic.get promise with
    | Returned v -> v
    | Raised (e, bt) -> Printexc.raise_with_backtrace e bt
    | Pending _ -> 
      begin 
        let seen = Atomic.get active_batch in
        if not seen then
          if Atomic.compare_and_set active_batch seen false
              then DS.bop !batch;
        match Atomic.get promise with
        | Returned v -> v
        | Raised (e, bt) -> Printexc.raise_with_backtrace e bt
        | Pending _ -> perform (Wait (promise, ds_chan))
      end

(* Doesn't automatically switch, requires a worker to encounter the DS_Op *)
  let ds_op_wrapperv1 pool op =
    let pd = get_pool_data pool in
    let p = Atomic.make (Pending []) in
    pd.batch := (op, p) :: !(pd.batch);
    try_launch p pd.ds_chan pd.batch pd.active_batch

  let step (type a) (f : a -> unit) (v : a) : unit =
    try_with f v
    { effc = fun (type a) (e : a t) ->
        match e with
        | Wait (p,c) -> Some (fun (k : (a, _) continuation) ->
            let rec loop () =
              let old = Atomic.get p in
              match old with
              | Pending l ->
                  if Atomic.compare_and_set p old (Pending ((k,c)::l)) then ()
                  else (Domain.cpu_relax (); loop ())
              | Returned v -> continue k v
              | Raised (e,bt) -> discontinue_with_backtrace k e bt
            in
            loop ())
        | _ -> None }

  let rec worker task_chan =
    match Multi_channel.recv task_chan with
    | Quit -> Multi_channel.clear_local_state task_chan
    | Work f -> step f (); worker task_chan
    | DS_Op _ -> failwith "unimplemented"
    | _ -> failwith "Impossible"

  let rec free_mode task_chan ds_chan batch active_batch =
    match Multi_channel.recv task_chan with
    | Quit -> Multi_channel.clear_local_state task_chan
    | Work f -> step f (); free_mode task_chan ds_chan batch active_batch
    (* This can be optimized at a later time *)
    | DS_Op _ as msg -> Multi_channel.send ds_chan msg; 
      ds_mode task_chan ds_chan batch active_batch
    (* Need this last case for extensible variant types *)
    | _ -> failwith "Impossible"
  and ds_mode task_chan ds_chan batch active_batch =
    match Multi_channel.recv_op ds_chan with
    | Some Work f -> step f (); ds_mode task_chan ds_chan batch active_batch
    | Some DS_Op (op, p) -> 
      batch := (op, p) :: !batch;
      let f () = try_launch p ds_chan batch active_batch in
      Multi_channel.send ds_chan (Work (fun () -> do_task f p)); 
      ds_mode task_chan ds_chan batch active_batch
    | Some _ -> failwith "Should not happen"
    (* No more tasks found, return to free_mode *)
    | None -> 
      (* if List.length pd.batch <> 0 
        then 
          (Multi_channel.send pd.ds_chan (Work (fun () -> try_launch pd));ds_mode pd)
        else *)
           free_mode task_chan ds_chan batch active_batch

  (* Because of this, we cannot run the program with only 1 domain, it will not be able to pick up the tasks *)
  let run (type a) pool (f : unit -> a) : a =
    let pd = get_pool_data pool in
    let p = Atomic.make (Pending []) in
    step (fun _ -> do_task f p) ();
    let rec loop () : a =
      match Atomic.get p with
      | Pending _ ->
          begin
            try
              match Multi_channel.recv_poll pd.task_chan with
              | Work f -> step f ()
              | Quit -> failwith "Task.run: tasks are active on pool"
              | DS_Op (op, p) -> Multi_channel.send pd.ds_chan (DS_Op (op, p))
              | _ -> failwith "Need for extensible variant"
            with Exit -> Domain.cpu_relax ()
          end;
          loop ()
    | Returned v -> v
    | Raised (e, bt) -> Printexc.raise_with_backtrace e bt
    in
    loop ()

  let named_pools = Hashtbl.create 8
  let named_pools_mutex = Mutex.create ()

  let setup_pool ?name ~num_domains () =
    if num_domains < 0 then
      invalid_arg "Task.setup_pool: num_domains must be at least 0"
    else
    let task_chan = Multi_channel.make (num_domains+1) in
    let ds_chan = Multi_channel.make (num_domains+1) in
    let active_batch = Atomic.make false in
    let batch = ref [] in
    let domains = Array.init num_domains (fun _ ->
      Domain.spawn (fun _ -> free_mode task_chan ds_chan batch active_batch))
    in
    let p = Atomic.make (Some {domains; task_chan; ds_chan; name; active_batch; batch}) in
    begin match name with
      | None -> ()
      | Some x ->
          Mutex.lock named_pools_mutex;
          Hashtbl.add named_pools x p;
          Mutex.unlock named_pools_mutex
    end;
    p

  let teardown_pool pool =
    let pd = get_pool_data pool in
    for _i=1 to Array.length pd.domains do
      Multi_channel.send pd.task_chan Quit
    done;
    Multi_channel.clear_local_state pd.task_chan;
    Array.iter Domain.join pd.domains;
    (* Remove the pool from the table *)
    begin match pd.name with
    | None -> ()
    | Some n ->
        Mutex.lock named_pools_mutex;
        Hashtbl.remove named_pools n;
        Mutex.unlock named_pools_mutex
    end;
    Atomic.set pool None

  let lookup_pool name =
    Mutex.lock named_pools_mutex;
    let p = Hashtbl.find_opt named_pools name in
    Mutex.unlock named_pools_mutex;
    p

  let get_num_domains pool =
    let pd = get_pool_data pool in
    Array.length pd.domains + 1

  let parallel_for ?(chunk_size=0) ~start ~finish ~body pool =
    let pd = get_pool_data pool in
    let chunk_size = if chunk_size > 0 then chunk_size
        else begin
          let n_domains = (Array.length pd.domains) + 1 in
          let n_tasks = finish - start + 1 in
          if n_domains = 1 then n_tasks
          else max 1 (n_tasks/(8*n_domains))
        end
    in
    let rec work pool fn s e =
      if e - s < chunk_size then
        for i = s to e do fn i done
      else begin
        let d = s + ((e - s) / 2) in
        let left = async pool (fun _ -> work pool fn s d) in
        work pool fn (d+1) e;
        await pool left
      end
    in
    work pool body start finish

end