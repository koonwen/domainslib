module B = Domainslib.Batcher

let f () = Unix.sleep 2; Printf.printf "Done%!"

let with_pool f = 
  let pool = B.setup_pool ~name:"pool" ~num_domains:0 () in
  let res = B.run pool f in
  B.teardown_pool pool;
  res
let asyncp f = 
  match B.lookup_pool "pool" with
  | Some pool -> B.async pool f
  | None -> failwith "No pool"

let awaitp p = 
  match B.lookup_pool "pool" with
  | Some pool -> B.await pool p
  | None -> failwith "No pool"

let () =  
  with_pool (fun () ->
    awaitp @@ asyncp f)