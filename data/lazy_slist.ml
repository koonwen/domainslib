open Batteries
open BatteriesThread

module type Compare = sig
  type t
  val compare : t -> t -> int
  val to_string : t -> string
  val hash : t -> int
end

module Node (V : Compare) = struct
  type t = Null | Node of data
  and data = {
    mutable key : int;
    item : V.t option;
    next : t array;
    lock : RMutex.t;
    marked : bool Atomic.t;
    fully_linked : bool Atomic.t;
    toplevel : int;
  }

  let node_to_string = function
    | Null -> "Null"
    | Node {item;key;_} -> 
      if Option.is_none item then Printf.sprintf "Node(key:%d; item:None)" key else
        Printf.sprintf "Node(key:%d; item:%s)" key (item |> Option.get |> V.to_string) 

  let next_to_string {next;_} =
    Array.fold (fun acc node -> acc ^ "; " ^ (node_to_string node)) "" next

  let to_string = function
    | Null -> "Null"
    | Node d as n -> Printf.sprintf "%s [%s]" (node_to_string n) (next_to_string d)

  let make ?item height =
    let key = match item with  Some item -> V.hash item | None -> 0 in
    Node {
      key;
      item;
      next = Array.make (height + 1) Null;
      lock = RMutex.create ();
      marked = Atomic.make false;
      fully_linked = Atomic.make false;
      toplevel = height
    }
  let ( !^ ) = function
    | Null -> failwith "Tried to dereference Null node"
    | Node data -> data
  let ( !!^ ) node = !^(!node)
  let overide_key _key = function
    | Null -> failwith "Tried to overide Null node"
    | Node data -> data.key <- _key

  let lock = function
    | Null -> failwith "Tried to lock Null node"
    | Node {lock; _} -> RMutex.lock lock

  let unlock = function
    | Null -> failwith "Tried to unlock Null node"
    | Node {lock; _} -> RMutex.unlock lock
end

module Make (V : Compare) = struct

  module Node = Node(V)
  let maxlevel = 32
  let random_level () =
    let lvl = ref 0 in
    while (Random.float 1.) < 0.5 && !lvl < maxlevel do
      incr lvl
    done;
    !lvl

  let mk_sentinel key = 
    let node = Node.make maxlevel in
    Node.overide_key key node;
    node

  let head = mk_sentinel min_int
  let tail = mk_sentinel max_int

  (* Initial structure between the sentinels *)
  let init () = 
    let open Node in
    Array.iteri (fun i _ -> !^head.next.(i) <- tail) !^head.next

  let () = init ()

  let find (item: V.t) (preds : Node.t array) (succs : Node.t array) : int =
    let open Node in
    let v = V.hash item in
    let lfound = ref (-1) in
    let pred = ref head in
    for level = maxlevel downto 0 do
      let curr = ref (!!^pred).next.(level) in
      while v > !!^curr.key do
        pred := !curr;
        curr := !!^pred.next.(level)
      done;
      if !lfound = -1 && v = !!^curr.key then lfound := level;
      preds.(level) <- !pred;
      succs.(level) <- !curr
    done;
    !lfound

  let contains (item : V.t) : bool =
    let open Node in
    let preds = Array.make (maxlevel+1) Node.Null in
    let succs = Array.make (maxlevel+1) Node.Null in
    let lfound = find item preds succs in
    lfound <> -1 && 
    (Atomic.get !^(succs.(lfound)).fully_linked) && 
    (not (Atomic.get !^(succs.(lfound)).marked))

  let add (item : V.t) : bool =  
    let open Node in
    let exception False in
    let exception True in
    let toplevel = random_level () in
    let preds = Array.make (maxlevel+1) Node.Null in
    let succs = Array.make (maxlevel+1) Node.Null in

    let aux_add () = 
      while true do
        let skip = ref false in
        let lfound = find item preds succs in
        if lfound <> -1 then (
          let node_found = succs.(lfound) in
          if not (Atomic.get !^node_found.marked) then (
            while not (Atomic.get !^node_found.fully_linked) do () done;
            raise False
          );
          skip := true;
        );
        if (not !skip) then (
          let highestlocked = ref (-1) in
          try (
            let pred, succ = ref Null, ref Null in
            let valid = ref true in
            let level = ref 0 in
            while (!valid && (!level <= toplevel)) do
              pred := preds.(!level);
              succ := succs.(!level);
              lock !pred;
              highestlocked := !level;
              valid := 
                (not (Atomic.get !!^pred.marked)) && 
                (not (Atomic.get !!^succ.marked)) &&
                (!!^pred.next.(!level) == !succ);
              level := !level + 1;
            done;
            if (not !valid) then skip := true;
            if (not !skip) then (
              let new_node = make ~item toplevel in
              (* first link succs *)
              for lvl = 0 to toplevel do
                !^new_node.next.(lvl) <- succs.(lvl);
              done;
              (* then link next fields of preds *)
              for lvl = 0 to toplevel do
                !^(preds.(lvl)).next.(lvl) <- new_node
              done;
              Atomic.set !^new_node.fully_linked true;
              raise True
            );
            for lvl = 0 to !highestlocked do
              unlock preds.(lvl)
            done
          ) with True ->
            (for lvl = 0 to !highestlocked do
               unlock preds.(lvl)
             done; raise True);
        );
      done
    in
    let result = ref None in
    (try aux_add () with 
     | False -> result := Some false 
     | True -> result := Some true);
    if !result = None then failwith "[add] This is unreachable" else 
      Option.get !result

  (* let batch_insert pool batch =
     let open Node in
     let tmp_head = mk_sentinel min_int in
     let tmp_tail = mk_sentinel max_int in
     Array.iteri (fun i _ -> !^tmp_head.next.(i) <- tmp_tail) !^tmp_head.next;

     Array.sort V.compare batch;
     let preds = Array.make (maxlevel+1) Node.Null in
     let succs = Array.make (maxlevel+1) Node.Null in

     failwith "" *)

  let print () = 
    let open Node in
    let rec walk height = function
      | Node {next;_} as n -> 
        Printf.printf "%s ->" @@ node_to_string n;
        walk height next.(height)
      | Null -> print_endline "Null"
    in
    for i = maxlevel downto 0 do
      walk i head;
      print_newline ()
    done
  let size () =
    let open Node in
    let rec walk = function
      | Node {next; _} -> 1 + walk next.(0)
      | Null -> -1
    in
    walk (!^head.next.(0))

end