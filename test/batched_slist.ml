module Batched_Slist : Domainslib.Batcher.DS = struct

  type op = Ins of int
  type res = Unit

  type 'a pointer = Null | Pointer of 'a ref

  let ( !^ ) = function
    | Null -> invalid_arg "Attempt to dereference the null pointer"
    | Pointer r -> !r

  let is_nil = function Null -> true | _ -> false

  type 'a slist = { mutable hdr : 'a node pointer; mutable level : int }
  and 'a node = { mutable value : 'a; forward : 'a node pointer array }

  let maxlevel = 8

  let slist =
    let hdr =
      Pointer
        (ref { value = max_int; forward = Array.make (maxlevel + 1) Null })
    in
    ref { hdr; level = 0 }

  let nil = !slist.hdr

  let random_level () =
    let lvl = ref 0 in
    while Random.float 1. < 0.5 && !lvl < maxlevel do
      incr lvl
    done;
    !lvl

  let init () =
    !slist.hdr <-
      Pointer (ref { value = min_int; forward = Array.make (maxlevel + 1) nil });
    !slist.level <- 0

  let make_node lvl value =
    Pointer (ref { value; forward = Array.make (lvl + 1) nil })

  let insert elt =
    (* Search for Node *)
    let update = Array.make (maxlevel + 1) Null in
    let x = ref !slist.hdr in
    for i = !slist.level downto 0 do
      while
        (not (is_nil !^(!x).forward.(i))) && !^(!^(!x).forward.(i)).value < elt
      do
        x := !^(!x).forward.(i)
      done;
      update.(i) <- !x
    done;
    x := !^(!x).forward.(0);
    (* Check if we are at the correct point *)
    let x = !x in
    if (not (is_nil x)) && !^x.value = elt then !^x.value <- elt
    else
      let lvl = random_level () in
      (* Printf.printf "Inserting with level %d\n" lvl; *)
      if lvl > !slist.level then (
        for i = !slist.level + 1 to lvl do
          update.(i) <- !slist.hdr
        done;
        !slist.level <- lvl);
      let x = make_node lvl elt in
      for i = 0 to lvl do
        !^x.forward.(i) <- !^(update.(i)).forward.(i);
        !^(update.(i)).forward.(i) <- x
      done

  let insert_v2 elt =
    (* Search for Node *)
    let update = Array.make (maxlevel + 1) Null in
    let x = ref !slist.hdr in
    for i = !slist.level downto 0 do
      while nil != !^(!x).forward.(i) && !^(!^(!x).forward.(i)).value < elt do
        x := !^(!x).forward.(i)
      done;
      update.(i) <- !x
    done;
    x := !^(!x).forward.(0);
    (* Check if we are at the correct point *)
    let x = !x in
    if nil != x && !^x.value = elt then !^x.value <- elt
    else
      let lvl = random_level () in
      (* Printf.printf "Inserting with level %d\n" lvl; *)
      if lvl > !slist.level then (
        for i = !slist.level + 1 to lvl do
          update.(i) <- !slist.hdr
        done;
        !slist.level <- lvl);
      let x = make_node lvl elt in
      for i = 0 to lvl do
        !^x.forward.(i) <- !^(update.(i)).forward.(i);
        !^(update.(i)).forward.(i) <- x
      done

  let remove elt =
    let update = Array.make (maxlevel + 1) Null in
    let x = ref !slist.hdr in
    for i = !slist.level downto 0 do
      while
        (not (is_nil !^(!x).forward.(i))) && !^(!^(!x).forward.(i)).value < elt
      do
        x := !^(!x).forward.(i)
      done;
      update.(i) <- !x
    done;
    x := !^(!x).forward.(0);
    if is_nil !x || !^(!x).value <> elt then raise Not_found;
    let flag, i = (ref true, ref 0) in
    (* Adjust forward pointers *)
    while !flag && !i <= !slist.level do
      (* Printf.printf "STUCK HERE%!"; *)
      if !^(update.(!i)).forward.(!i) != !x then flag := false;
      if !flag then !^(update.(!i)).forward.(!i) <- !^(!x).forward.(!i);
      incr i
    done;
    (* Adjust header level *)
    while !slist.level > 0 && !^(!slist.hdr).forward.(!slist.level) == nil do
      (* Printf.printf "NO HALT%!"; *)
      !slist.level <- !slist.level - 1
    done

  let search elt =
    let x = ref !slist.hdr in
    for i = !slist.level downto 0 do
      while
        (not (is_nil !^(!x).forward.(i))) && !^(!^(!x).forward.(i)).value < elt
      do
        x := !^(!x).forward.(i)
      done
    done;
    x := !^(!x).forward.(0);
    if !x <> nil && !^(!x).value = elt then Some !x else None

  let rec single_traversal i p =
    if is_nil p then print_endline "Null"
    else
      let { value; forward; _ } = !^p in
      Printf.printf "(%d) -> " value;
      single_traversal i forward.(i)

  let rec single_traversal_n i p n =
    if is_nil p then print_endline "Null"
    else if n < 0 then print_endline "..."
    else
      let { value; forward; _ } = !^p in
      Printf.printf "(%d) -> " value;
      single_traversal_n i forward.(i) (n - 1)

  let rec single_traversal_w_cycle_detection i p seen =
    if is_nil p then print_endline "Null"
    else
      let { value; forward; _ } = !^p in
      Printf.printf "(%d) -> " value;
      if seen then Printf.printf "CYCLE\n"
      else if value = max_int then
        single_traversal_w_cycle_detection i forward.(i) true
      else single_traversal_w_cycle_detection i forward.(i) seen

  let print_slist () =
    for i = !slist.level downto 0 do
      single_traversal i !slist.hdr;
      print_newline ()
    done

  let print_slist_v2 () =
    for i = !slist.level downto 0 do
      single_traversal_w_cycle_detection i !slist.hdr false;
      print_newline ()
    done

  let print_node { value : 'a; forward : 'a node pointer array } =
    Printf.printf "value : %d\n" value;
    let level = Array.length forward in
    for i = level - 1 downto 0 do
      Printf.printf "Level %d -> %d\n" i !^(forward.(i)).value
    done

  let size () =
    let rec aux acc p =
      if is_nil p then acc
      else
        let { value; forward; _ } = !^p in
        if value = max_int || value = min_int then aux acc forward.(0)
        else (* Printf.printf "(%d) -> " value; *)
          aux (acc + 1) forward.(0)
    in
    aux 0 !slist.hdr

  type info = {
    maxinsertlevel : int ref;
    level : int array;
    data : int array;
    new_nodes : int node pointer array;
    new_nodes_back : int node pointer array;
  }

  type prep = {
    new_nodes : int node pointer array;
    level : int array;
    size : int ref;
    prev_nodes : int array;
    maxinsertlevel : int ref;
    new_nodes_back : int node pointer array;
  }

  let batch_create_nodes idx
      { maxinsertlevel; level; data; new_nodes; new_nodes_back; _ } =
    let lvl = random_level () in
    if !maxinsertlevel < lvl then maxinsertlevel := lvl;
    level.(idx) <- lvl;
    new_nodes.(idx) <- make_node lvl data.(idx);
    new_nodes_back.(idx) <- make_node lvl data.(idx)

  let batch_relate_nodes idx
      { new_nodes; level; size; prev_nodes; maxinsertlevel; new_nodes_back } =
    let y = new_nodes.(idx) in
    let check = ref (idx + 1) in
    for lvl = 0 to level.(idx) do
      !^y.forward.(lvl) <- nil;
      let j, flag = (ref !check, ref true) in
      while !j < !size && !flag do
        if level.(!j) >= lvl then (
          (* Set forward of the (idx) node at the ith level to node j *)
          !^y.forward.(lvl) <- new_nodes.(!j);
          !^(new_nodes_back.(!j)).forward.(lvl) <- y;
          (* Set the previous pointer *)
          prev_nodes.(((!maxinsertlevel + 1) * !j) + lvl) <- idx;
          check := !j;
          flag := false);
        incr j
      done
    done

  let print_relation
      { new_nodes; level; size; prev_nodes; maxinsertlevel; new_nodes_back } =
    for idx = 0 to !size - 1 do
      Printf.printf "(idx %d)\n" idx;
      for lvl = level.(idx) downto 0 do
        Printf.printf "Forward_nodes:\n";
        single_traversal_n lvl new_nodes.(idx) 2;
        Printf.printf "Back_nodes:\n";
        single_traversal_n lvl new_nodes_back.(idx) 2
      done;
      print_newline ()
    done

  let batch_merge_lists idx prep =
    let exception Break in
    let aux idx
        { new_nodes; level; prev_nodes; new_nodes_back; maxinsertlevel; _ } =
      let x = ref !slist.hdr in
      let y = new_nodes.(idx) in
      let yback = new_nodes_back.(idx) in
      let update = Array.make (maxlevel + 1) !slist.hdr in
      assert (!^y.value == !^yback.value);

      for i = !slist.level downto 0 do
        while
          nil != !^(!x).forward.(i) && !^(!^(!x).forward.(i)).value < !^y.value
        do
          x := !^(!x).forward.(i)
        done;

        (* No duplicates *)
        if
          !x != nil
          && !^(!x).forward.(i) != nil
          && !^(!^(!x).forward.(i)).value = !^y.value
        then raise Break;

        update.(i) <- !x
      done;

      for i = 0 to level.(idx) do
        if
          !^y.forward.(i) == nil
          || !^(!^(update.(i)).forward.(i)).value <= !^(!^y.forward.(i)).value
        then
          if !^(update.(i)).forward.(i) != nil then
            !^y.forward.(i) <- !^(update.(i)).forward.(i);
        let prev_node = ref prev_nodes.(((!maxinsertlevel + 1) * idx) + i) in
        if
          !prev_node = -1
          || !^(new_nodes.(!prev_node)).value <= !^(update.(i)).value
        then (
          !^yback.forward.(i) <- update.(i);
          prev_nodes.(((!maxinsertlevel + 1) * idx) + i) <- -2)
      done
    in
    try aux idx prep with Break -> ()

  let batch_merge_lists idx
      { new_nodes; level; prev_nodes; new_nodes_back; maxinsertlevel; _ } =
    let x = ref !slist.hdr in
    let y = new_nodes.(idx) in
    let yback = new_nodes_back.(idx) in
    let update = Array.make (maxlevel + 1) !slist.hdr in
    assert (!^y.value == !^yback.value);

    let i, flag = (ref !slist.level, ref true) in
    while !i >= 0 && !flag do
      (* Search for position to slot in *)
      while
        (not (is_nil !^(!x).forward.(!i)))
        && !^(!^(!x).forward.(!i)).value < !^y.value
      do
        x := !^(!x).forward.(!i)
      done;
      (* Check no duplicates *)
      if
        !x != nil
        && !^(!x).forward.(!i) != nil
        && !^(!^(!x).forward.(!i)).value = !^y.value
      then flag := false;
      update.(!i) <- !x;
      decr i
    done;
    if !flag = false then ()
    else
      let prev_node = ref (-1) in
      for i = 0 to level.(idx) do
        if
          is_nil !^y.forward.(i)
          || !^(!^(update.(i)).forward.(i)).value <= !^(!^y.forward.(i)).value
        then
          if not (is_nil !^(update.(i)).forward.(i)) then
            !^y.forward.(i) <- !^(update.(i)).forward.(i);
        assert (
          let ind = ((!maxinsertlevel + 1) * idx) + i in
          if ind >= Array.length prev_nodes then (
            Printf.printf
              "\nmaxinsertlevel = %d idx = %d i = %d; array_len = %d\n"
              !maxinsertlevel idx i (Array.length prev_nodes);
            false)
          else true);
        prev_node := prev_nodes.(((!maxinsertlevel + 1) * idx) + i);
        if
          !prev_node = -1
          || !^(new_nodes.(!prev_node)).value <= !^(update.(i)).value
        then (
          !^yback.forward.(i) <- update.(i);
          prev_nodes.(((!maxinsertlevel + 1) * idx) + i) <- -2)
      done

  let remove_duplicates arr num_elements =
    if num_elements <= 1 then num_elements
    else
      let j = ref 0 in
      for i = 0 to num_elements - 2 do
        if arr.(i) <> arr.(i + 1) then (
          arr.(!j) <- arr.(i);
          incr j)
      done;
      arr.(!j) <- arr.(num_elements - 1);
      incr j;
      !j

  let search_nodes elt = match search elt with Some _ -> max_int | None -> elt

  let count_elems arr =
    Array.fold_left (fun acc x -> if x <> max_int then acc + 1 else acc) 0 arr

  let batch_insert_par (pool : Domainslib.Task.pool) data num_elements =
    let module T = Domainslib.Task in
    let num_elements = ref num_elements in
    T.parallel_for ~start:0 ~finish:(!num_elements - 1)
      ~body:(fun idx -> data.(idx) <- search_nodes data.(idx))
      pool;
    Array.stable_sort Int.compare data;
    num_elements := remove_duplicates data !num_elements;
    num_elements := count_elems data;

    let level = Array.make !num_elements 0 in
    let maxinsertlevel = ref 0 in
    let new_nodes = Array.make !num_elements Null in
    let new_nodes_back = Array.make !num_elements Null in

    let create_node_args =
      { maxinsertlevel; level; data; new_nodes; new_nodes_back }
    in

    for idx = 0 to !num_elements - 1 do
      batch_create_nodes idx create_node_args
    done;

    if !maxinsertlevel > !slist.level then !slist.level <- !maxinsertlevel;
    let prev_nodes = Array.make ((!maxinsertlevel + 1) * !num_elements) (-1) in
    let related_node_args =
      {
        new_nodes;
        level;
        size = num_elements;
        prev_nodes;
        maxinsertlevel;
        new_nodes_back;
      }
    in

    T.parallel_for ~start:0 ~finish:(!num_elements - 1)
      ~body:(fun idx -> batch_relate_nodes idx related_node_args)
      pool;

    let merge_list_args = related_node_args in
    T.parallel_for ~start:0 ~finish:(!num_elements - 1)
      ~body:(fun idx -> batch_merge_lists idx merge_list_args)
      pool;

    let back_node = ref Null in
    for i = 0 to !num_elements - 1 do
      for j = 0 to level.(i) do
        if prev_nodes.(((!maxinsertlevel + 1) * i) + j) = -2 then (
          back_node := !^(new_nodes_back.(i)).forward.(j);
          !^(!back_node).forward.(j) <- new_nodes.(i))
      done
    done

    let bop (ls : (op * res Promises.promise) list) =
      let n = List.length ls in
      let data = Array.make n 0 in
      List.iteri (fun i v -> data.(i) <- v) ls;
      batch_insert_par data n
      T.parallel_for ~start 
      
end
