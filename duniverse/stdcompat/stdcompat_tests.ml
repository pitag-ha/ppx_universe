let test_array_for_all2 () =
  let visited = ref [] in
  let p i s =
    visited := i :: !visited;
    String.make 1 (char_of_int ((int_of_char 'a') + i)) = s in
  assert (
    Stdcompat.Array.for_all2 p [| 0; 1; 2; 3 |] [| "a"; "b"; "c"; "d" |]);
  assert (List.rev !visited = [0; 1; 2; 3]);
  visited := [];
  let p i s =
    visited := i :: !visited;
    String.make 1 (char_of_int ((int_of_char 'a') + i)) = s && i <> 2 in
  assert (not (
    Stdcompat.Array.for_all2 p [| 0; 1; 2; 3 |] [| "a"; "b"; "c"; "d" |]));
  assert (List.rev !visited = [0; 1; 2]);
  visited := [];
  assert (
    try
      ignore (
        Stdcompat.Array.for_all2 p [| 0; 1; 2; 3 |] [| "a"; "b"; "c" |]);
      false
    with Invalid_argument _ -> true);
  assert (List.rev !visited = [])

let test_array_exists2 () =
  let visited = ref [] in
  let p i s =
    visited := i :: !visited;
    String.make 1 (char_of_int ((int_of_char 'a') + i)) <> s in
  assert (not (
    Stdcompat.Array.exists2 p [| 0; 1; 2; 3 |] [| "a"; "b"; "c"; "d" |]));
  assert (List.rev !visited = [0; 1; 2; 3]);
  visited := [];
  let p i s =
    visited := i :: !visited;
    String.make 1 (char_of_int ((int_of_char 'a') + i)) <> s || i = 2 in
  assert (
    Stdcompat.Array.exists2 p [| 0; 1; 2; 3 |] [| "a"; "b"; "c"; "d" |]);
  assert (List.rev !visited = [0; 1; 2]);
  visited := [];
  assert (
    try
      ignore (
        Stdcompat.Array.exists2 p [| 0; 1; 2; 3 |] [| "a"; "b"; "c" |]);
      false
    with Invalid_argument _ -> true);
  assert (List.rev !visited = [])

let test_list_filteri () =
  let visited = ref [] in
  let p i s =
    visited := i :: !visited;
    assert (string_of_int i = s);
    i mod 2 = 0 in
  assert (
    Stdcompat.List.filteri p (Stdcompat.List.init 6 string_of_int) =
      ["0"; "2"; "4"]);
  assert (List.rev !visited = [0; 1; 2; 3; 4; 5])

let test_list_fold_left_map () =
  let visited = ref [] in
  let f accu s =
    visited := s :: !visited;
    accu ^ s, "a" ^ s in
  assert (
    Stdcompat.List.fold_left_map f "" (Stdcompat.List.init 6 string_of_int) =
      ("012345", ["a0"; "a1"; "a2"; "a3"; "a4"; "a5"]));
  assert (List.rev !visited = ["0"; "1"; "2"; "3"; "4"; "5"])

let test_seq_cons () =
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.cons 0 (Stdcompat.Seq.cons 1
      (Stdcompat.List.to_seq [2; 3]))) = [0; 1; 2; 3])

let test_seq_append () =
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.append
      (Stdcompat.List.to_seq [0; 1; 2])
      (Stdcompat.List.to_seq [3; 4; 5])) = [0; 1; 2; 3; 4; 5])

let test_seq_unfold () =
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.unfold (fun i ->
      if i < 5 then
        Some (string_of_int i, i + 1)
      else
        None) 0) = ["0"; "1"; "2"; "3"; "4"])

let test_set_filter_map () =
  let module Set = Stdcompat.Set.Make (Stdcompat.Int) in
  let visited = ref [] in
  assert (
    List.sort compare (Set.elements (Set.filter_map (fun i ->
      visited := i :: !visited;
      if i mod 2 = 0 then
        Some (6 - i / 2)
      else
        None)
      (Set.of_seq (Stdcompat.List.to_seq
        (Stdcompat.List.init 6 Stdcompat.Fun.id))))) = [4; 5; 6]);
  assert (List.rev !visited = [0; 1; 2; 3; 4; 5])

let test_map_filter_map () =
  let module Map = Stdcompat.Map.Make (Stdcompat.Int) in
  let visited = ref [] in
  assert (
    List.sort compare (Map.bindings (Map.filter_map (fun k s ->
      visited := k :: !visited;
      assert (string_of_int k = s);
      if k mod 2 = 0 then
        Some ("a" ^ s)
      else
        None)
      (Map.of_seq (Stdcompat.List.to_seq
        (Stdcompat.List.init 6 (fun i -> i, string_of_int i)))))) =
          [0, "a0"; 2, "a2"; 4, "a4"]);
  assert (List.rev !visited = [0; 1; 2; 3; 4; 5])

let test_mkdir_rmdir () =
  Stdcompat.Sys.mkdir "new_directory" 0o600;
  assert (Stdcompat.Sys.is_directory "new_directory");
  begin try
    Stdcompat.Sys.mkdir "new_directory" 0o600;
    assert false;
  with Sys_error _ ->
    assert true;
  end;
  Stdcompat.Sys.rmdir "new_directory";
  begin try
    Stdcompat.Sys.rmdir "new_directory";
    assert false;
  with Sys_error _ ->
    assert true;
  end

let test_set_to_seq_to_rev_seq () =
  let module M = Stdcompat.Set.Make (Stdcompat.Int) in
  let s = M.add 2 (M.add 1 (M.add 3 M.empty)) in
  assert (Stdcompat.List.of_seq (M.to_seq s) = [1; 2; 3]);
  assert (Stdcompat.List.of_seq (M.to_rev_seq s) = [3; 2; 1])

let test_map_to_seq_to_rev_seq () =
  let module M = Stdcompat.Map.Make (Stdcompat.Int) in
  let s = M.add 2 () (M.add 1 () (M.add 3 () M.empty)) in
  assert (Stdcompat.List.of_seq (M.to_seq s) = [1, (); 2, (); 3, ()]);
  assert (Stdcompat.List.of_seq (M.to_rev_seq s) = [3, (); 2, (); 1, ()])

let test_list_partition_map () =
  assert (Stdcompat.List.partition_map (fun i ->
    if i > 0 then
      Stdcompat.Either.Left i
    else
      Stdcompat.Either.Right (-i)) [1; -2; 3; -4; 5; -6]
    = ([1; 3; 5], [2; 4; 6]))

let test_list_compare () =
  let l1 = [1; 2; 3; 4] in
  let l2 = [1; 2; 4; 3] in
  let l3 = [1; 2; 5; 6] in
  assert (Stdcompat.List.compare Stdcompat.Int.compare l1 l1 = 0);
  assert (Stdcompat.List.compare Stdcompat.Int.compare l1 l2 < 0);
  assert (Stdcompat.List.compare Stdcompat.Int.compare l1 l3 < 0);
  assert (Stdcompat.List.compare Stdcompat.Int.compare l2 l1 > 0);
  assert (Stdcompat.List.compare Stdcompat.Int.compare l2 l2 = 0);
  assert (Stdcompat.List.compare Stdcompat.Int.compare l2 l3 < 0);
  assert (Stdcompat.List.compare Stdcompat.Int.compare l3 l1 > 0);
  assert (Stdcompat.List.compare Stdcompat.Int.compare l3 l2 > 0);
  assert (Stdcompat.List.compare Stdcompat.Int.compare l3 l3 = 0)

let test_list_equal () =
  let l1 = [1; 2; 3; 4] in
  let l2 = [1; 2; 4; 3] in
  let l3 = [1; 2; 5; 6] in
  assert (Stdcompat.List.equal Stdcompat.Int.equal l1 l1);
  assert (not (Stdcompat.List.equal Stdcompat.Int.equal l1 l2));
  assert (not (Stdcompat.List.equal Stdcompat.Int.equal l1 l3));
  assert (not (Stdcompat.List.equal Stdcompat.Int.equal l2 l1));
  assert (Stdcompat.List.equal Stdcompat.Int.equal l2 l2);
  assert (not (Stdcompat.List.equal Stdcompat.Int.equal l2 l3));
  assert (not (Stdcompat.List.equal Stdcompat.Int.equal l3 l1));
  assert (not (Stdcompat.List.equal Stdcompat.Int.equal l3 l2));
  assert (Stdcompat.List.equal Stdcompat.Int.equal l3 l3)

let test_hashtbl_rebuild () =
  let h = Hashtbl.create 17 in
  Hashtbl.add h 1 ();
  Hashtbl.add h 2 ();
  Hashtbl.add h 3 ();
  let h' = Stdcompat.Hashtbl.rebuild h in
  let module M = Stdcompat.Set.Make (Stdcompat.Int) in
  assert (
    M.equal
      (M.of_seq (Stdcompat.Seq.map fst (Stdcompat.Hashtbl.to_seq h')))
      (M.add 1 (M.add 2
         (M.add 3 M.empty))))

let test_format_pp_print_seq () =
  let buffer = Buffer.create 17 in
  Format.fprintf (Format.formatter_of_buffer buffer) "%a@."
    (Stdcompat.Format.pp_print_seq
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",")
      Format.pp_print_int) (Stdcompat.List.to_seq [1; 2; 3]);
  assert (Buffer.contents buffer = "1,2,3\n")

let test_either () =
  assert (Stdcompat.Either.compare
    ~left:Stdcompat.Int.compare ~right:Stdcompat.Int.compare
    (Stdcompat.Either.Left 1) (Stdcompat.Either.Left 1) = 0);
  assert (Stdcompat.Either.compare
    ~left:Stdcompat.Int.compare ~right:Stdcompat.Int.compare
    (Stdcompat.Either.Left 1) (Stdcompat.Either.Left 2) < 0);
  assert (Stdcompat.Either.compare
    ~left:Stdcompat.Int.compare ~right:Stdcompat.Int.compare
    (Stdcompat.Either.Left 1) (Stdcompat.Either.Right 2) < 0);
  assert (Stdcompat.Either.compare
    ~left:Stdcompat.Int.compare ~right:Stdcompat.Int.compare
    (Stdcompat.Either.Right 1) (Stdcompat.Either.Left 2) > 0);
  assert (Stdcompat.Either.compare
    ~left:Stdcompat.Int.compare ~right:Stdcompat.Int.compare
    (Stdcompat.Either.Right 1) (Stdcompat.Either.Right 2) < 0)

let test_seq_concat () =
  assert (Stdcompat.List.of_seq
    (Stdcompat.Seq.concat (Stdcompat.Seq.map Stdcompat.List.to_seq
      (Stdcompat.List.to_seq [[1; 2]; []; [3]; [4; 5]]))) =
    [1; 2; 3; 4; 5]);
  assert (Stdcompat.List.of_seq
    (Stdcompat.Seq.concat_map Stdcompat.List.to_seq
      (Stdcompat.List.to_seq [[1; 2]; []; [3]; [4; 5]])) =
    [1; 2; 3; 4; 5])

let test_seq_is_empty () =
  assert (Stdcompat.Seq.is_empty Stdcompat.Seq.empty);
  assert (not (
    Stdcompat.Seq.is_empty (
      Stdcompat.Seq.once (Stdcompat.Seq.cons () Stdcompat.Seq.empty))))

let test_seq_uncons () =
  assert (Stdcompat.Seq.uncons Stdcompat.Seq.empty = None);
  begin
    match Stdcompat.Seq.uncons
      (Stdcompat.Seq.once (Stdcompat.Seq.cons () Stdcompat.Seq.empty)) with
    | None -> assert false
    | Some ((), tl) -> assert (Stdcompat.Seq.uncons tl = None)
  end

let test_seq_length () =
  let s = Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id) in
  assert (Stdcompat.Seq.length s = 6)

let test_seq_iteri () =
  let accu = ref [] in
  let add i j = accu := (i, j) :: !accu in
  Stdcompat.Seq.iteri add (Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id));
  assert (List.rev !accu = Stdcompat.List.init 6 (fun i -> (i, i)))

let test_seq_fold_lefti () =
  let add accu i j = (i, j) :: accu in
  let accu = Stdcompat.Seq.fold_lefti add []
    (Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id)) in
  assert (List.rev accu = Stdcompat.List.init 6 (fun i -> (i, i)))

let test_seq_forall () =
  assert (Stdcompat.Seq.for_all (fun _ -> raise Exit) Stdcompat.Seq.empty);
  let accu = ref [] in
  let add i = accu := i :: !accu in
  assert (Stdcompat.Seq.for_all (fun i -> add i; true)
    (Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id)));
  assert (List.rev !accu = Stdcompat.List.init 6 Stdcompat.Fun.id);
  accu := [];
  assert (not (Stdcompat.Seq.for_all (fun i -> add i; i <> 4)
    (Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id))));
  assert (List.rev !accu = Stdcompat.List.init 5 Stdcompat.Fun.id)

let test_seq_exists () =
  assert (not (Stdcompat.Seq.exists (fun _ -> raise Exit) Stdcompat.Seq.empty));
  let accu = ref [] in
  let add i = accu := i :: !accu in
  assert (not (Stdcompat.Seq.exists (fun i -> add i; false)
    (Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id))));
  assert (List.rev !accu = Stdcompat.List.init 6 Stdcompat.Fun.id);
  accu := [];
  assert (Stdcompat.Seq.exists (fun i -> add i; i = 4)
    (Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id)));
  assert (List.rev !accu = Stdcompat.List.init 5 Stdcompat.Fun.id)

let test_seq_find () =
  assert (Stdcompat.Seq.find (fun _ -> raise Exit) Stdcompat.Seq.empty = None);
  let accu = ref [] in
  let add i = accu := i :: !accu in
  assert (Stdcompat.Seq.find (fun i -> add i; false)
    (Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id)) = None);
  assert (List.rev !accu = Stdcompat.List.init 6 Stdcompat.Fun.id);
  accu := [];
  assert (Stdcompat.Seq.find (fun i -> add i; i = 4)
    (Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id)) = Some 4);
  assert (List.rev !accu = Stdcompat.List.init 5 Stdcompat.Fun.id)

let test_seq_find_map () =
  assert (
    Stdcompat.Seq.find_map (fun _ -> raise Exit) Stdcompat.Seq.empty = None);
  let accu = ref [] in
  let add i = accu := i :: !accu in
  assert (Stdcompat.Seq.find_map (fun i -> add i; None)
    (Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id)) = None);
  assert (List.rev !accu = Stdcompat.List.init 6 Stdcompat.Fun.id);
  accu := [];
  assert (Stdcompat.Seq.find_map (fun i ->
    add i; if i = 4 then Some i else None)
    (Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id)) = Some 4);
  assert (List.rev !accu = Stdcompat.List.init 5 Stdcompat.Fun.id)

let test_seq_iter2 () =
  let test_with_lengths i j =
    let accu = ref [] in
    let add i j = accu := (i, j) :: !accu in
    Stdcompat.Seq.iter2 add
      (Stdcompat.Seq.once (Stdcompat.Seq.init i Stdcompat.Fun.id))
      (Stdcompat.Seq.once (Stdcompat.Seq.init j Stdcompat.Fun.id));
    assert (List.rev !accu = Stdcompat.List.init (min i j) (fun i -> (i, i))) in
  test_with_lengths 6 6;
  test_with_lengths 6 7;
  test_with_lengths 7 6

let test_seq_fold_left2 () =
  let test_with_lengths i j =
    let add accu i j = (i, j) :: accu in
    let accu = Stdcompat.Seq.fold_left2 add []
      (Stdcompat.Seq.once (Stdcompat.Seq.init i Stdcompat.Fun.id))
      (Stdcompat.Seq.once (Stdcompat.Seq.init j Stdcompat.Fun.id)) in
    assert (List.rev accu = Stdcompat.List.init (min i j) (fun i -> (i, i))) in
  test_with_lengths 6 6;
  test_with_lengths 6 7;
  test_with_lengths 7 6

let test_seq_for_all2 () =
  let test_with_lengths i j p =
    Stdcompat.Seq.for_all2 p
      (Stdcompat.Seq.once (Stdcompat.Seq.init i Stdcompat.Fun.id))
      (Stdcompat.Seq.once (Stdcompat.Seq.init j Stdcompat.Fun.id)) in
  assert (test_with_lengths 0 0 (fun _ _ -> raise Exit));
  assert (test_with_lengths 0 6 (fun _ _ -> raise Exit));
  assert (test_with_lengths 6 0 (fun _ _ -> raise Exit));
  let accu = ref [] in
  let add i j = accu := (i, j) :: !accu in
  assert (test_with_lengths 6 6 (fun i j -> add i j; true));
  assert (List.rev !accu = Stdcompat.List.init 6 (fun i -> (i, i)));
  accu := [];
  assert (test_with_lengths 6 7 (fun i j -> add i j; true));
  assert (List.rev !accu = Stdcompat.List.init 6 (fun i -> (i, i)));
  accu := [];
  assert (test_with_lengths 7 6 (fun i j -> add i j; true));
  assert (List.rev !accu = Stdcompat.List.init 6 (fun i -> (i, i)));
  accu := [];
  assert (not (test_with_lengths 6 6 (fun i j -> add i j; i < 4)));
  assert (List.rev !accu = Stdcompat.List.init 5 (fun i -> (i, i)))

let test_seq_exists2 () =
  let test_with_lengths i j p =
    Stdcompat.Seq.exists2 p
      (Stdcompat.Seq.once (Stdcompat.Seq.init i Stdcompat.Fun.id))
      (Stdcompat.Seq.once (Stdcompat.Seq.init j Stdcompat.Fun.id)) in
  assert (not (test_with_lengths 0 0 (fun _ _ -> raise Exit)));
  assert (not (test_with_lengths 0 6 (fun _ _ -> raise Exit)));
  assert (not (test_with_lengths 6 0 (fun _ _ -> raise Exit)));
  let accu = ref [] in
  let add i j = accu := (i, j) :: !accu in
  assert (not (test_with_lengths 6 6 (fun i j -> add i j; false)));
  assert (List.rev !accu = Stdcompat.List.init 6 (fun i -> (i, i)));
  accu := [];
  assert (not (test_with_lengths 6 7 (fun i j -> add i j; false)));
  assert (List.rev !accu = Stdcompat.List.init 6 (fun i -> (i, i)));
  accu := [];
  assert (not (test_with_lengths 7 6 (fun i j -> add i j; false)));
  assert (List.rev !accu = Stdcompat.List.init 6 (fun i -> (i, i)));
  accu := [];
  assert (test_with_lengths 6 6 (fun i j -> add i j; i = 4));
  assert (List.rev !accu = Stdcompat.List.init 5 (fun i -> (i, i)))

let test_seq_equal () =
  let test_with_lengths i j p =
    Stdcompat.Seq.equal p
      (Stdcompat.Seq.once (Stdcompat.Seq.init i Stdcompat.Fun.id))
      (Stdcompat.Seq.once (Stdcompat.Seq.init j Stdcompat.Fun.id)) in
  assert (test_with_lengths 0 0 (fun _ _ -> raise Exit));
  assert (not (test_with_lengths 0 6 (fun _ _ -> raise Exit)));
  assert (not (test_with_lengths 6 0 (fun _ _ -> raise Exit)));
  let accu = ref [] in
  let add i j = accu := (i, j) :: !accu in
  assert (test_with_lengths 6 6 (fun i j -> add i j; true));
  assert (List.rev !accu = Stdcompat.List.init 6 (fun i -> (i, i)));
  accu := [];
  assert (not (test_with_lengths 6 7 (fun i j -> add i j; true)));
  assert (List.rev !accu = Stdcompat.List.init 6 (fun i -> (i, i)));
  accu := [];
  assert (not (test_with_lengths 7 6 (fun i j -> add i j; true)));
  assert (List.rev !accu = Stdcompat.List.init 6 (fun i -> (i, i)));
  accu := [];
  assert (not (test_with_lengths 6 6 (fun i j -> add i j; i < 4)));
  assert (List.rev !accu = Stdcompat.List.init 5 (fun i -> (i, i)))

let test_seq_compare () =
  let test_with_lengths i j p =
    Stdcompat.Seq.compare p
      (Stdcompat.Seq.once (Stdcompat.Seq.init i Stdcompat.Fun.id))
      (Stdcompat.Seq.once (Stdcompat.Seq.init j Stdcompat.Fun.id)) in
  assert (test_with_lengths 0 0 (fun _ _ -> raise Exit) = 0);
  assert (test_with_lengths 0 6 (fun _ _ -> raise Exit) = -1);
  assert (test_with_lengths 6 0 (fun _ _ -> raise Exit) = 1);
  let accu = ref [] in
  let add i j = accu := (i, j) :: !accu in
  assert (test_with_lengths 6 6 (fun i j -> add i j; 0) = 0);
  assert (List.rev !accu = Stdcompat.List.init 6 (fun i -> (i, i)));
  accu := [];
  assert (test_with_lengths 6 7 (fun i j -> add i j; 0) = -1);
  assert (List.rev !accu = Stdcompat.List.init 6 (fun i -> (i, i)));
  accu := [];
  assert (test_with_lengths 7 6 (fun i j -> add i j; 0) = 1);
  assert (List.rev !accu = Stdcompat.List.init 6 (fun i -> (i, i)));
  accu := [];
  assert (test_with_lengths 6 6
    (fun i j -> add i j; if i = 4 then 1 else 0) = 1);
  assert (List.rev !accu = Stdcompat.List.init 5 (fun i -> (i, i)));
  accu := [];
  assert (test_with_lengths 6 6
    (fun i j -> add i j; if i = 4 then -1 else 0) = -1);
  assert (List.rev !accu = Stdcompat.List.init 5 (fun i -> (i, i)))

let test_seq_init () =
  begin try
    let _ = Stdcompat.Seq.init (-1) (fun _ -> raise Exit) () in
    assert false
  with Invalid_argument _ ->
    ()
  end;
  let accu = ref [] in
  let s = Stdcompat.Seq.init 6 (fun i -> accu := i :: !accu; i) in
  assert (!accu = []);
  assert (Stdcompat.List.of_seq s = Stdcompat.List.init 6 Stdcompat.Fun.id);
  assert (List.rev !accu = Stdcompat.List.init 6 Stdcompat.Fun.id)

let test_seq_repeat () =
  let s = Stdcompat.Seq.repeat () in
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.take 6 s) =
      Stdcompat.List.init 6 (fun _ -> ()))

let test_seq_forever () =
  let value_ref = ref None in
  let s = Stdcompat.Seq.forever
    (fun () ->
      let value = Stdcompat.Option.get !value_ref in
      value_ref := None; value) in
  value_ref := Some 1;
  let hd, s' = Stdcompat.Option.get (Stdcompat.Seq.uncons s) in
  assert (hd = 1);
  value_ref := Some 2;
  let hd, _s'' = Stdcompat.Option.get (Stdcompat.Seq.uncons s') in
  assert (hd = 2);
  value_ref := Some 3;
  let hd, _s''' = Stdcompat.Option.get (Stdcompat.Seq.uncons s) in
  assert (hd = 3)

let test_seq_cycle () =
  let s = Stdcompat.Seq.cycle (Stdcompat.Seq.init 3 Stdcompat.Fun.id) in
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.take 4 s) = [0; 1; 2; 0]);
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.take 5 s) = [0; 1; 2; 0; 1]);
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.take 6 s) = [0; 1; 2; 0; 1; 2])

let test_seq_iterate () =
  let called = ref 0 in
  let s = Stdcompat.Seq.iterate (fun x -> incr called; x + 1) 0 in
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.take 4 s) = [0; 1; 2; 3]);
  called := 3;
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.take 5 s) = [0; 1; 2; 3; 4]);
  called := 7;
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.take 6 s) = [0; 1; 2; 3; 4; 5]);
  called := 13

let test_seq_mapi () =
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.mapi (fun i j -> (i, j))
      (Stdcompat.Seq.init 6 Stdcompat.Fun.id)) =
      Stdcompat.List.init 6 (fun i -> (i, i)))

let test_seq_scan () =
  assert (
    Stdcompat.List.of_seq (
      Stdcompat.Seq.scan ( + ) 0
        (Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id))) =
    [0; 0; 1; 3; 6; 10; 15])

let test_seq_take () =
  begin try
    let _ = Stdcompat.Seq.take (-1) (fun _ -> raise Exit) () in
    assert false
  with Invalid_argument _ ->
    ()
  end;
  assert (Stdcompat.Seq.is_empty (Stdcompat.Seq.take 0
    (fun _ -> raise Exit)));
  let s = Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id) in
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.take 4 s) = [0; 1; 2; 3]);
  let s = Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id) in
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.take 6 s) = [0; 1; 2; 3; 4; 5])

let test_seq_drop () =
  begin try
    let _ = Stdcompat.Seq.drop (-1) (fun _ -> raise Exit) () in
    assert false
  with Invalid_argument _ ->
    ()
  end;
  let _f = Stdcompat.Seq.drop 0 (fun _ -> raise Exit) in
  let s = Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id) in
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.drop 4 s) = [4; 5]);
  let s = Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id) in
  assert (Stdcompat.Seq.is_empty (Stdcompat.Seq.drop 6 s))

let test_seq_take_while () =
  let s = Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id) in
  assert (Stdcompat.List.of_seq
    (Stdcompat.Seq.take_while (fun _i -> false) s) = []);
  let s = Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id) in
  assert (Stdcompat.List.of_seq
    (Stdcompat.Seq.take_while (fun i -> i < 3) s) = [0; 1; 2]);
  let s = Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id) in
  assert (Stdcompat.List.of_seq
    (Stdcompat.Seq.take_while (fun _i -> true) s) = [0; 1; 2; 3; 4; 5])

let test_seq_drop_while () =
  let s = Stdcompat.Seq.init 6 Stdcompat.Fun.id in
  assert (Stdcompat.List.of_seq
    (Stdcompat.Seq.drop_while (fun i -> i < 3) s) = [3; 4; 5]);
  let s = Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id) in
  assert (Stdcompat.Seq.is_empty
    (Stdcompat.Seq.drop_while (fun _i -> true) s))

let test_seq_group () =
  let s =
    Stdcompat.Seq.group (fun i j -> i / 3 = j / 3)
    (Stdcompat.Seq.ints 0) in
  let l012, tl = Stdcompat.Option.get (Stdcompat.Seq.uncons s) in
  let l345, _tl = Stdcompat.Option.get (Stdcompat.Seq.uncons tl) in
  assert (Stdcompat.List.of_seq l345 = [3; 4; 5]);
  assert (Stdcompat.List.of_seq l012 = [0; 1; 2]);
  let s =
    Stdcompat.Seq.group (fun _i _j -> true)
    (Stdcompat.Seq.ints 0) in
  assert (Stdcompat.List.of_seq (Stdcompat.Seq.take 3 (fst (Stdcompat.Option.get
    (Stdcompat.Seq.uncons s)))) = [0; 1; 2])

let test_seq_memoize () =
  let s = Stdcompat.Seq.memoize (Stdcompat.Seq.once (Stdcompat.Seq.ints 0)) in
  assert (Stdcompat.List.of_seq (Stdcompat.Seq.take 3 s) = [0; 1; 2]);
  assert (Stdcompat.List.of_seq (Stdcompat.Seq.take 4 s) = [0; 1; 2; 3])

let test_seq_once () =
  let s = Stdcompat.Seq.once (Stdcompat.Seq.ints 0) in
  let hd, tl = Stdcompat.Option.get (Stdcompat.Seq.uncons s) in
  assert (hd = 0);
  begin try
    ignore (Stdcompat.Seq.uncons s);
    assert false
  with Stdcompat.Seq.Forced_twice ->
    ()
  end;
  let hd', tl' = Stdcompat.Option.get (Stdcompat.Seq.uncons tl) in
  assert (hd' = 1);
  begin try
    ignore (Stdcompat.Seq.uncons tl);
    assert false
  with Stdcompat.Seq.Forced_twice ->
    ()
  end;
  let hd'', _tl'' = Stdcompat.Option.get (Stdcompat.Seq.uncons tl') in
  assert (hd'' = 2)

let test_seq_transpose () =
  let test_format m n =
    let s =
      Stdcompat.Seq.init m (fun i ->
        Stdcompat.Seq.init n (fun j ->
          i * n + j)) in
    let t =
      Stdcompat.Seq.init n (fun j ->
        Stdcompat.Seq.init m (fun i ->
          i * n + j)) in
    assert (Stdcompat.Seq.equal (Stdcompat.Seq.equal Stdcompat.Int.equal)
      (Stdcompat.Seq.transpose s) t) in
  test_format 3 3;
  test_format 3 4;
  test_format 4 3;
  let s =
    Stdcompat.Seq.iterate (fun i ->
      Stdcompat.Seq.map (( * ) 2) i) (Stdcompat.Seq.ints 0) in
  let t = Stdcompat.Seq.transpose s in
  assert (Stdcompat.Seq.equal (Stdcompat.Seq.equal Stdcompat.Int.equal)
    (Stdcompat.Seq.map (Stdcompat.Seq.take 3) (Stdcompat.Seq.take 3 t))
    (Stdcompat.Seq.init 3 (fun i ->
        Stdcompat.Seq.init 3 (fun j ->
          i * (1 lsl j)))));
  let s =
    Stdcompat.Seq.map Stdcompat.List.to_seq (Stdcompat.List.to_seq
      [[0; 1]; [2; 3; 4; 5]; [6; 7; 8]]) in
  let t = Stdcompat.Seq.transpose s in
  assert (Stdcompat.List.of_seq (Stdcompat.Seq.map Stdcompat.List.of_seq t) =
    [[0; 2; 6]; [1; 3; 7]; [4; 8]; [5]])

let test_seq_zip () =
  assert (Stdcompat.Seq.is_empty
    (Stdcompat.Seq.zip Stdcompat.Seq.empty (fun _ -> raise Exit)));
  assert (Stdcompat.Seq.is_empty
    (Stdcompat.Seq.zip (Stdcompat.Seq.cons () (fun _ -> raise Exit))
      Stdcompat.Seq.empty));
  assert (
    Stdcompat.List.of_seq
      (Stdcompat.Seq.zip (Stdcompat.Seq.init 3 Stdcompat.Fun.id)
         (Stdcompat.Seq.init 4 (fun i -> assert (i < 4); i))) =
    Stdcompat.List.init 3 (fun i -> (i, i)));
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.take 3
      (Stdcompat.Seq.zip (Stdcompat.Seq.ints 0) (Stdcompat.Seq.ints 0))) =
    Stdcompat.List.init 3 (fun i -> (i, i)))

let test_seq_map2 () =
  let pair i j = (i, j) in
  assert (Stdcompat.Seq.is_empty
    (Stdcompat.Seq.map2 pair Stdcompat.Seq.empty (fun _ -> raise Exit)));
  assert (Stdcompat.Seq.is_empty
    (Stdcompat.Seq.map2 pair (Stdcompat.Seq.cons () (fun _ -> raise Exit))
      Stdcompat.Seq.empty));
  assert (
    Stdcompat.List.of_seq
      (Stdcompat.Seq.map2 pair (Stdcompat.Seq.init 3 Stdcompat.Fun.id)
         (Stdcompat.Seq.init 4 (fun i -> assert (i < 4); i))) =
    Stdcompat.List.init 3 (fun i -> (i, i)));
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.take 3
      (Stdcompat.Seq.map2 pair (Stdcompat.Seq.ints 0) (Stdcompat.Seq.ints 0))) =
    Stdcompat.List.init 3 (fun i -> (i, i)))

let test_seq_interleave () =
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.interleave Stdcompat.Seq.empty
      (Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id))) =
    [0; 1; 2; 3; 4; 5]);
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.interleave
      (Stdcompat.Seq.once (Stdcompat.Seq.init 6 Stdcompat.Fun.id))
      Stdcompat.Seq.empty) =
    [0; 1; 2; 3; 4; 5]);
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.interleave
      (Stdcompat.Seq.once (Stdcompat.Seq.init 3 (fun i -> i * 2)))
      (Stdcompat.Seq.once (Stdcompat.Seq.init 3 (fun i -> i * 2 + 1)))) =
    [0; 1; 2; 3; 4; 5]);
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.interleave
      (Stdcompat.Seq.once (Stdcompat.Seq.init 5 (fun i -> i * 2)))
      (Stdcompat.Seq.once (Stdcompat.Seq.init 3 (fun i -> i * 2 + 1)))) =
    [0; 1; 2; 3; 4; 5; 6; 8]);
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.interleave
      (Stdcompat.Seq.once (Stdcompat.Seq.init 3 (fun i -> i * 2)))
      (Stdcompat.Seq.once (Stdcompat.Seq.init 5 (fun i -> i * 2 + 1)))) =
    [0; 1; 2; 3; 4; 5; 7; 9])

let test_seq_sorted_merge () =
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.sorted_merge
      (fun (i, _) (j, _) -> Stdcompat.Int.compare i j)
      (Stdcompat.Seq.once
        (Stdcompat.List.to_seq [1, false; 4, false; 6, false; 7, false]))
      (Stdcompat.Seq.once
        (Stdcompat.List.to_seq [1, true; 2, true; 4, true; 5, true]))) =
    [1, false; 1, true; 2, true; 4, false; 4, true; 5, true; 6, false;
      7, false]);
  assert (
    Stdcompat.List.of_seq (Stdcompat.Seq.take 8 (
     Stdcompat.Seq.sorted_merge Stdcompat.Int.compare
       (Stdcompat.Seq.once
         (Stdcompat.Seq.map (fun i -> assert (i < 4); i * 3)
           (Stdcompat.Seq.ints 0)))
       (Stdcompat.Seq.once
         (Stdcompat.Seq.map (fun i -> assert (i < 5); 1 lsl i)
           (Stdcompat.Seq.ints 0))))) =
    [0; 1; 2; 3; 4; 6; 8; 9])

let test_seq_product () =
  assert (List.sort compare (Stdcompat.List.of_seq
      (Stdcompat.Seq.product
        (Stdcompat.Seq.init 3 Stdcompat.Fun.id) (Stdcompat.Seq.init 3 Stdcompat.Fun.id))) =
    (Stdcompat.List.init 9 (fun i -> (i / 3, i mod 3))));
  assert (
    List.length (Stdcompat.List.sort_uniq compare
    (Stdcompat.List.of_seq (Stdcompat.Seq.take 8
      (Stdcompat.Seq.product
        (Stdcompat.Seq.ints 0) (Stdcompat.Seq.ints 0))))) = 8)

let test_seq_map_product () =
  let pair i j = (i, j) in
  assert (List.sort compare (
    Stdcompat.List.of_seq
      (Stdcompat.Seq.map_product pair
        (Stdcompat.Seq.init 3 Stdcompat.Fun.id) (Stdcompat.Seq.init 3 Stdcompat.Fun.id))) =
    (Stdcompat.List.init 9 (fun i -> (i / 3, i mod 3))));
  assert (
    List.length (Stdcompat.List.sort_uniq compare
    (Stdcompat.List.of_seq (Stdcompat.Seq.take 8
      (Stdcompat.Seq.map_product pair
        (Stdcompat.Seq.ints 0) (Stdcompat.Seq.ints 0))))) = 8)

let test_seq_unzip () =
  let s, s' =
    Stdcompat.Seq.unzip (Stdcompat.Seq.map (fun i -> (i, i))
      (Stdcompat.Seq.take 6 (Stdcompat.Seq.ints 0))) in
  assert (Stdcompat.List.of_seq s = Stdcompat.List.init 6 Stdcompat.Fun.id);
  assert (Stdcompat.List.of_seq s' = Stdcompat.List.init 6 Stdcompat.Fun.id);
  let s, s' =
    Stdcompat.Seq.unzip (Stdcompat.Seq.map (fun i -> assert (i < 6); (i, i))
      (Stdcompat.Seq.ints 0)) in
  assert (Stdcompat.List.of_seq (Stdcompat.Seq.take 6 s) =
    Stdcompat.List.init 6 Stdcompat.Fun.id);
  assert (Stdcompat.List.of_seq (Stdcompat.Seq.take 6 s') =
    Stdcompat.List.init 6 Stdcompat.Fun.id)

let test_seq_partition_map () =
  let s, s' =
    Stdcompat.Seq.partition_map
      (fun i ->
        if i mod 3 = 0 then
          Stdcompat.Either.Left i
        else
          Stdcompat.Either.Right i)
      (Stdcompat.Seq.take 6 (Stdcompat.Seq.ints 0)) in
  assert (Stdcompat.List.of_seq s = [0; 3]);
  assert (Stdcompat.List.of_seq s' = [1; 2; 4; 5]);
  let s, s' =
    Stdcompat.Seq.partition_map
      (fun i ->
        assert (i <= 15);
        if i mod 3 = 0 then
          Stdcompat.Either.Left i
        else
          Stdcompat.Either.Right i)
      (Stdcompat.Seq.ints 0) in
  assert (Stdcompat.List.of_seq (Stdcompat.Seq.take 6 s) =
    [0; 3; 6; 9; 12; 15]);
  assert (Stdcompat.List.of_seq (Stdcompat.Seq.take 6 s') =
    [1; 2; 4; 5; 7; 8])

let test_seq_partition () =
  let s, s' =
    Stdcompat.Seq.partition (fun i -> i mod 3 = 0)
      (Stdcompat.Seq.take 6 (Stdcompat.Seq.ints 0)) in
  assert (Stdcompat.List.of_seq s = [0; 3]);
  assert (Stdcompat.List.of_seq s' = [1; 2; 4; 5]);
  let s, s' =
    Stdcompat.Seq.partition
      (fun i -> assert (i <= 15); i mod 3 = 0)
      (Stdcompat.Seq.ints 0) in
  assert (Stdcompat.List.of_seq (Stdcompat.Seq.take 6 s) =
    [0; 3; 6; 9; 12; 15]);
  assert (Stdcompat.List.of_seq (Stdcompat.Seq.take 6 s') =
    [1; 2; 4; 5; 7; 8])

let test_seq_of_dispenser () =
  let counter = ref 0 in
  let s = Stdcompat.Seq.of_dispenser (fun () ->
    let index = !counter in
    if index >= 6 then
      None
    else
      begin
        counter := succ index;
        Some index
      end) in
  assert (Stdcompat.List.of_seq s = [0; 1; 2; 3; 4; 5]);
  counter := 0;
  let s = Stdcompat.Seq.of_dispenser (fun () ->
    let index = !counter in
    counter := succ index;
    Some index) in
  assert (Stdcompat.List.of_seq (Stdcompat.Seq.take 6 s) = [0; 1; 2; 3; 4; 5])

let test_seq_to_dispenser () =
  let d = Stdcompat.Seq.to_dispenser
      (Stdcompat.Seq.take 6 (Stdcompat.Seq.ints 0)) in
  assert (d () = Some 0);
  assert (d () = Some 1);
  assert (d () = Some 2);
  assert (d () = Some 3);
  assert (d () = Some 4);
  assert (d () = Some 5);
  assert (d () = None);
  assert (d () = None);
  let d = Stdcompat.Seq.to_dispenser (Stdcompat.Seq.ints 0) in
  assert (d () = Some 0);
  assert (d () = Some 1);
  assert (d () = Some 2);
  assert (d () = Some 3);
  assert (d () = Some 4);
  assert (d () = Some 5)

let test_seq_ints () =
  let s = Stdcompat.Seq.ints 4 in
  assert (Stdcompat.List.of_seq (Stdcompat.Seq.take 6 s) = [4; 5; 6; 7; 8; 9])

let test_seq_4_14 () =
  test_seq_is_empty ();
  test_seq_uncons ();
  test_seq_length ();
  test_seq_iteri ();
  test_seq_forall ();
  test_seq_exists ();
  test_seq_find ();
  test_seq_find_map ();
  test_seq_iter2 ();
  test_seq_fold_left2 ();
  test_seq_for_all2 ();
  test_seq_exists2 ();
  test_seq_equal ();
  test_seq_compare ();
  test_seq_init ();
  test_seq_repeat ();
  test_seq_forever ();
  test_seq_cycle ();
  test_seq_iterate ();
  test_seq_mapi ();
  test_seq_scan ();
  test_seq_take ();
  test_seq_drop ();
  test_seq_take_while ();
  test_seq_drop_while ();
  test_seq_group ();
  test_seq_memoize ();
  test_seq_once ();
  test_seq_transpose ();
  test_seq_zip ();
  test_seq_map2 ();
  test_seq_interleave ();
  test_seq_sorted_merge ();
  test_seq_product ();
  test_seq_map_product ();
  test_seq_unzip ();
  test_seq_partition_map ();
  test_seq_partition ();
  test_seq_of_dispenser ();
  test_seq_to_dispenser ();
  test_seq_ints ()

let test_int32_min_max () =
  assert (Stdcompat.Int32.min 1l 2l = 1l);
  assert (Stdcompat.Int32.max 1l 2l = 2l)

let test_int32_unsigned_compare () =
  assert (Stdcompat.Int32.unsigned_compare 1l 2l < 0);
  assert (Stdcompat.Int32.unsigned_compare (-1l) 2l > 0)

let test_array_fold_left_map () =
  let f counter item =
    succ counter, item + counter in
  assert (Stdcompat.Array.fold_left_map f 0 [| 1; 2; 3; 4; 5 |] =
    (5, [| 1; 3; 5; 7; 9 |]))

let test_array_find_opt () =
  assert (Stdcompat.Array.find_opt
    (fun i -> i mod 2 = 0) [| 1; 3; 4; 5 |] = Some 4);
  assert (Stdcompat.Array.find_opt
    (fun i -> i mod 2 = 0) [| 1; 3; 5 |] = None)

let test_array_find_map () =
  let f i =
    if i mod 2 = 0 then Some (i / 2) else None in
  assert (Stdcompat.Array.find_map f [| 1; 3; 4; 5 |] = Some 2);
  assert (Stdcompat.Array.find_map f [| 1; 3; 5 |] = None)

let test_array_split () =
  assert (Stdcompat.Array.split [| |] = ([| |], [| |]));
  assert (Stdcompat.Array.split [| (1, 2); (3, 4) |]
    = ([| 1; 3 |], [| 2; 4 |]))

let test_array_combine () =
  assert (Stdcompat.Array.combine [| |] [| |] = [| |]);
  assert (Stdcompat.Array.combine [| 1; 3 |] [| 2; 4 |]
    = [| (1, 2); (3, 4) |])

let test_string_fold_left () =
  let f i c =
    assert (int_of_char c - int_of_char '0' = i);
    succ i in
  assert (Stdcompat.String.fold_left f 0 "0123" = 4);
  assert (Stdcompat.String.fold_left f 0 "" = 0)

let test_string_fold_right () =
  let f c i =
    assert (int_of_char c - int_of_char '0' = pred i);
    pred i in
  assert (Stdcompat.String.fold_right f "0123" 4 = 0);
  assert (Stdcompat.String.fold_right f "" 0 = 0)

let test_string_for_all () =
  let f c =
    match c with
    | '0' .. '9' -> true
    | _ -> false in
  assert (Stdcompat.String.for_all f "0123" = true);
  assert (Stdcompat.String.for_all f "012a3" = false);
  assert (Stdcompat.String.for_all f "" = true)

let test_string_exists () =
  let f c =
    match c with
    | 'a' .. 'z' -> true
    | _ -> false in
  assert (Stdcompat.String.exists f "0123" = false);
  assert (Stdcompat.String.exists f "012a3" = true);
  assert (Stdcompat.String.exists f "" = false)

let test_string_starts_with () =
  assert (Stdcompat.String.starts_with ~prefix:"a" "abc" = true);
  assert (Stdcompat.String.starts_with ~prefix:"a" "bc" = false);
  assert (Stdcompat.String.starts_with ~prefix:"a" "a" = true);
  assert (Stdcompat.String.starts_with ~prefix:"a" "" = false);
  assert (Stdcompat.String.starts_with ~prefix:"ab" "abc" = true);
  assert (Stdcompat.String.starts_with ~prefix:"ab" "ab" = true);
  assert (Stdcompat.String.starts_with ~prefix:"ab" "bc" = false);
  assert (Stdcompat.String.starts_with ~prefix:"ab" "ac" = false);
  assert (Stdcompat.String.starts_with ~prefix:"ab" "a" = false);
  assert (Stdcompat.String.starts_with ~prefix:"ab" "" = false)

let test_string_ends_with () =
  assert (Stdcompat.String.ends_with ~suffix:"a" "cba" = true);
  assert (Stdcompat.String.ends_with ~suffix:"a" "cb" = false);
  assert (Stdcompat.String.ends_with ~suffix:"a" "a" = true);
  assert (Stdcompat.String.ends_with ~suffix:"a" "" = false);
  assert (Stdcompat.String.ends_with ~suffix:"ab" "cab" = true);
  assert (Stdcompat.String.ends_with ~suffix:"ab" "ab" = true);
  assert (Stdcompat.String.ends_with ~suffix:"ab" "bc" = false);
  assert (Stdcompat.String.ends_with ~suffix:"ab" "cb" = false);
  assert (Stdcompat.String.ends_with ~suffix:"ab" "b" = false);
  assert (Stdcompat.String.ends_with ~suffix:"ab" "" = false)

let test_channels () =
  Stdcompat.Out_channel.with_open_text "testfile" (fun oc ->
    Stdcompat.Out_channel.output_substring oc "hello, world" 7 5);
  assert (Stdcompat.In_channel.with_open_text "testfile"
    Stdcompat.In_channel.input_all = "world");
  Sys.remove "testfile"

let () =
  assert (Stdcompat.hypot 3. 4. = 5.);
  assert (Stdcompat.copysign 1. 2. = 1.);
  assert (Stdcompat.copysign 1. (-. 2.) = -. 1.);
  assert (Stdcompat.copysign (-. 1.) 2. = 1.);
  assert (Stdcompat.copysign (-. 1.) (-. 2.) = -. 1.);
  assert (
    try
      ignore (Stdcompat.raise_notrace Exit);
      false
    with Exit -> true);
  assert (Stdcompat.bool_of_string_opt "true" = Some true);
  assert (Stdcompat.bool_of_string_opt "false" = Some false);
  assert (Stdcompat.bool_of_string_opt "foo" = None);
  assert (Stdcompat.int_of_string_opt "42" = Some 42);
  assert (Stdcompat.int_of_string_opt "foo" = None);
  assert (Stdcompat.float_of_string_opt "42." = Some 42.);
  assert (Stdcompat.float_of_string_opt "foo" = None);
  assert (Lazy.force (Stdcompat.Lazy.from_fun (fun () -> 42)) = 42);
  assert (Lazy.force (Stdcompat.Lazy.from_val 42) = 42);
  assert (Stdcompat.Char.lowercase_ascii 'A' = 'a');
  assert (Stdcompat.Char.uppercase_ascii 'a' = 'A');
  assert (Stdcompat.Char.equal 'a' 'a');
  assert (not (Stdcompat.Char.equal 'A' 'a'));
  assert (Stdcompat.String.init 2 (fun i -> char_of_int i) = "\000\001");
  assert
    (Stdcompat.String.mapi
       (fun i c -> char_of_int (i + int_of_char c)) "abc" = "ace");
  assert (
    let s = Stdcompat.Bytes.create 3 in
    Stdcompat.String.iteri (fun i c -> Stdcompat.Bytes.set s i c) "abc";
    s = Stdcompat.Bytes.of_string "abc");
  assert (Stdcompat.String.map Stdcompat.Char.uppercase_ascii "abc" = "ABC");
  assert (Stdcompat.String.trim " \t abc\n" = "abc");
  assert (Stdcompat.String.lowercase_ascii "AbcD" = "abcd");
  assert (Stdcompat.String.uppercase_ascii "AbcD" = "ABCD");
  assert (Stdcompat.String.capitalize_ascii "abcD" = "AbcD");
  assert (Stdcompat.String.uncapitalize_ascii "AbcD" = "abcD");
  assert (Stdcompat.String.equal "abc" "abc");
  assert (not (Stdcompat.String.equal "Abc" "abc"));
  assert (Stdcompat.String.split_on_char ' ' " abc  d ef  "
    = ["";"abc";"";"d";"ef";"";""]);
  assert (Stdcompat.String.index_opt "abaababa" 'a' = Some 0);
  assert (Stdcompat.String.index_opt "abaababa" 'c' = None);
  assert (Stdcompat.String.rindex_opt "abaababa" 'a' = Some 7);
  assert (Stdcompat.String.rindex_opt "abaababa" 'c' = None);
  assert (Stdcompat.String.index_from_opt "abaababa" 1 'a' = Some 2);
  assert (Stdcompat.String.index_from_opt "abaababa" 1 'c' = None);
  assert (Stdcompat.String.rindex_from_opt "abaababa" 4 'a' = Some 3);
  assert (Stdcompat.String.rindex_from_opt "abaababa" 4 'c' = None);
  assert (
    let s = Stack.create () in
    Stack.push 1 s;
    Stack.push 2 s;
    Stack.push 3 s;
    Stdcompat.Stack.fold ( + ) 0 s = 6 &&
    Stack.length s = 3);
  assert (
    let t = Hashtbl.create 17 in
    Hashtbl.add t 1 2;
    Hashtbl.add t 3 4;
    (Stdcompat.Hashtbl.stats t).Stdcompat.Hashtbl.num_bindings = 2);
  assert (
    let t = Hashtbl.create 17 in
    Hashtbl.add t 1 1;
    Hashtbl.add t 2 2;
    Hashtbl.add t 2 3;
    Hashtbl.add t 3 4;
    Stdcompat.Hashtbl.filter_map_inplace
        (fun k v -> if k = 3 then None else Some (pred v)) t;
    Hashtbl.find_all t 1 = [0] &&
    Hashtbl.find_all t 2 = [2; 1] &&
    Hashtbl.find_all t 3 = []);
  assert (
    let t = Hashtbl.create 17 in
    Hashtbl.add t 1 1;
    Hashtbl.add t 2 2;
    Stdcompat.Hashtbl.find_opt t 1 = Some 1 &&
    Stdcompat.Hashtbl.find_opt t 3 = None);
  assert (
    let module H = struct
      type t = int

      let equal : int -> int -> bool = ( = )

      let hash : int -> int = fun x -> x
    end in
    let module M = Hashtbl.Make (H) in
    let module M' = Stdcompat.Hashtbl.Make (H) in
    let t = M.create 17 in
    M.add t 1 1;
    M.add t 2 2;
    M'.find_opt t 1 = Some 1 &&
    M'.find_opt t 3 = None);
  assert (
    let module S = Set.Make (String) in
    let module S' = Stdcompat.Set.Make (String) in
    let s = S'.of_list ["a"; "b"; "c"] in
    S.compare
      (S'.map (fun s -> s ^ ".") s)
      (S'.of_list ["a."; "b."; "c."]) = 0 &&
    S'.find_opt "a" s = Some "a" &&
    S'.find_opt "d" s = None &&
    S'.find_first (fun s -> s >= "b") s = "b" &&
    S'.find_last (fun s -> s <= "b") s = "b");
  assert (
    let module M = Map.Make (String) in
    let module M' = Stdcompat.Map.Make (String) in
    let m = M.add "1" 2 M.empty in
    M'.compare compare (
      M'.update "1" (function None -> Some 0 | Some i -> Some (i + 1))
      (M'.update "2" (function None -> Some 0 | Some _ -> None)
         (M'.update "3" (function None -> None | Some i -> Some i) m)))
      (M.add "1" 3 (M.add "2" 0 M.empty)) = 0);
  assert (
    let b = Stdcompat.Bytes.of_string "hello" in
    let b = Stdcompat.Bytes.extend b (-1) 2 in
    Stdcompat.Bytes.sub_string b 0 4 = "ello" &&
    Stdcompat.Bytes.length b = 6);
  assert (
    let l = ref [] in
    let f a b =
      l := (a, b) :: !l in
    Stdcompat.List.iteri f [1; 2; 3];
    !l = [2, 3; 1, 2; 0, 1]);
  assert (
    let f a b =
      (a, b) in
    Stdcompat.List.mapi f  [1; 2; 3] = [0, 1; 1, 2; 2, 3]);
  assert (
    Stdcompat.List.sort_uniq compare  [2; 1; 3; 2; 1; 3]
    = [1; 2; 3]);
  assert (Stdcompat.List.cons 1 [2; 3] = [1; 2; 3]);
  assert (Stdcompat.List.compare_lengths [1] [2; 3] < 0);
  assert (Stdcompat.List.compare_lengths [1; 2] [2; 3] = 0);
  assert (Stdcompat.List.compare_lengths [1; 2; 3] [2; 3] > 0);
  assert (Stdcompat.List.compare_length_with [1] 2 < 0);
  assert (Stdcompat.List.compare_length_with [1; 2] 2 = 0);
  assert (Stdcompat.List.compare_length_with [1; 2; 3] 2 > 0);;
  assert (Stdcompat.List.nth_opt [1; 2; 3] 2 = Some 3);
  assert (Stdcompat.List.nth_opt [1; 2; 3] 3 = None);
  assert (
    try
      ignore (Stdcompat.List.nth_opt [1; 2; 3] (-1));
      false
    with Invalid_argument _ -> true);
  assert (Stdcompat.List.find_opt (fun i -> i mod 2 = 0) [1; 2; 3] = Some 2);
  assert (Stdcompat.List.find_opt (fun i -> i mod 4 = 0) [1; 2; 3] = None);
  assert (Stdcompat.List.assoc_opt 2 [1, 0; 2, 1; 3, 2] = Some 1);
  assert (Stdcompat.List.assoc_opt 4 [1, 0; 2, 1; 3, 2] = None);
  assert (Stdcompat.List.assq_opt 2 [1, 0; 2, 1; 3, 2] = Some 1);
  assert ("a" == "a" || (* "a" == "a" since OCaml 4.10.0 *)
    Stdcompat.List.assq_opt "a" ["a", 1; "b", 2; "c", 3] = None);
  assert (
    let r1 = ref 1 in
    let r1' = ref 1 in
    let r2 = ref 2 in
    let r3 = ref 3 in
    Stdcompat.List.assoc_opt r1' [r1, 1; r2, 2; r3, 3] = Some 1 &&
    Stdcompat.List.assq_opt r1' [r1, 1; r2, 2; r3, 3] = None);
  assert (Stdcompat.Filename.extension "a.b/c.de" = ".de");
  assert (Stdcompat.Filename.extension "a.b/cd" = "");
  assert (Stdcompat.Filename.remove_extension "a.b/c.de" = "a.b/c");
  assert (Stdcompat.Filename.remove_extension "a.b/cd" = "a.b/cd");
  
  assert (Stdcompat.Filename.remove_extension "a.b\\cd" = "a");
  
  (*
  assert (Stdcompat.Filename.remove_extension "a.b\\cd" = "a.b\\cd");
  *)
  assert (
    let array = Stdcompat.Array.Floatarray.create 2 in
    Stdcompat.Array.Floatarray.set array 0 1.;
    Stdcompat.Array.Floatarray.set array 1 2.;
    Stdcompat.Array.Floatarray.get array 0 = 1. &&
    Stdcompat.Array.Floatarray.get array 1 = 2.);
  assert (
    let l = ref [] in
    let f a b =
      l := (a, b) :: !l in
    Stdcompat.Array.iter2 f [| 0; 1 |] [| 2; 3 |];
    !l = [1, 3; 0, 2]);
  assert (
    let f a b =
      (a, b) in
    Stdcompat.Array.map2 f  [| 0; 1 |] [| 2; 3 |] = [| 0, 2; 1, 3 |]);
  assert (Stdcompat.Array.for_all (fun x -> x > 0) [| 1; 2; 3 |]);
  assert (not (Stdcompat.Array.for_all (fun x -> x > 0) [| 1; 2; 0; 3 |]));
  assert (Stdcompat.Array.exists (fun x -> x > 2) [| 1; 2; 3 |]);
  assert (not (Stdcompat.Array.exists (fun x -> x > 3) [| 1; 2; 3 |]));
  assert (Stdcompat.Array.mem "a" [| "a"; "b"; "c" |]);
  assert (not (Stdcompat.Array.mem "d" [| "a"; "b"; "c" |]));
  assert (Stdcompat.Array.memq 2 [| 1; 2; 3 |]);
  assert ("a" == "a" || (* "a" == "a" since OCaml 4.10.0 *)
    not (Stdcompat.Array.memq "a" [| "a"; "b"; "c" |]));
  assert (
    let r1 = ref 1 in
    let r1' = ref 1 in
    let r2 = ref 2 in
    let r3 = ref 3 in
    Stdcompat.List.mem r1' [r1; r2; r3] &&
    not (Stdcompat.List.memq r1' [r1; r2; r3]));
  assert (
    let q = Stdcompat.Queue.create () in
    Stdcompat.Array.of_seq (Stdcompat.Queue.to_seq q) = [| |]);
  assert (
    let q = Stdcompat.Queue.create () in
    Stdcompat.Queue.add_seq q (Stdcompat.List.to_seq ["a"; "b"; "c"]);
    Stdcompat.Array.of_seq (Stdcompat.Queue.to_seq q) = [| "a"; "b"; "c" |]);
  assert (
    let l = Stdcompat.List.to_seq ["a", 1; "b", 2; "c", 3] in
    let module M = Stdcompat.Map.Make (String) in
    let q = Stdcompat.Hashtbl.of_seq (M.to_seq (M.of_seq l)) in
    let m = M.of_seq (Stdcompat.Hashtbl.to_seq q) in
    M.cardinal m = 3 && M.find "a" m = 1 && M.find "b" m = 2
      && M.find "c" m = 3);
  assert (
    Stdcompat.Filename.chop_suffix_opt ~suffix:".txt" "readme.txt"
      = Some "readme");
  assert (
    Stdcompat.Filename.chop_suffix_opt ~suffix:".txt" "x"
      = None);
  assert (
    Stdcompat.Filename.chop_suffix_opt ~suffix:".txt" "readme.md"
      = None);
  (*
  assert (
    Stdcompat.Filename.chop_suffix_opt ~suffix:".txt" "readme.TXT"
      = Some "readme");
  *)
  
  assert (
    Stdcompat.Filename.chop_suffix_opt ~suffix:".txt" "readme.TXT"
      = None);
  
  assert (Stdcompat.Filename.chop_suffix "readme.txt" ".txt" = "readme");
  begin try
    ignore (Stdcompat.Filename.chop_suffix "x" ".txt");
    assert false
  with Invalid_argument _ -> ()
  end;
  begin try
    ignore (Stdcompat.Filename.chop_suffix "readme.md" ".txt");
    assert false
  with Invalid_argument _ -> ()
  end;
  (*
  assert (Stdcompat.Filename.chop_suffix "readme.TXT" ".txt" = "readme");
  *)
  
  begin try
    ignore (Stdcompat.Filename.chop_suffix "readme.TXT" ".txt");
    assert false
  with Invalid_argument _ -> ()
  end;
  
  (*
  assert (Stdcompat.Filename.dir_sep = "\\");
  *)
  
  assert (Stdcompat.Filename.dir_sep = "/");
  
  assert (not (Stdcompat.Float.sign_bit 1.));
  assert (not (Stdcompat.Float.sign_bit 0.));
  assert (not (Stdcompat.Float.is_nan 42.));
  assert (Stdcompat.Float.is_nan (0. /. 0.));
  assert (not (Stdcompat.Float.is_infinite 0.));
  assert (Stdcompat.Float.is_infinite (1. /. 0.));
  assert (not (Stdcompat.Float.is_infinite (0. /. 0.)));
  assert (Stdcompat.Float.is_finite 0.);
  assert (not (Stdcompat.Float.is_finite (1. /. 0.)));
  assert (not (Stdcompat.Float.is_finite (0. /. 0.)));
  assert (Stdcompat.Float.trunc 1.5 = 1.);
  assert (Stdcompat.Float.trunc (-2.6) = -2.);
  assert (Stdcompat.Float.is_infinite (Stdcompat.Float.trunc (1. /. 0.)));
  assert (Stdcompat.Float.is_nan (Stdcompat.Float.trunc (0. /. 0.)));
  assert (Stdcompat.Float.is_integer 1.);
  assert (not (Stdcompat.Float.is_integer 1.5));
  assert (Stdcompat.Float.is_infinite (Stdcompat.Float.trunc (1. /. 0.)));
  assert (Stdcompat.Float.is_nan (Stdcompat.Float.trunc (0. /. 0.)));
  assert (Stdcompat.Float.round 1.5 = 2.);
  assert (Stdcompat.Float.round 1.4 = 1.);
  assert (Stdcompat.Float.round (-2.6) = -3.);
  assert (Stdcompat.Float.round (-3.5) = -4.);
  assert (Stdcompat.Float.round (-4.4) = -4.);
  assert (Stdcompat.Float.is_infinite (Stdcompat.Float.round (1. /. 0.)));
  assert (Stdcompat.Float.is_nan (Stdcompat.Float.round (0. /. 0.)));
  assert (Stdcompat.Float.sign_bit (-1.));
  assert (Stdcompat.Float.min_num 1. (0. /. 0.) = 1.);
  assert (Stdcompat.Float.min_num (0. /. 0.) 1. = 1.);
  assert (Stdcompat.Float.min_num 1. (-1.) = (-1.));
  assert (Stdcompat.Float.min_num (-1.) 1. = (-1.));
  assert (Stdcompat.Float.sign_bit (Stdcompat.Float.min_num 0. (-0.)));
  assert (Stdcompat.Float.sign_bit (Stdcompat.Float.min_num (-0.) 0.));
  assert (Stdcompat.Float.is_nan (
    Stdcompat.Float.min_num (0. /. 0.) (0. /. 0.)));
  assert (Stdcompat.Float.max_num 1. (0. /. 0.) = 1.);
  assert (Stdcompat.Float.max_num (0. /. 0.) 1. = 1.);
  assert (Stdcompat.Float.max_num 1. (-1.) = 1.);
  assert (Stdcompat.Float.max_num (-1.) 1. = 1.);
  assert (not (Stdcompat.Float.sign_bit (Stdcompat.Float.max_num 0. (-0.))));
  assert (not (Stdcompat.Float.sign_bit (Stdcompat.Float.max_num (-0.) 0.)));
  assert (Stdcompat.Float.is_nan (
    Stdcompat.Float.max_num (0. /. 0.) (0. /. 0.)));
  assert (Stdcompat.Float.min_max_num 1. (0. /. 0.) = (1., 1.));
  assert (Stdcompat.Float.min_max_num (0. /. 0.) 1. = (1., 1.));
  assert (Stdcompat.Float.min_max_num 1. (-1.) = (-1., 1.));
  assert (Stdcompat.Float.min_max_num (-1.) 1. = (-1., 1.));
  assert (
    let min, max = Stdcompat.Float.min_max_num 0. (-0.) in
    Stdcompat.Float.sign_bit min &&
    not (Stdcompat.Float.sign_bit max));
  assert (
    let min, max = Stdcompat.Float.min_max_num (-0.) 0. in
    Stdcompat.Float.sign_bit min &&
    not (Stdcompat.Float.sign_bit max));
  assert (
    let min, max = Stdcompat.Float.min_max_num (0. /. 0.) (0. /. 0.) in
    Stdcompat.Float.is_nan min &&
    Stdcompat.Float.is_nan max);
  assert (Stdcompat.Float.is_nan (Stdcompat.Float.min 1. (0. /. 0.)));
  assert (Stdcompat.Float.is_nan (Stdcompat.Float.min (0. /. 0.) 1.));
  assert (Stdcompat.Float.min 1. (-1.) = (-1.));
  assert (Stdcompat.Float.min (-1.) 1. = (-1.));
  assert (Stdcompat.Float.sign_bit (Stdcompat.Float.min 0. (-0.)));
  assert (Stdcompat.Float.sign_bit (Stdcompat.Float.min (-0.) 0.));
  assert (Stdcompat.Float.is_nan (
    Stdcompat.Float.min (0. /. 0.) (0. /. 0.)));
  assert (Stdcompat.Float.is_nan (Stdcompat.Float.max 1. (0. /. 0.)));
  assert (Stdcompat.Float.is_nan (Stdcompat.Float.max (0. /. 0.) 1.));
  assert (Stdcompat.Float.max 1. (-1.) = 1.);
  assert (Stdcompat.Float.max (-1.) 1. = 1.);
  assert (not (Stdcompat.Float.sign_bit (Stdcompat.Float.max 0. (-0.))));
  assert (not (Stdcompat.Float.sign_bit (Stdcompat.Float.max (-0.) 0.)));
  assert (Stdcompat.Float.is_nan (
    Stdcompat.Float.max (0. /. 0.) (0. /. 0.)));
  assert (
    let min, max = Stdcompat.Float.min_max 1. (0. /. 0.) in
    Stdcompat.Float.is_nan min &&
    Stdcompat.Float.is_nan max);
  assert (
    let min, max = Stdcompat.Float.min_max (0. /. 0.) 1. in
    Stdcompat.Float.is_nan min &&
    Stdcompat.Float.is_nan max);
  assert (Stdcompat.Float.min_max 1. (-1.) = (-1., 1.));
  assert (Stdcompat.Float.min_max (-1.) 1. = (-1., 1.));
  assert (
    let min, max = Stdcompat.Float.min_max 0. (-0.) in
    Stdcompat.Float.sign_bit min &&
    not (Stdcompat.Float.sign_bit max));
  assert (
    let min, max = Stdcompat.Float.min_max (-0.) 0. in
    Stdcompat.Float.sign_bit min &&
    not (Stdcompat.Float.sign_bit max));
  assert (
    let min, max = Stdcompat.Float.min_max (0. /. 0.) (0. /. 0.) in
    Stdcompat.Float.is_nan min &&
    Stdcompat.Float.is_nan max);
  assert (Stdcompat.Float.next_after max_float infinity = infinity);
  assert (Stdcompat.Float.next_after 0. infinity = Int64.float_of_bits Stdcompat.Int64.one);
  assert (Stdcompat.Float.next_after (Int64.float_of_bits Stdcompat.Int64.one) 0. = 0.);
  assert (Stdcompat.Float.next_after 1. 1. = 1.);
  assert (Stdcompat.Float.is_nan (Stdcompat.Float.next_after (0. /. 0.) 1.));
  assert (Stdcompat.Float.is_nan (Stdcompat.Float.next_after 1. (0. /. 0.)));
  let b = Stdcompat.Bytes.of_string "\x20\x30\x40\x50\x60\x70\x80\x90" in
  assert (Stdcompat.Bytes.get_uint8 b 1 = 0x30);
  assert (Stdcompat.Bytes.get_int8 b 1 = 0x30);
  assert (Stdcompat.Bytes.get_uint8 b 6 = 0x80);
  assert (Stdcompat.Bytes.get_int8 b 6 = -0x80);
  assert (Stdcompat.Bytes.get_uint16_le b 4 = 0x7060);
  assert (Stdcompat.Bytes.get_uint16_be b 4 = 0x6070);
  assert (Stdcompat.Bytes.get_int16_le b 4 = 0x7060);
  assert (Stdcompat.Bytes.get_int16_be b 4 = 0x6070);
  assert (Stdcompat.Bytes.get_uint16_le b 6 = 0x9080);
  assert (Stdcompat.Bytes.get_uint16_be b 6 = 0x8090);
  assert (Stdcompat.Bytes.get_int16_le b 6 = -0x6F80);
  assert (Stdcompat.Bytes.get_int16_be b 6 = -0x7F70);
  assert (Stdcompat.Bytes.get_int32_le b 0 = 0x50403020l);
  assert (Stdcompat.Bytes.get_int32_be b 0 = 0x20304050l);
  assert (Stdcompat.Bytes.get_int64_le b 0 = 0x9080706050403020L);
  assert (Stdcompat.Bytes.get_int64_be b 0 = 0x2030405060708090L);
  let check_invalid_arg f =
    try
      f ();
      false
    with Invalid_argument _ ->
      true in
  assert (check_invalid_arg (fun () -> Stdcompat.Bytes.get_uint8 b (-1)));
  assert (check_invalid_arg (fun () -> Stdcompat.Bytes.get_uint8 b 8));
  assert (check_invalid_arg (fun () -> Stdcompat.Bytes.get_uint16_le b (-1)));
  assert (check_invalid_arg (fun () -> Stdcompat.Bytes.get_uint16_le b 7));
  assert (check_invalid_arg (fun () -> Stdcompat.Bytes.get_int32_le b (-1)));
  assert (check_invalid_arg (fun () -> Stdcompat.Bytes.get_int32_le b 5));
  assert (check_invalid_arg (fun () -> Stdcompat.Bytes.get_int64_le b (-1)));
  assert (check_invalid_arg (fun () -> Stdcompat.Bytes.get_int64_le b 1));
  assert (
    Stdcompat.Bytes.set_uint8 b 1 0x90;
    Stdcompat.Bytes.get_uint8 b 1 = 0x90);
  assert (
    Stdcompat.Bytes.set_int8 b 1 (-0x20);
    Stdcompat.Bytes.get_int8 b 1 = (-0x20));
  assert (
    Stdcompat.Bytes.set_uint16_le b 1 0x1234;
    Stdcompat.Bytes.get_uint16_le b 1 = 0x1234);
  assert (
    Stdcompat.Bytes.set_uint16_be b 1 0x1234;
    Stdcompat.Bytes.get_uint16_be b 1 = 0x1234);
  assert (
    Stdcompat.Bytes.set_int16_le b 1 (-0x1234);
    Stdcompat.Bytes.get_int16_le b 1 = (-0x1234));
  assert (
    Stdcompat.Bytes.set_int16_be b 1 (-0x1234);
    Stdcompat.Bytes.get_int16_be b 1 = (-0x1234));
  assert (
    Stdcompat.Bytes.set_int32_le b 1 0x12345678l;
    Stdcompat.Bytes.get_int32_le b 1 = 0x12345678l);
  assert (
    Stdcompat.Bytes.set_int32_be b 1 0x12345678l;
    Stdcompat.Bytes.get_int32_be b 1 = 0x12345678l);
  assert (
    Stdcompat.Bytes.set_int64_le b 0 0x123456789ABCDEF0L;
    Stdcompat.Bytes.get_int64_le b 0 = 0x123456789ABCDEF0L);
  assert (
    Stdcompat.Bytes.set_int64_be b 0 0x123456789ABCDEF0L;
    Stdcompat.Bytes.get_int64_be b 0 = 0x123456789ABCDEF0L);
  assert (check_invalid_arg (fun () -> Stdcompat.Bytes.set_uint8 b (-1) 0));
  assert (check_invalid_arg (fun () -> Stdcompat.Bytes.set_uint8 b 8 0));
  assert (check_invalid_arg (fun () -> Stdcompat.Bytes.set_uint16_le b (-1) 0));
  assert (check_invalid_arg (fun () -> Stdcompat.Bytes.set_uint16_le b 7 0));
  assert (check_invalid_arg (fun () -> Stdcompat.Bytes.set_int32_le b (-1) 0l));
  assert (check_invalid_arg (fun () -> Stdcompat.Bytes.set_int32_le b 5 0l));
  assert (check_invalid_arg (fun () -> Stdcompat.Bytes.set_int64_le b (-1) 0L));
  assert (check_invalid_arg (fun () -> Stdcompat.Bytes.set_int64_le b 1 0L));
  assert (Stdcompat.Bytes.get_int64_be b 0 = 0x123456789ABCDEF0L);
  assert (
    let finalized = ref false in
    Stdcompat.Fun.protect ~finally:(fun () -> finalized := true) (fun () -> true) &&
    !finalized);
  assert (
    try
      Stdcompat.Fun.protect ~finally:(fun () -> raise Exit) (fun () -> failwith "protect")
    with Stdcompat.Fun.Finally_raised Exit -> true | _ -> false);
  let (_ : 'a Stdcompat.Seq.t) = Stdcompat.Stdlib.Seq.empty in
  let (_ : 'a Stdcompat.Stdlib.Seq.t) = Stdcompat.Seq.empty in
  let (_ : (unit, 'a) Stdcompat.result) = Stdcompat.Ok () in
  let (_ : (unit, 'a) Stdcompat.Result.t) = Stdcompat.Ok () in
  let (_ : (unit, 'a) Stdcompat.Stdlib.Result.t) = Stdcompat.Ok () in
  let (_ : (unit, 'a) Stdcompat.result) = Stdcompat.Result.Ok () in
  let (_ : (unit, 'a) Stdcompat.Result.t) = Stdcompat.Result.Ok () in
  let (_ : (unit, 'a) Stdcompat.Stdlib.Result.t) = Stdcompat.Result.Ok () in
  let (_ : (unit, 'a) Stdcompat.result) = Stdcompat.Stdlib.Result.Ok () in
  let (_ : (unit, 'a) Stdcompat.Result.t) = Stdcompat.Stdlib.Result.Ok () in
  let (_ : (unit, 'a) Stdcompat.Stdlib.Result.t) = Stdcompat.Stdlib.Result.Ok () in
  assert (Stdcompat.Fun.id true);
  assert (Stdcompat.Fun.const true false);
  assert (Stdcompat.Fun.flip ( - ) 1 2 = 1);
  assert (Stdcompat.Fun.negate not true);
  assert (
    let bytes = Stdcompat.Bytes.of_string "abcde" in
    Stdcompat.Bytes.unsafe_blit_string "ABCDEF" 2 bytes 1 3;
    Stdcompat.Bytes.compare bytes (Stdcompat.Bytes.of_string "aCDEe") == 0);
  assert (
    Stdcompat.List.concat_map
      (fun x -> [x * 2; x - 1]) [1; 2; 3] = [2; 0; 4; 1; 6; 2]);
  assert (
    Stdcompat.List.find_map (Stdcompat.List.assoc_opt 1)
      [[2, 3; 4, 1]; [5, 2;1, 7]] = Some 7);
  assert (
    Stdcompat.List.find_map (Stdcompat.List.assoc_opt 1)
      [[2, 3; 4, 1]; [5, 2;2, 7]] = None);
  let check_quote_string s =
    let (file, channel) = Filename.open_temp_file "test_quote_command" ".ml" in
    Stdcompat.Fun.protect (fun () ->
      Stdcompat.Fun.protect (fun () ->
        Printf.fprintf channel "
          assert (Sys.argv.(1) = \"%s\")
        " (String.escaped s))
      ~finally:(fun () -> close_out channel);
      let file_exe = Filename.chop_suffix file ".ml" ^ ".exe" in
      assert (
        Sys.command (Stdcompat.Filename.quote_command "ocamlopt.opt"
          [file; "-o"; file_exe]) == 0);
      assert (
        Sys.command (Stdcompat.Filename.quote_command file_exe [s]) == 0))
    ~finally:(fun () -> Sys.remove file) in
  check_quote_string "a\\\\";
  check_quote_string "a\\\\b\"\\\"\\\\\"\"\\";
  test_array_for_all2 ();
  test_array_exists2 ();
  test_list_filteri ();
  test_list_fold_left_map ();
  test_seq_cons ();
  test_seq_append ();
  test_seq_unfold ();
  test_seq_4_14 ();
  test_set_filter_map ();
  test_map_filter_map ();
  test_mkdir_rmdir ();
  test_set_to_seq_to_rev_seq ();
  test_map_to_seq_to_rev_seq ();
  test_list_partition_map ();
  test_list_compare ();
  test_list_equal ();
  test_hashtbl_rebuild ();
  test_format_pp_print_seq ();
  test_either ();
  test_seq_concat ();
  test_int32_min_max ();
  test_int32_unsigned_compare ();
  test_array_fold_left_map ();
  test_array_find_opt ();
  test_array_find_map ();
  test_array_split ();
  test_array_combine ();
  test_string_fold_left ();
  test_string_fold_right ();
  test_string_for_all ();
  test_string_exists ();
  test_string_starts_with ();
  test_string_ends_with ();
  test_channels ();
  ()
