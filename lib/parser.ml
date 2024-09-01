open Core

exception ParseFailure of Sexp.t * string [@@deriving sexp]
exception CombinedFailure of exn list [@@deriving sexp]

type 'a parse_result = ('a, exn) Result.t

let map_exn_to_some res =
  match res with Result.Error e -> Option.some e | _ -> None

let lift_result_from_list (l : 'a parse_result list) : 'a list parse_result =
  if List.for_all ~f:Result.is_ok l then
    Result.return @@ List.map ~f:Result.ok_exn l
  else Result.fail (CombinedFailure (List.filter_map l ~f:map_exn_to_some))

let rec try_all s fns =
  match fns with
  | [] -> None
  | fn :: fns' -> (
      let fs = fn s in
      match fs with None -> try_all s fns' | Some v -> Some v)

let is_valid_identifier s =
  let is_valid =
    String.for_alli s ~f:(fun i c ->
        if i = 0 then Char.is_alpha c || Char.equal c '_'
        else Char.is_alphanum c || Char.equal c '_' || Char.equal c '\'')
  in
  is_valid

let ensure_valid_identifier s =
  (*Add logic for ensuring the string is a valid identifier*)
  if is_valid_identifier s then Option.some s else None

let get_atom : Sexp.t -> string option =
 fun s -> match s with Sexp.Atom a -> Option.some a | _ -> None

let get_atom_result : Sexp.t -> string parse_result =
 fun s ->
  match get_atom s with
  | None -> Result.fail (ParseFailure (s, "Not an atom"))
  | Some id -> Result.return id

let get_atom_exn : Sexp.t -> string =
 fun s ->
  match get_atom s with
  | None -> raise (ParseFailure (s, "Not an atom"))
  | Some id -> id

let get_list_result : Sexp.t -> Sexp.t list parse_result =
 fun s ->
  match s with
  | Sexp.List s -> Result.return s
  | _ -> Result.fail (ParseFailure (s, "Not a list"))

let rec parse_type : Sexp.t -> Ast.typ parse_result = function
  | Sexp.Atom s -> Ast.SimpleTy (s, []) |> Result.return
  | Sexp.List (hd :: tl) ->
      Result.Let_syntax.(
        let%bind typename =
          Result.of_option
            ~error:(ParseFailure (hd, "Couldn't parse name of record field"))
            (get_atom hd)
        in
        (* let params = List.map tl ~f:parse_type in *)
        (* if List.for_all params ~f:Result.is_ok then *)
        (*   return *)
        (*     (Ast.SimpleTy *)
        (*        (typename, List.map ~f:(fun ty -> Result.ok_exn ty) params)) *)
        (* else *)
        (*   Result.fail *)
        (*     (CombinedFailure (List.filter_map params ~f:map_exn_to_some))) *)
        let%bind params = lift_result_from_list @@ List.map tl ~f:parse_type in
        return (Ast.SimpleTy (typename, params)))
  | l -> Result.fail (ParseFailure (l, "cannot parse type out of empty list"))

let parse_record_field : Sexp.t -> Ast.field_t parse_result =
 fun t ->
  match t with
  | Sexp.List [ name; ty ] ->
      Result.Let_syntax.(
        let%bind field_name = get_atom_result name in
        let%bind ty = parse_type ty in
        return { Ast.name = field_name; ty })
  | _ ->
      Result.fail
        (ParseFailure
           ( t,
             "Cannot parse (<id> <type>) because there's no type specified and \
              syntax isn't right" ))

(* let parse_input_parameter : Sexp.t -> Ast.name Ast.maybe_typed parse_result = *)
(*  fun t -> *)
(*  let open Result.Let_syntax in *)
(*   match t with *)
(*   | Sexp.List [ name; ty ] -> *)
(*         let%bind field_name = get_atom_result name in *)
(*         let%bind ty = parse_type ty in *)
(*         return { Ast.content = field_name; ty=(Some ty) } *)
(*   | Sexp.Atom name -> return {Ast.content = name ; ty = None} *)
(*   | _ -> *)
(*       Result.fail *)
(*         (ParseFailure *)
(*            ( t, *)
(* "Cannot parse (<id> <type>) because there's no type specified and \ *)
   (*               syntax isn't right" )) *)

let parse_record_fields : Sexp.t -> Ast.field_t list parse_result =
 fun fields ->
  match fields with
  | Sexp.List l ->
      Result.Let_syntax.(
        let%bind parsed_fields =
          lift_result_from_list @@ List.map l ~f:parse_record_field
        in
        return parsed_fields)
  | _ ->
      Result.fail
        (ParseFailure (fields, "Cannot parse record fields from given sexp"))

let parse_maybe_typed_name : Sexp.t -> Ast.name Ast.maybe_typed parse_result =
  function
  | Sexp.Atom atom -> Result.return { Ast.content = atom; ty = None }
  | Sexp.List [ name; ty ] ->
      Result.Let_syntax.(
        let%bind name = get_atom_result name in
        let%bind ty = parse_type ty in
        return { Ast.content = name; ty = Some ty })
  | Sexp.List l as original_sexp ->
      Result.fail
        (ParseFailure
           ( original_sexp,
             "Expected list with only 2 elements, got something less or more" ))

let parse_forall_type_constr = function
  | Sexp.Atom atom -> Result.return @@ Ast.ConstrGenericTy (atom, "Any")
  | Sexp.List [ name; ty ] ->
      Result.Let_syntax.(
        let%bind name = get_atom_result name in
        let%bind type_constr = get_atom_result ty in
        return @@ Ast.ConstrGenericTy (name, type_constr))
  | Sexp.List l as original_sexp ->
      Result.fail
        (ParseFailure
           ( original_sexp,
             "Expected list with only 2 elements e.g: (a Any), got something \
              less or more" ))

let parse_forall = function
  | Sexp.List (forall_token :: rest) ->
      Result.Let_syntax.(
        let%bind forall_tok = get_atom_result forall_token in
        if String.equal forall_tok Keywords.forall then
          let%bind typenames_and_constraints =
            lift_result_from_list @@ List.map ~f:parse_forall_type_constr rest
          in
          return typenames_and_constraints
        else
          Result.fail (ParseFailure (forall_token, "Expected forall keyword")))
  | _ as t -> Result.fail (ParseFailure (t, "Expected (forall <typeparams> )"))

(* Parses something like this ((x int) (y bool) returns int) *)
let rec parse_fn_input_params_and_return_type l =
  let open Result.Let_syntax in
  match l with
  | [] -> return ([], None)
  | hd :: tl -> (
      let hd_as_atom = get_atom hd in
      match hd_as_atom with
      | Some atom when String.equal atom Keywords.returns ->
          let%bind ret_type = parse_type (Sexp.List tl) in
          return ([], Option.some ret_type)
      | Some atom -> return ([ { Ast.content = atom; ty = None } ], None)
      | None ->
          (*must mean it's a list and we can continue parsing input param of form (<id> <type>)*)
          let%bind input_type_param = parse_record_field hd in
          let%bind rest_of_input_type_params, ret =
            parse_fn_input_params_and_return_type tl
          in
          return
            ( {
                Ast.content = input_type_param.name;
                ty = Some input_type_param.ty;
              }
              :: rest_of_input_type_params,
              ret ))

(* Explicity typed lambda: (fn ((x int) (y bool) returns int) (<body>) ) *)
(* Loosely typed lambda: (fn (x y) (<body>) ) , type inferred from usage *)
(* generic lambda: (fn (forall (a Debug) (b Iterable)) ((x a) (y b)) (<body>) ) , type inferred from usage *)
(* generic lambda: (fn (forall a b) ((x a) (y b)) (<body>) ) , type inferred from usage *)
let parse_fn_expression (t : Sexp.t list) : Ast.abs_t parse_result =
  match t with
  | [ foralls; type_decl; body ] ->
      Result.Let_syntax.(
        let%bind type_params = parse_forall foralls in
        let%bind type_decl_as_list = get_list_result type_decl in
        let%bind input_type_params, ret_type =
          parse_fn_input_params_and_return_type type_decl_as_list
        in
        (*TODO: Parse function body *)
        let body = [] in
        let lam : Ast.abs_t =
          { type_params; params = input_type_params; ret_type; body }
        in
        Result.return lam)
  | [ type_decl; body ] ->
      Result.Let_syntax.(
        let%bind type_decl_as_list = get_list_result type_decl in
        let%bind input_type_params, ret_type =
          parse_fn_input_params_and_return_type type_decl_as_list
        in
        (*TODO: Parse function body *)
        let body = [] in
        let lam : Ast.abs_t =
          { type_params = []; params = input_type_params; ret_type; body }
        in
        Result.return lam)
  | _ -> Result.fail (ParseFailure (Sexp.List t, "Cannot parse"))

(* Explicity typed function declaration: (fn foo ((x int) (y bool) returns int) (<body>) ) *)
(* Loosely typed function declaration: (fn foo (x y) (<body>) ) , type inferred *)
(* generic function: (fn (forall (a Debug) (b Iterable)) ((x a) (y b)) (<body>)) , type inferred from usage *)
(* generic function: (fn (forall a b) ((x a) (y b)) (<body>)) , type inferred from usage *)
let parse_fn (t : Sexp.t list) =
  match t with
  | name :: rest ->
      Result.Let_syntax.(
        let%bind name = get_atom_result name in
        let%bind fn = parse_fn_expression rest in
        return @@ Ast.FnDecl { name; lambda = fn })
  | _ -> Result.fail (ParseFailure (Sexp.List t, "Cannot parse"))

(* (record foo ((x int) (y int)))  *)
(* (record foo (forall a) ((x int) (y int) (z a)))  *)
let parse_record (t : Sexp.t list) =
  match t with
  | name :: rest -> (
      Result.Let_syntax.(
        let%bind name = get_atom_result name in
        match rest with
        | [ fields_decl ] ->
            let%bind fields = parse_record_fields fields_decl in
            let rcd = { Ast.name; type_params = []; fields } in
            return (Ast.RecordDecl rcd)
        | [ forall_decl; fields_decl ] ->
            let%bind params = parse_forall forall_decl in
            let%bind fields = parse_record_fields fields_decl in
            let rcd = { Ast.name; type_params = params; fields } in
            return (Ast.RecordDecl rcd)
        | _ ->
            Result.fail
              (ParseFailure
                 (Sexp.List rest, "Unexpected syntax after record name:" ^ name))
        (* let%bind fields_sexp = get_list_result fields_decl in *)
        (* let%bind fields = parse_record_fields fields_decl in *)
        (* let rcd = { Ast.name; type_params = []; fields } in *)
        (* return (Ast.RecordDecl rcd) *)))
  | _ -> Result.fail (ParseFailure (Sexp.List t, "Cannot parse"))

(* (enum (option a) ((Some a) (None)))  *)
let parse_enum (t : Sexp.t list) = Result.return Ast.EnumDecl

let transform_sexp (t : Sexp.t) : Ast.Statement.t =
  match t with
  | Sexp.Atom s ->
      let open Ast in
      let ast =
        try_all s
          [
            (fun s ->
              (*Try to parse int*)
              s |> Int64.of_string_opt |> Option.map ~f:(fun x -> Expr (Int x)));
            (fun s ->
              (*Try to parse float*)
              s |> Float.of_string_opt
              |> Option.map ~f:(fun x -> Expr (Float x)));
            (fun s ->
              (* Try to parse identifier *)
              s |> ensure_valid_identifier
              |> Option.map ~f:(fun s -> Expr (Ident s)));
          ]
      in
      Option.value_exn
        ~message:("None of the rules match with given atom:" ^ s)
        ast
  | Sexp.List [] -> raise (ParseFailure (t, "empty sexp cannot be parsed"))
  | Sexp.List (hd :: tl) ->
      let identifier = get_atom_exn hd in
      if is_valid_identifier identifier then
        match identifier with
        | _ when String.equal identifier Keywords.record ->
            Result.ok_exn (parse_record tl)
        | _ when String.equal identifier Keywords.fn ->
            Result.ok_exn (parse_fn tl)
        | _ when String.equal identifier Keywords.enum ->
            Result.ok_exn (parse_enum tl)
        | _ -> failwith (Format.sprintf "%s is an unknown keyword" identifier)
      else failwith (Format.sprintf "%s is not a valid identifier" identifier)

let parse s =
  Sexp.of_string_many_conv_exn s transform_sexp |> Ast.Program.of_list

let parse_and_print ast =
  print_endline @@ Sexplib.Sexp.to_string_hum @@ Ast.Program.sexp_of_t
  @@ parse ast

(** TESTS **)

(* Mostly has just the happy paths,TODO: cover more paths *)
let%test "sanity test" = 1 = 1

let%expect_test "record parsing" =
  parse_and_print "(record foo ((x int) (y int)))";
  [%expect
    {|
    ((RecordDecl
      ((name foo) (type_params ())
       (fields
        (((name x) (ty (SimpleTy int ()))) ((name y) (ty (SimpleTy int ()))))))))
    |}];
  parse_and_print
    "(record foo (forall a) \n\
    \        ((x int) \n\
    \        (y int) \n\
    \        (z a))\n\
    \    )";
  [%expect
    {|
    ((RecordDecl
      ((name foo) (type_params ((ConstrGenericTy a Any)))
       (fields
        (((name x) (ty (SimpleTy int ()))) ((name y) (ty (SimpleTy int ())))
         ((name z) (ty (SimpleTy a ()))))))))
    |}];
  parse_and_print
    "(record foo (forall (a Showable)) \n\
    \        ((x int) \n\
    \        (y int) \n\
    \        (z a))\n\
    \    )";

  [%expect
    {|
    ((RecordDecl
      ((name foo) (type_params ((ConstrGenericTy a Showable)))
       (fields
        (((name x) (ty (SimpleTy int ()))) ((name y) (ty (SimpleTy int ())))
         ((name z) (ty (SimpleTy a ()))))))))
    |}]

let%expect_test "functions parsing" =
  parse_and_print "(fn foo ((x int) (y bool) returns int) ( (f y) (g x) ) )";
  [%expect
    {|
    ((FnDecl
      ((name foo)
       (lambda
        ((type_params ())
         (params
          (((ty ((SimpleTy int ()))) (content x))
           ((ty ((SimpleTy bool ()))) (content y))))
         (ret_type ((SimpleTy int ()))) (body ()))))))
    |}];
  parse_and_print "(fn foo (x y) () )";
  [%expect
    {|
    ((FnDecl
      ((name foo)
       (lambda
        ((type_params ()) (params (((ty ()) (content x)))) (ret_type ())
         (body ()))))))
    |}];
  parse_and_print "(fn foo ((x int) (y bool) returns int) ((f x) (g y) ) )";
  [%expect
    {|
    ((FnDecl
      ((name foo)
       (lambda
        ((type_params ())
         (params
          (((ty ((SimpleTy int ()))) (content x))
           ((ty ((SimpleTy bool ()))) (content y))))
         (ret_type ((SimpleTy int ()))) (body ()))))))
    |}];
  parse_and_print "(fn foo ((x int) (y bool) returns int) ((f x ) (g y)) )";
  [%expect
    {|
    ((FnDecl
      ((name foo)
       (lambda
        ((type_params ())
         (params
          (((ty ((SimpleTy int ()))) (content x))
           ((ty ((SimpleTy bool ()))) (content y))))
         (ret_type ((SimpleTy int ()))) (body ()))))))
    |}]
