open Core
module E = Enma

let expressions = [ "1"; "1.1"; "someName"; "some_name"; "some_name'" ]

let expected_asts =
  [
    E.Ast.Expr (E.Ast.Int 1L);
    E.Ast.Expr (E.Ast.Float 1.1);
    E.Ast.Expr (E.Ast.Ident "someName");
    E.Ast.Expr (E.Ast.Ident "some_name");
    E.Ast.Expr (E.Ast.Ident "some_name'");
  ]

let simple_parser_test () =
  let _ =
    List.map
      ~f:(fun (s, e) ->
        Alcotest.(check bool)
          "Check parser"
          (Sexp.equal
             (E.Ast.Statement.sexp_of_t
                (List.hd_exn @@ E.Ast.Program.to_list @@ E.Parser.parse_exn s))
             (E.Ast.Statement.sexp_of_t e))
          true)
      (List.zip_exn expressions expected_asts)
  in
  ()

let () =
  Alcotest.run "enma"
    [
      ("parser_tests", [ Alcotest.test_case "simple" `Quick simple_parser_test ]);
    ]
