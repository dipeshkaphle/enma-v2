open Core

let () =
  let module E = Enma in
  let open E in
  let sexp = "(record Foo (forall a l) ((x a) (y l)))" in
  try
    let ast = Parser.parse sexp in
    print_endline @@ Sexplib.Sexp.to_string_hum @@ Ast.Program.sexp_of_t ast
  with e -> print_endline @@ Exn.to_string e
