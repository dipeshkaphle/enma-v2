exception ParseFailure of Core.Sexp.t * string [@@deriving sexp]
exception CombinedFailure of exn list [@@deriving sexp]

val parse : string -> Ast.Program.t
