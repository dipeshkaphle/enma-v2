open Core

type name = string [@@deriving sexp]

type typ = SimpleTy of name * typ list | ArrowTy of name * typ
[@@deriving sexp]

type 'a typed = { ty : typ; content : 'a } [@@deriving sexp]

type 'a maybe_typed = { mutable ty : typ option; content : 'a }
[@@deriving sexp]

type field_t = { name : name; ty : typ } [@@deriving sexp]

type record_t = { name : name; type_params : typ list; fields : field_t list }
[@@deriving sexp]

type abs_t = {
  type_params : typ list;
  params : name maybe_typed list;
  mutable ret_type : typ option;
  body : stmt list;
}
[@@deriving sexp]

and named_abs_t = { name : name; abs : abs_t } [@@deriving sexp]

and expr =
  | Int of Int64.t
  | Float of Float.t
  | Ident of name
  | Lam of abs_t
  | App of { fn : name; args : name list }
[@@deriving sexp]

and stmt =
  | Expr of expr
  | FnDecl of named_abs_t
  | RecordDecl of record_t
  | EnumDecl
[@@deriving sexp]

module Expression = struct
  type t = expr [@@deriving sexp]
end

module Statement = struct
  type t = stmt [@@deriving sexp]
end

module Program : sig
  type t [@@deriving sexp]

  val of_list : Statement.t list -> t
  val to_list : t -> Statement.t list
end = struct
  type t = Statement.t list [@@deriving sexp]

  let of_list ts = ts
  let to_list t = t
end
