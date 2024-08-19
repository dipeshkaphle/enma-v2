(*TODO: think this through properly*)

(* open Effect *)
(* open Effect.Deep *)

(* type _ Effect.t += String : unit Effect.t *)
(* type _ Effect.t += Number : unit Effect.t *)

type literal =
  | String of string
  | Int of int64
  | Bool of bool
  | Char of char
  | Float of float

type token = LParen | RParen | Id of string | Literal of literal

(* let  *)
