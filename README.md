# Design Decisions
- [ ] EBNF
  - [ ] Create from scratch
  - [ ] Lisp
  - [ ] Other
  - Note: Should be simple to lex and parse
- [ ] Compiler Language
  - [ ] Rust
  - [ ] OCaml
  - [ ] Other
  - Note: Should allow fast iterations of language features
- [ ] Language Features
  - [ ] Strong type system
  - [ ] Type inference
  - [ ] Garbage collection
  - [ ] Just-in-time compilation
  - [ ] Other




## Syntax

### Records

NOTE: Even though there's parsing of generics with Constraints, it's not supported yet.

``` lisp
(record foo ((x int) (y int)))

(record foo (forall a)  -- a is Any,which is special
        ((x int) 
        (y int) 
        (z a))
    )
(record foo (forall ( a Showable)) 
        ((x int) 
        (y int) 
        (z a))
    )
```

### Functions

TODO: body is not being parsed as of now
``` lisp
(fn foo ((x int) (y bool) returns int) (<body>) )
(fn foo (x y) (<body>) ) -- type is inferred
(fn (forall (a Debug) (b Iterable)) ((x a) (y b)) (<body>)) -- type inferred from usage 
(fn (forall a b) ((x a) (y b)) (<body>)) -- type inferred from usage
```
