type primitive =
  | PSwap [@value 1]
  | PDup
  | PDrop
  | PBury
  | PDig
  | PAdd
  | PSub
  | PMul
  | PDiv
  | PConcat
  | PItoa
  | PPrint
[@@deriving enum]

let string_of_primitive = function
  | PSwap -> "swap"
  | PDup -> "dup"
  | PDrop -> "drop"
  | PBury -> "bury"
  | PDig -> "dig"
  | PAdd -> "+"
  | PSub -> "-"
  | PMul -> "*"
  | PDiv -> "div"
  | PConcat -> ".."
  | PItoa -> "itoa"
  | PPrint -> "print"

let primitive_of_string = function
  | "swap" -> Some PSwap
  | "dup" -> Some PDup
  | "drop" -> Some PDrop
  | "bury" -> Some PBury
  | "dig" -> Some PDig
  | "+" -> Some PAdd
  | "-" -> Some PSub
  | "*" -> Some PMul
  | "div" -> Some PDiv
  | ".." -> Some PConcat
  | "itoa" -> Some PItoa
  | "print" -> Some PPrint
  | _ -> None

let effect_of_primitive =
  let open Typing in
  function
  | PDup -> ([ TyVar (TypeId 0) ], [ TyVar (TypeId 0); TyVar (TypeId 0) ])
  | PDrop -> ([ TyVar (TypeId 0) ], [])
  | PSwap ->
      ( [ TyVar (TypeId 1); TyVar (TypeId 0) ],
        [ TyVar (TypeId 0); TyVar (TypeId 1) ] )
  | PBury ->
      ( [ TyVar (TypeId 2); TyVar (TypeId 1); TyVar (TypeId 0) ],
        [ TyVar (TypeId 1); TyVar (TypeId 0); TyVar (TypeId 2) ] )
  | PDig ->
      ( [ TyVar (TypeId 2); TyVar (TypeId 1); TyVar (TypeId 0) ],
        [ TyVar (TypeId 0); TyVar (TypeId 2); TyVar (TypeId 1) ] )
  | PAdd | PSub | PMul -> ([ ty_int; ty_int ], [ ty_int ])
  | PDiv -> ([ ty_int; ty_int ], [ ty_int; ty_int ])
  | PConcat -> ([ ty_str; ty_str ], [ ty_str ])
  | PItoa -> ([ ty_int ], [ ty_str ])
  | PPrint -> ([ ty_str ], [])
