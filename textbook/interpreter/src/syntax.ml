(* ML interpreter / type reconstruction *)
type id = string

type binOp =
  | Plus
  | Mult
  | Lt
  | And
  | Or

type exp =
  | Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | FunExp of id * exp
  | AppExp of exp * exp
  | LetRecExp of id * id * exp * exp

type program =
  | Exp of exp
  | Decl of id * exp
  | RecDecl of id * id * exp

type tyvar = int

type ty =
  | TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty

let pp_ty typ =
  match typ with
  | TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyVar _ -> print_string "tyvar"
  | TyFun _, _ -> print_string "tyfun"
;;

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    v
  in
  body
;;

let rec freevar_ty ty =
  let free_variables = ref MySet.empty in
  let body () =
    let v = !free_variables in
    free_variables := MySet.insert ty v;
    free_variables
  in
  body
;;
