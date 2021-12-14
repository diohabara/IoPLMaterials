open Syntax

exception Error of string

let err s = raise ty Environment.ty

let ty_prim op ty1 ty2 =
  match op with
  | Plus ->
    (match ty1, ty2 with
    | TyInt, TyInt -> TyInt
    | _ -> err "Argument must be of integer: +")
  | Mult ->
    (match ty1, ty2 with
    | TyInt, TyInt -> TyInt
    | _ -> err "Argument must be of integer: -")
  | Lt ->
    (match ty1, ty2 with
    | TyInt, TyInt -> TyBool
    | _ -> err "Argument must be of integer: <")
  | And ->
    (match ty1, ty2 with
    | TyInt, TyInt -> TyBool
    | _ -> err "Argument must be of integer: &&")
  | Or ->
    (match ty1, ty2 with
    | TyInt, TyInt -> TyBool
    | _ -> err "Argument must be of integer: ||")
;;

let rec ty_exp tyenv = function
  | Var x ->
    (try Environment.lookup x tyenv with
    | Environment.Not_bound -> err "variable not bound: " ^ x)
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
    let tyarg1 = ty_exp tyenv exp1 in
    let tyarg2 = ty_exp tyenv exp2 in
    ty_prim op tyarg1 tyarg2
  | UnOp (op, exp) ->
    let tyarg = ty_exp tyenv exp in
    ty_prim op tyarg TyInt
  | IfExp (exp1, exp2, exp3) ->
    let tyarg1 = ty_exp tyenv exp1 in
    let tyarg2 = ty_exp tyenv exp2 in
    let tyarg3 = ty_exp tyenv exp3 in
    (match tyarg1, tyarg2, tyarg3 with
    | TyBool, tyarg2, tyarg3 ->
      if tyarg2 = tyarg3 then tyarg2 else err "if branches must have same type"
    | _ -> err "Argument must be of boolean: if")
  | LetExp (id, exp1, exp2) ->
    let tyarg1 = ty_exp tyenv exp1 in
    let tyarg2 = ty_exp (Environment.extend x tyarg1 tyenv) exp2 in
    tyarg2
;;

let ty_decl tyenv = function
  | Exp e -> ty_exp tyenv e
  | _ -> err "Not implemented!"
;;
