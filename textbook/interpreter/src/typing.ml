open Syntax

exception Error of string

type subst = (tyvar * ty) list

let rec subst_type (type_subst : subst) (ty : ty) =
  match ty with
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyVar v ->
    (match type_subst with
    | [] -> TyVar v
    | (v', t') :: rest -> if v = v' then t' else subst_type rest ty)
  | TyFun (t1, t2) -> TyFun (subst_type type_subst t1, subst_type type_subst t2)
;;

let err s = raise (Error s)

type tyenv = ty Environment.t

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
    | Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
    let tyarg1 = ty_exp tyenv exp1 in
    let tyarg2 = ty_exp tyenv exp2 in
    ty_prim op tyarg1 tyarg2
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
    let tyarg2 = ty_exp (Environment.extend id tyarg1 tyenv) exp2 in
    tyarg2
  | _ -> err "Not implemented!"
;;

let ty_decl tyenv = function
  | Exp e -> ty_exp tyenv e
  | _ -> err "Not implemented!"
;;
