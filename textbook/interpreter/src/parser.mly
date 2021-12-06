%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT
%token IF THEN ELSE TRUE FALSE
%token AND OR
%token LET IN EQ
%token RARROW FUN
%token REC
%token EOF

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
  | e=Expr SEMISEMI { Exp e }
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }
  | LET REC x=ID EQ FUN p=ID RARROW e=Expr SEMISEMI { RecDecl (x, p, e) }

Expr :
  | e=IfExpr { e }
  | e=LTExpr { e }
  | e=LExpr { e }
  | e=LETExpr { e }
  | e=FunExpr { e }
  | e=LetRecExpr { e }

LetRecExpr:
  | LET REC i=ID EQ FUN p=ID RARROW e1=Expr IN e2=Expr { LetRecExp(i, p, e1, e2) }

FunExpr:
  | FUN i=ID RARROW e=Expr { FunExp(i, e) }

LETExpr :
  | LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }

LTExpr :
  | l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
  | l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr :
  | l=MExpr MULT r=AExpr { BinOp (Mult, l, r) }
  | e=AppExpr { e }

AppExpr:
  | e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }

LExpr:
  | l=AExpr AND r=AExpr { BinOp (And, l, r) }
  | l=AExpr OR r=AExpr { BinOp (Or, l, r) }

AExpr :
  | i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
  | IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }
