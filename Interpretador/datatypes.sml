signature DATATYPES =
sig
  (* Expressoes *)
  datatype Id = Id of string
  and Lit = IntLit of string
          | RealLit of string
          | BoolLit of string
          | CharLit of string
          | StringLit of string
  and Exp = LitExp of Lit 
          | ArrExp of Exp list
          | OrExp of Exp * Exp
          | AndExp of Exp * Exp
          | NegExp of Exp
          | EqExp of Exp * Exp
          | NeExp of Exp * Exp
          | LtExp of Exp * Exp
          | LeExp of Exp * Exp
          | GtExp of Exp * Exp
          | GeExp of Exp * Exp
          | IdOrArrAccessExp of Id * (Exp list)
          | AssignExp of Id * (Exp list) * Exp
          | UMinusExp of Exp
          | AddExp of Exp * Exp
          | SubExp of Exp * Exp
          | MultExp of Exp * Exp
          | DivExp of Exp * Exp
          | ParenExp of Exp
          | AppExp of Id * Exp list
  (* Comandos *)
  and DecOrCmd = DecNotCmd of Dec
               | CmdNotDec of Cmd
  and Cmd = CompCmd of DecOrCmd list
          | ExpCmd of (Exp option)
          | SelCmd of Exp * Cmd * (Cmd option)
          | IterCmd of Exp * Cmd
          | JumpCmd of Exp option
          | Skip
  (* Declaracoes *)
  and TypeSpec = Void | Int | Real | Bool | Char | String | Dataset | Model
                | Array of TypeSpec
  and Dec = Dec of TypeSpec * Id * (Exp option)
  (* Definicoes *)
  and FunDef = FunDef of TypeSpec * Id * (Dec list) * Cmd
  (* Programa *)
  and DecOrFunDef = DecNotFunDef of Dec
                  | FunDefNotDec of FunDef
  and Prog = Prog of DecOrFunDef list
end;

structure DataTypes : DATATYPES =
struct
  (* Expressoes *)
  datatype Id = Id of string
  and Lit = IntLit of string
          | RealLit of string
          | BoolLit of string
          | CharLit of string
          | StringLit of string
  and Exp = LitExp of Lit 
          | ArrExp of Exp list
          | OrExp of Exp * Exp
          | AndExp of Exp * Exp
          | NegExp of Exp
          | EqExp of Exp * Exp
          | NeExp of Exp * Exp
          | LtExp of Exp * Exp
          | LeExp of Exp * Exp
          | GtExp of Exp * Exp
          | GeExp of Exp * Exp
          | IdOrArrAccessExp of Id * (Exp list)
          | AssignExp of Id * (Exp list) * Exp
          | UMinusExp of Exp
          | AddExp of Exp * Exp
          | SubExp of Exp * Exp
          | MultExp of Exp * Exp
          | DivExp of Exp * Exp
          | ParenExp of Exp
          | AppExp of Id * Exp list
  (* Comandos *)
  and DecOrCmd = DecNotCmd of Dec
               | CmdNotDec of Cmd
  and Cmd = CompCmd of DecOrCmd list
          | ExpCmd of (Exp option)
          | SelCmd of Exp * Cmd * (Cmd option)
          | IterCmd of Exp * Cmd
          | JumpCmd of Exp option
          | Skip
  (* Declaracoes *)
  and TypeSpec = Void | Int | Real | Bool | Char | String | Dataset | Model
                | Array of TypeSpec
  and Dec = Dec of TypeSpec * Id * (Exp option)
  (* Definicoes *)
  and FunDef = FunDef of TypeSpec * Id * (Dec list) * Cmd
  (* Programa *)
  and DecOrFunDef = DecNotFunDef of Dec
                  | FunDefNotDec of FunDef
  and Prog = Prog of DecOrFunDef list
end;

