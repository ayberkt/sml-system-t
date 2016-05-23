functor ExpLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Exp_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure T = Term
structure TY = Type
(* structure TLC = TopLevelCommands *)
exception Parse of string

fun createLam (var : Var.t) (t : TY.t) (body : T.t) : T.t
  = T.$$(TermOps.Lam t, [(T.\\ (var, body))])

fun createArr (t1 : TY.t) (t2: TY.t) : TY.t =
  Type.$$(TypeOps.ARR, [t1, t2])

val nat = TY.$$(TypeOps.NAT, [])


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\009\000\000\000\
\\001\000\001\000\009\000\004\000\008\000\005\000\007\000\006\000\006\000\
\\007\000\005\000\000\000\
\\001\000\002\000\021\000\000\000\
\\001\000\003\000\024\000\011\000\023\000\000\000\
\\001\000\004\000\019\000\000\000\
\\001\000\005\000\029\000\000\000\
\\001\000\008\000\018\000\000\000\
\\001\000\009\000\022\000\000\000\
\\001\000\009\000\035\000\010\000\010\000\000\000\
\\001\000\010\000\010\000\000\000\
\\001\000\010\000\010\000\011\000\017\000\000\000\
\\001\000\010\000\010\000\011\000\032\000\000\000\
\\001\000\010\000\010\000\012\000\016\000\000\000\
\\001\000\010\000\010\000\013\000\037\000\000\000\
\\001\000\010\000\010\000\014\000\028\000\000\000\
\\001\000\010\000\011\000\000\000\
\\001\000\010\000\030\000\000\000\
\\001\000\015\000\033\000\000\000\
\\001\000\021\000\000\000\022\000\000\000\000\000\
\\039\000\000\000\
\\040\000\003\000\024\000\000\000\
\\041\000\000\000\
\\042\000\000\000\
\\043\000\010\000\010\000\000\000\
\\044\000\000\000\
\\045\000\010\000\010\000\000\000\
\\046\000\000\000\
\\047\000\000\000\
\"
val actionRowNumbers =
"\001\000\021\000\009\000\015\000\
\\001\000\001\000\022\000\027\000\
\\001\000\000\000\012\000\023\000\
\\010\000\006\000\004\000\026\000\
\\002\000\007\000\003\000\019\000\
\\001\000\001\000\002\000\014\000\
\\025\000\020\000\005\000\016\000\
\\001\000\011\000\017\000\001\000\
\\008\000\001\000\013\000\024\000\
\\018\000"
val gotoT =
"\
\\002\000\036\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\010\000\003\000\001\000\000\000\
\\002\000\011\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\012\000\003\000\001\000\000\000\
\\003\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\018\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\023\000\003\000\001\000\000\000\
\\002\000\024\000\003\000\001\000\000\000\
\\001\000\025\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\029\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\032\000\003\000\001\000\000\000\
\\000\000\
\\002\000\034\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 37
val numrules = 9
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | IDENT of unit ->  (string) | var of unit ->  (Var.t)
 | exp of unit ->  (T.t) | ty of unit ->  (TY.t)
end
type svalue = MlyValue.svalue
type result = T.t
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 21) => true | (T 20) => true | _ => false
val showTerminal =
fn (T 0) => "IDENT"
  | (T 1) => "NATTYPE"
  | (T 2) => "ARRTYPE"
  | (T 3) => "ZERO"
  | (T 4) => "SUCC"
  | (T 5) => "REC"
  | (T 6) => "LAMBDA"
  | (T 7) => "COLON"
  | (T 8) => "MAPSTO"
  | (T 9) => "LPAREN"
  | (T 10) => "RPAREN"
  | (T 11) => "LCURLY"
  | (T 12) => "RCURLY"
  | (T 13) => "BAR"
  | (T 14) => "WITH"
  | (T 15) => "END"
  | (T 16) => "STEP"
  | (T 17) => "EVAL"
  | (T 18) => "CHECKED"
  | (T 19) => "UNCHECKED"
  | (T 20) => "SEMI"
  | (T 21) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15)
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, NATTYPE1left, NATTYPE1right)) :: rest671)) => let
 val  result = MlyValue.ty (fn _ => (nat))
 in ( LrTable.NT 0, ( result, NATTYPE1left, NATTYPE1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.ty ty2, _, ty2right)) :: _ :: ( _, ( 
MlyValue.ty ty1, ty1left, _)) :: rest671)) => let val  result = 
MlyValue.ty (fn _ => let val  ty1 = ty1 ()
 val  ty2 = ty2 ()
 in ( createArr ty1 ty2 )
end)
 in ( LrTable.NT 0, ( result, ty1left, ty2right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.var var1, var1left, var1right)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  (var as var1) = 
var1 ()
 in (T.`` var)
end)
 in ( LrTable.NT 1, ( result, var1left, var1right), rest671)
end
|  ( 3, ( ( _, ( _, ZERO1left, ZERO1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => (T.$$(TermOps.Zero, [])))
 in ( LrTable.NT 1, ( result, ZERO1left, ZERO1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
SUCC1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  exp1 = exp1 ()
 in (T.$$(TermOps.Succ, [exp1]))
end)
 in ( LrTable.NT 1, ( result, SUCC1left, exp1right), rest671)
end
|  ( 5, ( ( _, ( _, _, RCURLY1right)) :: ( _, ( MlyValue.exp exp5, _,
 _)) :: _ :: ( _, ( MlyValue.exp exp4, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.exp exp3, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.exp exp2,
 _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _
, REC1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 val  exp4 = exp4 ()
 val  exp5 = exp5 ()
 in (T.$$(TermOps.Rec, [exp2, exp3, exp1]))
end)
 in ( LrTable.NT 1, ( result, REC1left, RCURLY1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ty ty1, _, _)) :: _ :: ( _, ( MlyValue.var var1, _, _)) :: _
 :: ( _, ( _, LAMBDA1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (var as var1) = var1 ()
 val  (ty as ty1) = ty1 ()
 val  (exp as exp1) = exp1 ()
 in ( createLam var ty exp )
end)
 in ( LrTable.NT 1, ( result, LAMBDA1left, exp1right), rest671)
end
|  ( 7, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp2, _,
 _)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (T.$$(TermOps.App, [exp1, exp2]))
end)
 in ( LrTable.NT 1, ( result, exp1left, RPAREN1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.IDENT IDENT1, IDENT1left, IDENT1right)) :: 
rest671)) => let val  result = MlyValue.var (fn _ => let val  (IDENT
 as IDENT1) = IDENT1 ()
 in (ParserState.getvar IDENT)
end)
 in ( LrTable.NT 2, ( result, IDENT1left, IDENT1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.exp x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Exp_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun IDENT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.IDENT (fn () => i),p1,p2))
fun NATTYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRTYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun ZERO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SUCC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun REC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun LAMBDA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun MAPSTO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LCURLY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RCURLY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun BAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun WITH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun STEP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun EVAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun CHECKED (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun UNCHECKED (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
end
end
