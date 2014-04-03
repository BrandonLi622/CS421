functor extraLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : extra_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* Brandon Li
   CPSC 421
   Assignment 3 *)

(* TODO: Remember to link sources.cm to the correct file *)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\160\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\002\000\008\000\003\000\007\000\004\000\006\000\005\000\005\000\
\\006\000\004\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\003\000\033\000\004\000\032\000\005\000\031\000\006\000\030\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\002\000\028\000\004\000\027\000\005\000\026\000\006\000\025\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\002\000\023\000\003\000\022\000\005\000\021\000\006\000\020\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\002\000\018\000\003\000\017\000\004\000\016\000\006\000\015\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\003\000\013\000\004\000\012\000\005\000\011\000\006\000\010\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\000\000\
\\195\000\004\000\049\000\005\000\048\000\006\000\047\000\000\000\
\\196\000\000\000\
\\197\000\000\000\
\\198\000\000\000\
\\199\000\003\000\045\000\005\000\044\000\006\000\043\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\202\000\000\000\
\\203\000\003\000\041\000\004\000\040\000\006\000\039\000\000\000\
\\204\000\000\000\
\\205\000\000\000\
\\206\000\000\000\
\\207\000\003\000\037\000\004\000\036\000\005\000\035\000\000\000\
\\208\000\000\000\
\\209\000\000\000\
\\210\000\000\000\
\\211\000\002\000\071\000\005\000\070\000\006\000\069\000\000\000\
\\212\000\000\000\
\\213\000\000\000\
\\214\000\000\000\
\\215\000\002\000\061\000\004\000\060\000\006\000\059\000\000\000\
\\216\000\000\000\
\\217\000\000\000\
\\218\000\000\000\
\\219\000\002\000\076\000\004\000\075\000\005\000\074\000\000\000\
\\220\000\000\000\
\\221\000\000\000\
\\222\000\000\000\
\\223\000\002\000\057\000\003\000\056\000\006\000\055\000\000\000\
\\224\000\000\000\
\\225\000\000\000\
\\226\000\000\000\
\\227\000\002\000\066\000\003\000\065\000\005\000\064\000\000\000\
\\228\000\000\000\
\\229\000\000\000\
\\230\000\000\000\
\\231\000\002\000\053\000\003\000\052\000\004\000\051\000\000\000\
\\232\000\000\000\
\\233\000\000\000\
\\234\000\005\000\104\000\006\000\103\000\000\000\
\\235\000\000\000\
\\236\000\000\000\
\\237\000\004\000\099\000\006\000\098\000\000\000\
\\238\000\000\000\
\\239\000\000\000\
\\240\000\004\000\092\000\005\000\091\000\000\000\
\\241\000\000\000\
\\242\000\000\000\
\\243\000\003\000\096\000\006\000\095\000\000\000\
\\244\000\000\000\
\\245\000\000\000\
\\246\000\003\000\089\000\005\000\088\000\000\000\
\\247\000\000\000\
\\248\000\000\000\
\\249\000\003\000\086\000\004\000\085\000\000\000\
\\250\000\000\000\
\\251\000\000\000\
\\252\000\002\000\118\000\006\000\117\000\000\000\
\\253\000\000\000\
\\254\000\000\000\
\\255\000\002\000\126\000\005\000\125\000\000\000\
\\000\001\000\000\
\\001\001\000\000\
\\002\001\002\000\113\000\004\000\112\000\000\000\
\\003\001\000\000\
\\004\001\000\000\
\\005\001\002\000\110\000\003\000\109\000\000\000\
\\006\001\000\000\
\\007\001\006\000\145\000\000\000\
\\008\001\000\000\
\\009\001\005\000\140\000\000\000\
\\010\001\000\000\
\\011\001\004\000\137\000\000\000\
\\012\001\000\000\
\\013\001\003\000\135\000\000\000\
\\014\001\000\000\
\\015\001\002\000\151\000\000\000\
\"
val actionRowNumbers =
"\007\000\001\000\032\000\027\000\
\\022\000\017\000\012\000\006\000\
\\048\000\044\000\040\000\036\000\
\\005\000\072\000\064\000\056\000\
\\044\000\004\000\068\000\064\000\
\\052\000\040\000\003\000\060\000\
\\056\000\052\000\036\000\002\000\
\\048\000\044\000\040\000\036\000\
\\031\000\090\000\087\000\081\000\
\\030\000\090\000\084\000\078\000\
\\029\000\087\000\084\000\075\000\
\\028\000\081\000\078\000\075\000\
\\026\000\102\000\099\000\090\000\
\\025\000\102\000\093\000\084\000\
\\024\000\099\000\093\000\078\000\
\\023\000\021\000\102\000\096\000\
\\087\000\020\000\019\000\096\000\
\\093\000\075\000\018\000\016\000\
\\099\000\096\000\081\000\015\000\
\\014\000\013\000\011\000\010\000\
\\009\000\008\000\047\000\110\000\
\\108\000\046\000\110\000\106\000\
\\045\000\108\000\106\000\043\000\
\\042\000\110\000\104\000\041\000\
\\108\000\104\000\039\000\038\000\
\\037\000\106\000\104\000\035\000\
\\034\000\033\000\071\000\112\000\
\\110\000\070\000\112\000\108\000\
\\069\000\063\000\062\000\112\000\
\\104\000\061\000\055\000\054\000\
\\053\000\067\000\066\000\112\000\
\\106\000\065\000\051\000\050\000\
\\049\000\059\000\058\000\057\000\
\\089\000\109\000\088\000\107\000\
\\086\000\085\000\105\000\080\000\
\\079\000\083\000\082\000\103\000\
\\077\000\076\000\074\000\073\000\
\\101\000\111\000\100\000\098\000\
\\097\000\092\000\091\000\095\000\
\\094\000\000\000"
val gotoT =
"\
\\031\000\001\000\032\000\157\000\000\000\
\\000\000\
\\005\000\007\000\000\000\
\\004\000\012\000\000\000\
\\003\000\017\000\000\000\
\\002\000\022\000\000\000\
\\001\000\027\000\000\000\
\\000\000\
\\009\000\032\000\000\000\
\\008\000\036\000\000\000\
\\007\000\040\000\000\000\
\\006\000\044\000\000\000\
\\000\000\
\\015\000\048\000\000\000\
\\013\000\052\000\000\000\
\\011\000\056\000\000\000\
\\008\000\060\000\000\000\
\\000\000\
\\014\000\061\000\000\000\
\\013\000\065\000\000\000\
\\010\000\066\000\000\000\
\\007\000\070\000\000\000\
\\000\000\
\\012\000\071\000\000\000\
\\011\000\075\000\000\000\
\\010\000\076\000\000\000\
\\006\000\077\000\000\000\
\\000\000\
\\009\000\078\000\000\000\
\\008\000\079\000\000\000\
\\007\000\080\000\000\000\
\\006\000\081\000\000\000\
\\000\000\
\\021\000\082\000\000\000\
\\020\000\085\000\000\000\
\\018\000\088\000\000\000\
\\000\000\
\\021\000\091\000\000\000\
\\019\000\092\000\000\000\
\\017\000\095\000\000\000\
\\000\000\
\\020\000\098\000\000\000\
\\019\000\099\000\000\000\
\\016\000\100\000\000\000\
\\000\000\
\\018\000\103\000\000\000\
\\017\000\104\000\000\000\
\\016\000\105\000\000\000\
\\000\000\
\\025\000\106\000\000\000\
\\024\000\109\000\000\000\
\\021\000\112\000\000\000\
\\000\000\
\\025\000\113\000\000\000\
\\022\000\114\000\000\000\
\\019\000\117\000\000\000\
\\000\000\
\\024\000\118\000\000\000\
\\022\000\119\000\000\000\
\\017\000\120\000\000\000\
\\000\000\
\\000\000\
\\025\000\121\000\000\000\
\\023\000\122\000\000\000\
\\020\000\125\000\000\000\
\\000\000\
\\000\000\
\\023\000\126\000\000\000\
\\022\000\127\000\000\000\
\\016\000\128\000\000\000\
\\000\000\
\\000\000\
\\024\000\129\000\000\000\
\\023\000\130\000\000\000\
\\018\000\131\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\029\000\132\000\000\000\
\\028\000\134\000\000\000\
\\000\000\
\\029\000\136\000\000\000\
\\027\000\137\000\000\000\
\\000\000\
\\028\000\139\000\000\000\
\\027\000\140\000\000\000\
\\000\000\
\\000\000\
\\029\000\141\000\000\000\
\\026\000\142\000\000\000\
\\000\000\
\\028\000\144\000\000\000\
\\026\000\145\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\027\000\146\000\000\000\
\\026\000\147\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\030\000\148\000\000\000\
\\029\000\150\000\000\000\
\\000\000\
\\030\000\151\000\000\000\
\\028\000\152\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\030\000\153\000\000\000\
\\026\000\154\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\030\000\155\000\000\000\
\\027\000\156\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 158
val numrules = 112
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
end
type svalue = MlyValue.svalue
type result = unit
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
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "W1"
  | (T 2) => "W2"
  | (T 3) => "W3"
  | (T 4) => "W4"
  | (T 5) => "W5"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 31, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.ntVOID p11, _, p11right)) :: ( _, ( _, 
W11left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p11 = p11 ()
 in ()
end; ()))
 in ( LrTable.NT 30, ( result, W11left, p11right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.ntVOID p21, _, p21right)) :: ( _, ( _, 
W21left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p21 = p21 ()
 in ()
end; ()))
 in ( LrTable.NT 30, ( result, W21left, p21right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.ntVOID p31, _, p31right)) :: ( _, ( _, 
W31left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p31 = p31 ()
 in ()
end; ()))
 in ( LrTable.NT 30, ( result, W31left, p31right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ntVOID p41, _, p41right)) :: ( _, ( _, 
W41left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p41 = p41 ()
 in ()
end; ()))
 in ( LrTable.NT 30, ( result, W41left, p41right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ntVOID p51, _, p51right)) :: ( _, ( _, 
W51left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p51 = p51 ()
 in ()
end; ()))
 in ( LrTable.NT 30, ( result, W51left, p51right), rest671)
end
|  ( 6, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 30, ( result, defaultPos, defaultPos), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ntVOID p121, _, p121right)) :: ( _, ( _, 
W21left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p121 = p121 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, W21left, p121right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ntVOID p131, _, p131right)) :: ( _, ( _, 
W31left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p131 = p131 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, W31left, p131right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ntVOID p141, _, p141right)) :: ( _, ( _, 
W41left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p141 = p141 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, W41left, p141right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ntVOID p151, _, p151right)) :: ( _, ( _, 
W51left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p151 = p151 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, W51left, p151right), rest671)
end
|  ( 11, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 0, ( result, defaultPos, defaultPos), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ntVOID p121, _, p121right)) :: ( _, ( _, 
W11left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p121 = p121 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, W11left, p121right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ntVOID p231, _, p231right)) :: ( _, ( _, 
W31left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p231 = p231 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, W31left, p231right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ntVOID p241, _, p241right)) :: ( _, ( _, 
W41left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p241 = p241 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, W41left, p241right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ntVOID p251, _, p251right)) :: ( _, ( _, 
W51left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p251 = p251 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, W51left, p251right), rest671)
end
|  ( 16, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ntVOID p131, _, p131right)) :: ( _, ( _, 
W11left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p131 = p131 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, W11left, p131right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.ntVOID p231, _, p231right)) :: ( _, ( _, 
W21left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p231 = p231 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, W21left, p231right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.ntVOID p341, _, p341right)) :: ( _, ( _, 
W41left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p341 = p341 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, W41left, p341right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.ntVOID p351, _, p351right)) :: ( _, ( _, 
W51left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p351 = p351 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, W51left, p351right), rest671)
end
|  ( 21, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 22, ( ( _, ( MlyValue.ntVOID p141, _, p141right)) :: ( _, ( _, 
W11left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p141 = p141 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, W11left, p141right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ntVOID p241, _, p241right)) :: ( _, ( _, 
W21left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p241 = p241 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, W21left, p241right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.ntVOID p341, _, p341right)) :: ( _, ( _, 
W31left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p341 = p341 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, W31left, p341right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.ntVOID p451, _, p451right)) :: ( _, ( _, 
W51left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p451 = p451 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, W51left, p451right), rest671)
end
|  ( 26, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 27, ( ( _, ( MlyValue.ntVOID p121, _, p121right)) :: ( _, ( _, 
W21left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p121 = p121 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, W21left, p121right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ntVOID p131, _, p131right)) :: ( _, ( _, 
W31left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p131 = p131 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, W31left, p131right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.ntVOID p141, _, p141right)) :: ( _, ( _, 
W41left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p141 = p141 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, W41left, p141right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.ntVOID p151, _, p151right)) :: ( _, ( _, 
W51left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p151 = p151 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, W51left, p151right), rest671)
end
|  ( 31, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 32, ( ( _, ( MlyValue.ntVOID p1231, _, p1231right)) :: ( _, ( _, 
W31left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1231 = p1231 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, W31left, p1231right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.ntVOID p1241, _, p1241right)) :: ( _, ( _, 
W41left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1241 = p1241 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, W41left, p1241right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.ntVOID p1251, _, p1251right)) :: ( _, ( _, 
W51left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1251 = p1251 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, W51left, p1251right), rest671)
end
|  ( 35, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 36, ( ( _, ( MlyValue.ntVOID p1231, _, p1231right)) :: ( _, ( _, 
W21left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1231 = p1231 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, W21left, p1231right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.ntVOID p1341, _, p1341right)) :: ( _, ( _, 
W41left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1341 = p1341 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, W41left, p1341right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.ntVOID p1351, _, p1351right)) :: ( _, ( _, 
W51left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1351 = p1351 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, W51left, p1351right), rest671)
end
|  ( 39, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 40, ( ( _, ( MlyValue.ntVOID p1241, _, p1241right)) :: ( _, ( _, 
W21left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1241 = p1241 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, W21left, p1241right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.ntVOID p1341, _, p1341right)) :: ( _, ( _, 
W31left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1341 = p1341 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, W31left, p1341right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.ntVOID p1451, _, p1451right)) :: ( _, ( _, 
W51left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1451 = p1451 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, W51left, p1451right), rest671)
end
|  ( 43, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 44, ( ( _, ( MlyValue.ntVOID p1251, _, p1251right)) :: ( _, ( _, 
W21left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1251 = p1251 ()
 in ()
end; ()))
 in ( LrTable.NT 8, ( result, W21left, p1251right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.ntVOID p1351, _, p1351right)) :: ( _, ( _, 
W31left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1351 = p1351 ()
 in ()
end; ()))
 in ( LrTable.NT 8, ( result, W31left, p1351right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.ntVOID p1451, _, p1451right)) :: ( _, ( _, 
W41left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1451 = p1451 ()
 in ()
end; ()))
 in ( LrTable.NT 8, ( result, W41left, p1451right), rest671)
end
|  ( 47, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 48, ( ( _, ( MlyValue.ntVOID p1231, _, p1231right)) :: ( _, ( _, 
W11left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1231 = p1231 ()
 in ()
end; ()))
 in ( LrTable.NT 9, ( result, W11left, p1231right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.ntVOID p2341, _, p2341right)) :: ( _, ( _, 
W41left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p2341 = p2341 ()
 in ()
end; ()))
 in ( LrTable.NT 9, ( result, W41left, p2341right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.ntVOID p2351, _, p2351right)) :: ( _, ( _, 
W51left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p2351 = p2351 ()
 in ()
end; ()))
 in ( LrTable.NT 9, ( result, W51left, p2351right), rest671)
end
|  ( 51, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 52, ( ( _, ( MlyValue.ntVOID p1241, _, p1241right)) :: ( _, ( _, 
W11left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1241 = p1241 ()
 in ()
end; ()))
 in ( LrTable.NT 10, ( result, W11left, p1241right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.ntVOID p2341, _, p2341right)) :: ( _, ( _, 
W31left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p2341 = p2341 ()
 in ()
end; ()))
 in ( LrTable.NT 10, ( result, W31left, p2341right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.ntVOID p2451, _, p2451right)) :: ( _, ( _, 
W51left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p2451 = p2451 ()
 in ()
end; ()))
 in ( LrTable.NT 10, ( result, W51left, p2451right), rest671)
end
|  ( 55, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 56, ( ( _, ( MlyValue.ntVOID p1251, _, p1251right)) :: ( _, ( _, 
W11left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1251 = p1251 ()
 in ()
end; ()))
 in ( LrTable.NT 11, ( result, W11left, p1251right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.ntVOID p2351, _, p2351right)) :: ( _, ( _, 
W31left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p2351 = p2351 ()
 in ()
end; ()))
 in ( LrTable.NT 11, ( result, W31left, p2351right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.ntVOID p2451, _, p2451right)) :: ( _, ( _, 
W41left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p2451 = p2451 ()
 in ()
end; ()))
 in ( LrTable.NT 11, ( result, W41left, p2451right), rest671)
end
|  ( 59, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 11, ( result, defaultPos, defaultPos), rest671)
end
|  ( 60, ( ( _, ( MlyValue.ntVOID p1341, _, p1341right)) :: ( _, ( _, 
W11left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1341 = p1341 ()
 in ()
end; ()))
 in ( LrTable.NT 12, ( result, W11left, p1341right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.ntVOID p2341, _, p2341right)) :: ( _, ( _, 
W21left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p2341 = p2341 ()
 in ()
end; ()))
 in ( LrTable.NT 12, ( result, W21left, p2341right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.ntVOID p3451, _, p3451right)) :: ( _, ( _, 
W51left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p3451 = p3451 ()
 in ()
end; ()))
 in ( LrTable.NT 12, ( result, W51left, p3451right), rest671)
end
|  ( 63, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 12, ( result, defaultPos, defaultPos), rest671)
end
|  ( 64, ( ( _, ( MlyValue.ntVOID p1351, _, p1351right)) :: ( _, ( _, 
W11left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1351 = p1351 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, W11left, p1351right), rest671)
end
|  ( 65, ( ( _, ( MlyValue.ntVOID p2351, _, p2351right)) :: ( _, ( _, 
W21left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p2351 = p2351 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, W21left, p2351right), rest671)
end
|  ( 66, ( ( _, ( MlyValue.ntVOID p3451, _, p3451right)) :: ( _, ( _, 
W41left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p3451 = p3451 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, W41left, p3451right), rest671)
end
|  ( 67, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 13, ( result, defaultPos, defaultPos), rest671)
end
|  ( 68, ( ( _, ( MlyValue.ntVOID p1451, _, p1451right)) :: ( _, ( _, 
W11left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p1451 = p1451 ()
 in ()
end; ()))
 in ( LrTable.NT 14, ( result, W11left, p1451right), rest671)
end
|  ( 69, ( ( _, ( MlyValue.ntVOID p2451, _, p2451right)) :: ( _, ( _, 
W21left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p2451 = p2451 ()
 in ()
end; ()))
 in ( LrTable.NT 14, ( result, W21left, p2451right), rest671)
end
|  ( 70, ( ( _, ( MlyValue.ntVOID p3451, _, p3451right)) :: ( _, ( _, 
W31left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p3451 = p3451 ()
 in ()
end; ()))
 in ( LrTable.NT 14, ( result, W31left, p3451right), rest671)
end
|  ( 71, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 14, ( result, defaultPos, defaultPos), rest671)
end
|  ( 72, ( ( _, ( MlyValue.ntVOID p12341, _, p12341right)) :: ( _, ( _
, W41left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p12341 = p12341 ()
 in ()
end; ()))
 in ( LrTable.NT 15, ( result, W41left, p12341right), rest671)
end
|  ( 73, ( ( _, ( MlyValue.ntVOID p12351, _, p12351right)) :: ( _, ( _
, W51left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p12351 = p12351 ()
 in ()
end; ()))
 in ( LrTable.NT 15, ( result, W51left, p12351right), rest671)
end
|  ( 74, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 15, ( result, defaultPos, defaultPos), rest671)
end
|  ( 75, ( ( _, ( MlyValue.ntVOID p12341, _, p12341right)) :: ( _, ( _
, W31left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p12341 = p12341 ()
 in ()
end; ()))
 in ( LrTable.NT 16, ( result, W31left, p12341right), rest671)
end
|  ( 76, ( ( _, ( MlyValue.ntVOID p12451, _, p12451right)) :: ( _, ( _
, W51left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p12451 = p12451 ()
 in ()
end; ()))
 in ( LrTable.NT 16, ( result, W51left, p12451right), rest671)
end
|  ( 77, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 16, ( result, defaultPos, defaultPos), rest671)
end
|  ( 78, ( ( _, ( MlyValue.ntVOID p12351, _, p12351right)) :: ( _, ( _
, W31left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p12351 = p12351 ()
 in ()
end; ()))
 in ( LrTable.NT 17, ( result, W31left, p12351right), rest671)
end
|  ( 79, ( ( _, ( MlyValue.ntVOID p12451, _, p12451right)) :: ( _, ( _
, W41left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p12451 = p12451 ()
 in ()
end; ()))
 in ( LrTable.NT 17, ( result, W41left, p12451right), rest671)
end
|  ( 80, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 17, ( result, defaultPos, defaultPos), rest671)
end
|  ( 81, ( ( _, ( MlyValue.ntVOID p12341, _, p12341right)) :: ( _, ( _
, W21left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p12341 = p12341 ()
 in ()
end; ()))
 in ( LrTable.NT 18, ( result, W21left, p12341right), rest671)
end
|  ( 82, ( ( _, ( MlyValue.ntVOID p13451, _, p13451right)) :: ( _, ( _
, W51left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p13451 = p13451 ()
 in ()
end; ()))
 in ( LrTable.NT 18, ( result, W51left, p13451right), rest671)
end
|  ( 83, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 18, ( result, defaultPos, defaultPos), rest671)
end
|  ( 84, ( ( _, ( MlyValue.ntVOID p12351, _, p12351right)) :: ( _, ( _
, W21left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p12351 = p12351 ()
 in ()
end; ()))
 in ( LrTable.NT 19, ( result, W21left, p12351right), rest671)
end
|  ( 85, ( ( _, ( MlyValue.ntVOID p13451, _, p13451right)) :: ( _, ( _
, W41left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p13451 = p13451 ()
 in ()
end; ()))
 in ( LrTable.NT 19, ( result, W41left, p13451right), rest671)
end
|  ( 86, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 19, ( result, defaultPos, defaultPos), rest671)
end
|  ( 87, ( ( _, ( MlyValue.ntVOID p12451, _, p12451right)) :: ( _, ( _
, W21left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p12451 = p12451 ()
 in ()
end; ()))
 in ( LrTable.NT 20, ( result, W21left, p12451right), rest671)
end
|  ( 88, ( ( _, ( MlyValue.ntVOID p13451, _, p13451right)) :: ( _, ( _
, W31left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p13451 = p13451 ()
 in ()
end; ()))
 in ( LrTable.NT 20, ( result, W31left, p13451right), rest671)
end
|  ( 89, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 20, ( result, defaultPos, defaultPos), rest671)
end
|  ( 90, ( ( _, ( MlyValue.ntVOID p12341, _, p12341right)) :: ( _, ( _
, W11left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p12341 = p12341 ()
 in ()
end; ()))
 in ( LrTable.NT 21, ( result, W11left, p12341right), rest671)
end
|  ( 91, ( ( _, ( MlyValue.ntVOID p23451, _, p23451right)) :: ( _, ( _
, W51left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p23451 = p23451 ()
 in ()
end; ()))
 in ( LrTable.NT 21, ( result, W51left, p23451right), rest671)
end
|  ( 92, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 21, ( result, defaultPos, defaultPos), rest671)
end
|  ( 93, ( ( _, ( MlyValue.ntVOID p12351, _, p12351right)) :: ( _, ( _
, W11left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p12351 = p12351 ()
 in ()
end; ()))
 in ( LrTable.NT 22, ( result, W11left, p12351right), rest671)
end
|  ( 94, ( ( _, ( MlyValue.ntVOID p23451, _, p23451right)) :: ( _, ( _
, W41left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p23451 = p23451 ()
 in ()
end; ()))
 in ( LrTable.NT 22, ( result, W41left, p23451right), rest671)
end
|  ( 95, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 22, ( result, defaultPos, defaultPos), rest671)
end
|  ( 96, ( ( _, ( MlyValue.ntVOID p12451, _, p12451right)) :: ( _, ( _
, W11left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p12451 = p12451 ()
 in ()
end; ()))
 in ( LrTable.NT 23, ( result, W11left, p12451right), rest671)
end
|  ( 97, ( ( _, ( MlyValue.ntVOID p23451, _, p23451right)) :: ( _, ( _
, W31left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p23451 = p23451 ()
 in ()
end; ()))
 in ( LrTable.NT 23, ( result, W31left, p23451right), rest671)
end
|  ( 98, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 23, ( result, defaultPos, defaultPos), rest671)
end
|  ( 99, ( ( _, ( MlyValue.ntVOID p13451, _, p13451right)) :: ( _, ( _
, W11left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  p13451 = p13451 ()
 in ()
end; ()))
 in ( LrTable.NT 24, ( result, W11left, p13451right), rest671)
end
|  ( 100, ( ( _, ( MlyValue.ntVOID p23451, _, p23451right)) :: ( _, (
 _, W21left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  p23451 = p23451 ()
 in ()
end; ()))
 in ( LrTable.NT 24, ( result, W21left, p23451right), rest671)
end
|  ( 101, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ()
)
 in ( LrTable.NT 24, ( result, defaultPos, defaultPos), rest671)
end
|  ( 102, ( ( _, ( _, W51left, W51right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 25, ( result, W51left, W51right), rest671)
end
|  ( 103, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ()
)
 in ( LrTable.NT 25, ( result, defaultPos, defaultPos), rest671)
end
|  ( 104, ( ( _, ( _, W41left, W41right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 26, ( result, W41left, W41right), rest671)
end
|  ( 105, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ()
)
 in ( LrTable.NT 26, ( result, defaultPos, defaultPos), rest671)
end
|  ( 106, ( ( _, ( _, W31left, W31right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 27, ( result, W31left, W31right), rest671)
end
|  ( 107, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ()
)
 in ( LrTable.NT 27, ( result, defaultPos, defaultPos), rest671)
end
|  ( 108, ( ( _, ( _, W21left, W21right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 28, ( result, W21left, W21right), rest671)
end
|  ( 109, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ()
)
 in ( LrTable.NT 28, ( result, defaultPos, defaultPos), rest671)
end
|  ( 110, ( ( _, ( _, W11left, W11right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 29, ( result, W11left, W11right), rest671)
end
|  ( 111, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ()
)
 in ( LrTable.NT 29, ( result, defaultPos, defaultPos), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : extra_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun W1 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun W2 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun W3 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun W4 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun W5 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
end
end
