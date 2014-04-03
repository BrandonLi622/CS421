(* Brandon Li
   Assignment 2
   CPSC 421 *)

type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

(* Finds the numerical value of a character for error printing purposes *)
fun charNumberAsString(s) = Int.toString(Char.ord(hd(String.explode(s))));

(* TODO: Right now this can raise an exception... *)
fun intFromString(s, pos) = valOf(Int.fromString(s)) handle
                       Option => (ErrorMsg.error pos ("expected integer, received " ^ s); 0) ;

datatype adjust = INC | DEC;
datatype state  = INIT_STATE | COM_STATE | STR_STATE | ESC_STATE;

val commentDepth = ref 0;
val currentState = ref INIT_STATE;
val currentString = ref "";

fun updateCommentDepth(x) = if x = INC 
                            then commentDepth := (!commentDepth + 1)
                            else commentDepth := (!commentDepth - 1);

fun updateState(x) = currentState := x;

(* Returns the state in string form, used for debugging purposes *)
fun stateInStr(INIT_STATE) = "INIT_STATE"
  | stateInStr(COM_STATE)  = "COM_STATE"
  | stateInStr(STR_STATE)  = "STR_STATE"
  | stateInStr(ESC_STATE)  = "ESC_STATE";

(* Returns the character (in string form) of the code represented by \ddd *)
fun strFromCharCode(x,pos) = let
                                val ddd_str = substring(x,1,3);
                                val ddd_int = valOf(Int.fromString(ddd_str));
                             in
                               if ddd_int <= 255 andalso ddd_int >= 0 (*extended ascii is from [0,255]*)
                               then String.str(chr(ddd_int))
                               else (ErrorMsg.error pos ("invalid ascii code"); "")
                             end;

(* Returns the character (in string form) of the code represented by \^c *)
fun strFromCtrlChar(x) = let
                            val c = valOf(Char.fromString(substring(x,2,1)))
                         in
                            String.str(chr(ord(c) - 64)) (*subtracting 64 is defined behavior*)
                         end;

                  

(*TODO: How to use reset? *)
(*TODO: What counts as white space? --> Check this once more *)
(*TODO: I NEED TO RESET ALL OF MY VARIABLES MYSELF TOO, e.g. check all YYBEGIN *)
(*TODO: Make sure I count newlines inside of escape sequence *)
(*TODO: Check my line numbers *)
(*TODO: make sure i'm not throwing exceptions randomly*)
(*TODO: Check that my \ddd range is correct *)
(*TODO: Page feed is \012, NOT \014*)


(*BUGS: I seem to have problems with line numbering... *)
(*BUGS: in test4 it steals the first character from function and treats unction as an id
Apparently this happens if i use \f as an escape sequence !!! WATCH OUT FOR THIS *)


(*Note : Used escape state because function is too ugly *)
(*Note: for "\^a" I print STRING("a") instead of STRING("") and also for "A\900b" abc I print STRING("Ab") and ID(abc) *)
(*Note: when a new line appears in a string I just stop the string and print an error*)

(* DONE STUFF *)
(*TODO: what types of whitespace can go inside of a string  --> Apparently only single space characters *)
(*TODO: Can a comment start like /* have <STRING> "stuff in between the / and *  --> NO? *)
(*TODO: Do I need monads to do strict evaluation ? *)                         
(*TODO: What about */* or /*/ *)
(*TODO: Do we send errors or should we just be printing? *)
(*TODO: Do things get evaluated in order? *)
(*TODO: How to do printable characters? *)
(*TODO: Illegal characters or incorrect strings*)
(*TODO: newline handling for different states *)
(*TODO: is ddd supposed to be in hex or decimal? *)
(*TODO: What should we do for unterminated strings that then have a new line? *)
(*TODO: also . does NOT match new line characters *)
(*TODO: is DEL considered a printable character? *)
(*TODO: what characters can go inside of comments -> ANY character is okay *)
(*TODO: for unterminated strings do we still print out the token *)
(*TODO: Where can whitespace go in the initial state? -> Between any 2 tokens*)
(*TODO: what characters can go inside of strings i.e. what about */ in test e -> mine passes this *)
(*TODO: How to use reset? *)
(*TODO: Page feed is \012, NOT \014*)
(*TODO: make sure i'm not throwing exceptions randomly*)
(*TODO: I NEED TO RESET ALL OF MY VARIABLES MYSELF TOO, e.g. check all YYBEGIN *)
(*TODO: Check that my \ddd range is correct *)
(*TODO: What counts as white space? --> Check this once more *)
(*TODO: Make sure I count newlines inside of escape sequence *)
(*TODO: Check my line numbers *)
(*TODO: check extended ascii *)
(*TODO: Testing STRCHAR *)
(*TODO: Also handle the \n at end of file problem!*)
(*TODO: my string positions are off *)
(*TODO: what do we put as starting and ending positions of characters? the actual characters or only what would be printed? *)
(*TODO: Make sure handle integers that are too big!!!*)



(* STRCHAR = [^\000\001\002\003\004\005\006\007\008\009\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031\127]; *)


(*BUGS: I seem to have problems with line numbering... *)
(*BUGS: in test4 it steals the first character from function and treats unction as an id
Apparently this happens if i use \f as an escape sequence !!! WATCH OUT FOR THIS *)
(*Note: when a new line appears in a string I just stop the string and print an error*)
(*Note: test case in myTest2.tig where quote terminates the \f___f\ pattern *)


(*Note : Used escape state because function is too ugly *)
(*Note: for "\^a" I print STRING("a") instead of STRING("") and also for "A\900b" abc I print STRING("Ab") and ID(abc) *)


val eof = fn () => let val pos = hd(!linePos) 
                   in
                    (if !currentState = COM_STATE then ErrorMsg.error pos "unclosed comment"
                     else if !currentState = STR_STATE then ErrorMsg.error pos "unclosed string"
                     else if !currentState = ESC_STATE then ErrorMsg.error pos "unclosed string"
                     else ();               (* Must be INIT_STATE *)
                     ErrorMsg.reset();
                     currentState := INIT_STATE;
                     commentDepth := 0;
                     currentString := "";
                     Tokens.EOF(pos,pos))
                   end;


%% 
%s COMMENT STRING ESCAPE;

SPACE = [ \t\n];
DIGIT = [0-9];
ALPHA = [A-Za-z];
ALPNUM = {DIGIT}|{ALPHA};

ID  = {ALPHA}({ALPNUM}|_)*;
INT = {DIGIT}+;

STRCHAR = [^\000\001\002\003\004\005\006\007\008\009\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031\127];

DDD = \\{DIGIT}{3};
CTRL = \\\^[A-Z@\[\]\\\^_];

COMMSTART = \/\*;
COMMEND   = \*\/;


%%
<INITIAL> \n	    =>  (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT> \n	    =>  (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<STRING>  \n	    =>  (ErrorMsg.error yypos ("unclosed string"); YYBEGIN INITIAL; 
                         updateState(INIT_STATE); currentString := "";
                         lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<ESCAPE> "\n"       =>  (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

<INITIAL> {SPACE}   =>  (continue());

<INITIAL> while     =>  (Tokens.WHILE    (yypos,yypos+5));
<INITIAL> for       =>  (Tokens.FOR      (yypos,yypos+3));
<INITIAL> to        =>  (Tokens.TO       (yypos,yypos+2));
<INITIAL> break     =>  (Tokens.BREAK    (yypos,yypos+5));
<INITIAL> let       =>  (Tokens.LET      (yypos,yypos+3));
<INITIAL> in        =>  (Tokens.IN       (yypos,yypos+2));
<INITIAL> end       =>  (Tokens.END      (yypos,yypos+3));
<INITIAL> function  =>  (Tokens.FUNCTION (yypos,yypos+8));
<INITIAL> var  	    =>  (Tokens.VAR      (yypos,yypos+3));
<INITIAL> type      =>  (Tokens.TYPE     (yypos,yypos+4));
<INITIAL> array     =>  (Tokens.ARRAY    (yypos,yypos+5));
<INITIAL> if        =>  (Tokens.IF       (yypos,yypos+2));
<INITIAL> then      =>  (Tokens.THEN     (yypos,yypos+4));
<INITIAL> else      =>  (Tokens.ELSE     (yypos,yypos+4));
<INITIAL> do        =>  (Tokens.WHILE    (yypos,yypos+2));
<INITIAL> of        =>  (Tokens.OF       (yypos,yypos+2));
<INITIAL> nil       =>  (Tokens.NIL      (yypos,yypos+3));

<INITIAL> ","	    =>  (Tokens.COMMA    (yypos,yypos+1));
<INITIAL> ":"       =>  (Tokens.COLON    (yypos,yypos+1));
<INITIAL> ";"       =>  (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL> "."       =>  (Tokens.DOT      (yypos,yypos+1));
<INITIAL> "("       =>  (Tokens.LPAREN   (yypos,yypos+1));
<INITIAL> ")"       =>  (Tokens.RPAREN   (yypos,yypos+1));
<INITIAL> "["       =>  (Tokens.LBRACK   (yypos,yypos+1));
<INITIAL> "]"       =>  (Tokens.RBRACK   (yypos,yypos+1));
<INITIAL> "{"       =>  (Tokens.LBRACE   (yypos,yypos+1));
<INITIAL> "}"       =>  (Tokens.RBRACE   (yypos,yypos+1));
<INITIAL> ">="      =>  (Tokens.GE       (yypos,yypos+2));
<INITIAL> ">"       =>  (Tokens.GT       (yypos,yypos+1));
<INITIAL> "<="      =>  (Tokens.LE       (yypos,yypos+2));
<INITIAL> "<"       =>  (Tokens.LT       (yypos,yypos+1));
<INITIAL> "="       =>  (Tokens.EQ       (yypos,yypos+1));
<INITIAL> "<>"      =>  (Tokens.NEQ      (yypos,yypos+2));
<INITIAL> "+"       =>  (Tokens.PLUS     (yypos,yypos+1));
<INITIAL> "-"       =>  (Tokens.MINUS    (yypos,yypos+1));
<INITIAL> "*"       =>  (Tokens.TIMES    (yypos,yypos+1));
<INITIAL> "/"       =>  (Tokens.DIVIDE   (yypos,yypos+1));
<INITIAL> "|"       =>  (Tokens.OR       (yypos,yypos+1));
<INITIAL> "&"       =>  (Tokens.AND      (yypos,yypos+1));
<INITIAL> ":="      =>  (Tokens.ASSIGN   (yypos,yypos+2));

<INITIAL> {INT}     => (Tokens.INT(intFromString(yytext,yypos),yypos,yypos+size(yytext)));
<INITIAL> {ID}      => (Tokens.ID (yytext,yypos,yypos+size(yytext)));

<INITIAL> "\""      => (YYBEGIN STRING; updateState(STR_STATE); currentString := ""; continue());
<STRING>  "\""      => (YYBEGIN INITIAL; updateState(INIT_STATE); Tokens.STRING(!currentString,yypos,yypos+size(!currentString)));
<STRING> {DDD}      => (currentString := (!currentString ^ strFromCharCode(yytext,yypos)); continue());
<STRING> {STRCHAR}  => (currentString := (!currentString ^ yytext); continue());
<STRING> "\\\""     => (currentString := (!currentString ^ "\""); continue());
<STRING> "\\\\"     => (currentString := (!currentString ^ "\\"); continue());
<STRING> {CTRL}     => (currentString := (!currentString ^ strFromCtrlChar(yytext)); continue());

<STRING> "\\ "      => (YYBEGIN ESCAPE; updateState(ESC_STATE); continue());
<STRING> "\\\t"     => (YYBEGIN ESCAPE; updateState(ESC_STATE); continue());
<STRING> "\\\012"   => (YYBEGIN ESCAPE; updateState(ESC_STATE); continue());
<STRING> "\\\n"     => (YYBEGIN ESCAPE; updateState(ESC_STATE); lineNum := !lineNum+1; linePos := (yypos + 1) :: !linePos; continue());


<ESCAPE> [ \t\012]    => (continue());
<ESCAPE> "\\"         => (YYBEGIN STRING; updateState(STR_STATE); continue());


<INITIAL> {COMMSTART} =>  (YYBEGIN COMMENT; updateState(COM_STATE); updateCommentDepth(INC); continue());
<COMMENT> {COMMSTART} =>  (updateCommentDepth(INC); continue());
<COMMENT> {COMMEND}   =>  (updateCommentDepth(DEC); if !commentDepth = 0
                                                    then (YYBEGIN INITIAL; updateState(INIT_STATE); continue())
                                                    else continue());
                                                    
<STRING>  .  => (ErrorMsg.error yypos ("illegal character " ^ charNumberAsString(yytext) ^ " " ^ stateInStr(!currentState)); continue());                                                         
<COMMENT> .  => (continue());                                                    
<INITIAL> .  => (ErrorMsg.error yypos ("illegal character " ^ charNumberAsString(yytext) ^ " " ^ stateInStr(!currentState)); continue());
<ESCAPE>  .  => (YYBEGIN STRING; updateState(STR_STATE); ErrorMsg.error yypos ("improper use of \\f__f\\ pattern"); continue());

