type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

(* TODO: Just for testing *)
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

fun stateInStr(INIT_STATE) = "INIT_STATE"
  | stateInStr(COM_STATE)  = "COM_STATE"
  | stateInStr(STR_STATE)  = "STR_STATE"
  | stateInStr(ESC_STATE)  = "ESC_STATE";

(* TODO: NEED TO CATCH THE EXCEPTION...except the problem is that the exception is in the option!!! *)
fun strFromCharCode(x,pos) = let
                                val ddd_str = substring(x,1,3);
                                val ddd_int = valOf(Int.fromString(ddd_str));
                             in
                               if ddd_int <= 255 andalso ddd_int > 0
                               then String.str(chr(ddd_int))
                               else (ErrorMsg.error pos ("invalid ascii code"); "")
                             end;

fun strFromCtrlChar(x) = let
                            val c = valOf(Char.fromString(substring(x,2,1)))
                         in
                            String.str(chr(ord(c) - 64)) (*this is defined behavior*)
                         end;

                  
(*TODO: Can a comment start like /* have <STRING> "stuff in between the / and *? *)
(*TODO: Do I need monads to do strict evaluation ? *)
(*TODO: What about */* or /*/ *)
(*TODO: Do we send errors just be printing? *)
(*TODO: Do things get evaluated in order? *)
(*TODO: How to do printable characters? *)
(*TODO: Illegal characters or incorrect strings*)
(*TODO: How to use reset? *)
(*TODO: newline handling for different states *)
(*TODO: is ddd supposed to be in hex or decimal? *)

(* Note : Used escape state because function is too ugly *)

(*TODO: Make sure I count newlines inside of escape sequence *)

(*TODO: Check my line numbers *)
(*TODO: also . does NOT match new line characters *)

(*Note: for "\^a" I print STRING("a") instead of STRING("")
 and also for "A\900b" abc I print STRING("Ab") and ID(abc) *)
(*TODO: What should we do for unterminated strings that then have a new line? *)

(*TODO: What counts as white space? *)

(*Note: when a new line appears in a string I just stop the string and print an error*)

(*TODO: Are codes in oct or decimal? *)

(*TODO: I NEED TO RESET ALL OF MY VARIABLES MYSELF TOO, e.g. check all YYBEGIN *)
                            


val eof = fn () => let val pos = hd(!linePos) 
                   in
                    (if !currentState = COM_STATE then ErrorMsg.error pos "unclosed comment"
                     else if !currentState = STR_STATE then ErrorMsg.error pos "unclosed string"
                     else if !currentState = ESC_STATE then ErrorMsg.error pos "unclosed string"
                     else ();               (* Must be INIT_STATE *)
                     ErrorMsg.reset();
                     currentState := INIT_STATE;
                     Tokens.EOF(pos,pos))
                   end;


%% 
%s COMMENT STRING ESCAPE;

SPACE = [ \t\n\f];
DIGIT = [0-9];
ALPHA = [A-Za-z];
ALPNUM = {DIGIT}|{ALPHA};

ID = {ALPHA}({ALPNUM}|_)*;
INT = {DIGIT}+;

STRCHAR = [\032]|{ALPNUM};
DDD = \\{DIGIT}{3};
CTRL = \\\^[A-Z@\[\]\\\^_];

COMMSTART = \/\*;
COMMEND   = \*\/;


%%
<INITIAL> \n	   =>  (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT> \n	   =>  (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<STRING>  \n	   =>  (ErrorMsg.error yypos ("unclosed string"); YYBEGIN INITIAL; 
                        updateState(INIT_STATE); 
                        lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

<INITIAL> {SPACE}+ =>  (continue());

<INITIAL> while    =>  (Tokens.WHILE   (yypos,yypos+5));
<INITIAL> for      =>  (Tokens.FOR     (yypos,yypos+3));
<INITIAL> to       =>  (Tokens.TO      (yypos,yypos+2));
<INITIAL> break    =>  (Tokens.BREAK   (yypos,yypos+5));
<INITIAL> let      =>  (Tokens.LET     (yypos,yypos+3));
<INITIAL> in       =>  (Tokens.IN      (yypos,yypos+2));
<INITIAL> end      =>  (Tokens.END     (yypos,yypos+3));
<INITIAL> function =>  (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL> var  	   =>  (Tokens.VAR     (yypos,yypos+3));
<INITIAL> type     =>  (Tokens.TYPE    (yypos,yypos+4));
<INITIAL> array    =>  (Tokens.ARRAY   (yypos,yypos+5));
<INITIAL> if       =>  (Tokens.IF      (yypos,yypos+2));
<INITIAL> then     =>  (Tokens.THEN    (yypos,yypos+4));
<INITIAL> else     =>  (Tokens.ELSE    (yypos,yypos+4));
<INITIAL> do       =>  (Tokens.WHILE   (yypos,yypos+2));
<INITIAL> of       =>  (Tokens.OF      (yypos,yypos+2));
<INITIAL> nil      =>  (Tokens.NIL     (yypos,yypos+3));

<INITIAL> ","	   =>  (Tokens.COMMA    (yypos,yypos+1));
<INITIAL> ":"      =>  (Tokens.COLON    (yypos,yypos+1));
<INITIAL> ";"      =>  (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL> "."      =>  (Tokens.DOT      (yypos,yypos+1));

<INITIAL> "("       =>  (Tokens.LPAREN(yypos,yypos+1));
<INITIAL> ")"       =>  (Tokens.RPAREN(yypos,yypos+1));
<INITIAL> "["       =>  (Tokens.LBRACK(yypos,yypos+1));
<INITIAL> "]"       =>  (Tokens.RBRACK(yypos,yypos+1));
<INITIAL> "{"       =>  (Tokens.LBRACE(yypos,yypos+1));
<INITIAL> "}"       =>  (Tokens.RBRACE(yypos,yypos+1));

<INITIAL> ">="     =>  (Tokens.GE    (yypos,yypos+2));
<INITIAL> ">"      =>  (Tokens.GT    (yypos,yypos+1));
<INITIAL> "<="     =>  (Tokens.LE    (yypos,yypos+2));
<INITIAL> "<"      =>  (Tokens.LT    (yypos,yypos+1));
<INITIAL> "="      =>  (Tokens.EQ    (yypos,yypos+1));
<INITIAL> "<>"     =>  (Tokens.NEQ   (yypos,yypos+2));
<INITIAL> "+"      =>  (Tokens.PLUS  (yypos,yypos+1));
<INITIAL> "-"      =>  (Tokens.MINUS (yypos,yypos+1));
<INITIAL> "*"      =>  (Tokens.TIMES (yypos,yypos+1));
<INITIAL> "/"      =>  (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL> "|"      =>  (Tokens.OR    (yypos,yypos+1));
<INITIAL> "&"      =>  (Tokens.AND   (yypos,yypos+1));
<INITIAL> ":="     =>  (Tokens.ASSIGN(yypos,yypos+2));

<INITIAL> {INT}    => (Tokens.INT(intFromString(yytext,yypos),yypos,yypos+size(yytext)));
<INITIAL> {ID}     => (Tokens.ID (yytext,yypos,yypos+size(yytext)));

<INITIAL> "\""     => (YYBEGIN STRING; updateState(STR_STATE); currentString := ""; continue());
<STRING>  "\""     => (YYBEGIN INITIAL; updateState(INIT_STATE); Tokens.STRING(!currentString,yypos,yypos+size(!currentString)));
<STRING> {DDD}     => (currentString := (!currentString ^ strFromCharCode(yytext,yypos)); continue());
<STRING> {STRCHAR} => (currentString := (!currentString ^ yytext); continue());
<STRING> "\\n"     => (currentString := (!currentString ^ "\n"); continue());
<STRING> "\\t"     => (currentString := (!currentString ^ "\t"); continue());
<STRING> "\\\""    => (currentString := (!currentString ^ "\""); continue());
<STRING> "\\\\"    => (currentString := (!currentString ^ "\\"); continue());
<STRING> {CTRL}    => (currentString := (!currentString ^ strFromCtrlChar(yytext)); continue());

<STRING> "\\ "     => (YYBEGIN ESCAPE; updateState(ESC_STATE); continue());
<STRING> "\\\t"    => (YYBEGIN ESCAPE; updateState(ESC_STATE); continue());
<STRING> "\\\f"    => (YYBEGIN ESCAPE; updateState(ESC_STATE); continue());
<STRING> "\\\n"    => (YYBEGIN ESCAPE; updateState(ESC_STATE); lineNum := !lineNum+1; linePos := (yypos + 1) :: !linePos; continue());

<ESCAPE> "\n"      => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<ESCAPE> [ \t\f]   => (continue());
<ESCAPE> "\\"      => (YYBEGIN STRING; updateState(STR_STATE); continue());
<ESCAPE> .         => (YYBEGIN STRING; updateState(STR_STATE); ErrorMsg.error yypos ("improper use of \\f__f\\ pattern"); continue());


<INITIAL> {COMMSTART} =>  (YYBEGIN COMMENT; updateState(COM_STATE); updateCommentDepth(INC); continue());
<COMMENT> {COMMSTART} =>  (updateCommentDepth(INC); continue());
<COMMENT> {COMMEND}   =>  (updateCommentDepth(DEC); if !commentDepth = 0
                                                    then (YYBEGIN INITIAL; updateState(INIT_STATE))
                                                    else (); 
                                                    continue());
<COMMENT> .  => (continue());                                                    




.       => (ErrorMsg.error yypos ("illegal character " ^ charNumberAsString(yytext) ^ " " ^ stateInStr(!currentState)); continue());

