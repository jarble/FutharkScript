/* lexical grammar */
%lex
%%

\s+                                   /* IGNORE */
"//".*                                /* IGNORE */
[/][*][^*]*[*]+([^/*][^*]*[*]+)*[/]   /* IGNORE */
[0-9]+("."[0-9]+)\b  return 'NUMBER'
[0-9]+\b             return 'INTEGER'
\"([^\\\"]|\\.)*\" return 'STRING_LITERAL'
"$"                   return "$"
"function"            return "function"
"continue"            return "continue"
"interface"           return "interface"
"export"              return 'export'
"private"             return 'private'
"public"              return 'public'
"extends"             return 'extends'
"typeof"              return "typeof"
"class"               return "class"
"static"              return "static"
"const"               return 'const'
"if"                  return 'if'
"new"                 return 'new'
"else"                return 'else'
"type"                return 'type'
"case"                return "case"
"default"             return 'default'
"return"              return 'return'
"yield"               return 'yield'
"while"               return 'while'
"switch"              return 'switch'
"break"               return 'break'
"for"                 return 'for'
"var"                 return 'var'
"let"                 return 'let'
"of"                  return 'of'
"Math"                return "Math"
"number"              return 'number'
"boolean"             return 'boolean'
"Number"              return 'Number'
"Array"               return 'Array'
","                   return ','
";"                   return ';'
"..."                 return '...'
"."                   return '.'
":"                   return ':'
"&&"                  return '&&'
"&"                   return '&'
"||"                  return '||'
"|"                   return '|'
">="                  return '>='
">"                   return '>'
"<="                  return '<='
"<"                   return '<'
"=>"                  return '=>'
"==="                 return '==='
"!=="                 return '!=='
"!"                   return "!"
"="                   return '='
"%"                   return '%'
"*="                  return '*='
"*"                   return '*'
"/="                  return '/='
"/"                   return '/'
"-="                  return '-='
"--"                  return '--'
"-"                   return '-'
"++"                  return '++'
"+="                  return '+='
"+"                   return '+'
"^"                   return '^'
"{"                   return '{'
"}"                   return '}'
"["                   return '['
"]"                   return ']'
"?"                   return '?'
"("                   return '('
")"                   return ')'
[a-zA-Z_][a-zA-Z0-9_]* return 'IDENTIFIER'
<<EOF>>               return 'EOF'
.                     return 'INVALID'

/lex

/* operator associations and precedence */

%right '?'

%left '...'
%left '||' '|'
%left '&&' '&'
%left '<' '<=' '>' '>=' '===' '!=='
%left '+' '-'
%left '*' '/' '%'
%left UMINUS

%start expressions

%% /* language grammar */

var_name: IDENTIFIER {$$ = "var_"+$1;};
module_name: IDENTIFIER {$$ = "Module_"+$1;};

expressions: top_level_statements EOF {return $1};

case_statement: "case" e ":" statement "break" ";" {$$ = ["case",$2," -> ",$4].join(" ")};
case_statements_: case_statement case_statements_ {$$ = $1+$2;} | case_statement {$$ =
 $1;};
case_statements: case_statements_ "default" ":" statement {$$ = $1+["default:",$4].join("");} | case_statements_;


access_modifier: "public" | "private";

top_level_statements: top_level_statements top_level_statement {$$ = $1+"\\n"+$2;} | top_level_statement {$$ =
 $1;} | create_var ";" {$$ = $1;};
 
statement
    :
    assign_var ";" statement {$$ = $1+" in "+$3;}
    | statement_
    ;

statement_:
	return_statement ";" {$$ = $1;}
    | "switch" "(" e ")" "{" case_statements "}" {$$ = ["match ",$3," ",$6].join("");}
    | if_statement
	;
	
top_level_statement:
	"function" func {$$ = $2;}
    | "class" module_name "{" class_statements "}" {$$ = "module "+$2+" = {"+$4+"}";}
    | "class" IDENTIFIER "<" class_type_parameters ">" "{" class_statements "}" {$$ = ["module ",$2," = {",$4," ",$7,"}"].join("");}
    | "class" module_name "extends" module_name "{" class_statements "}" {$$ = ["module ",$2," = {",$6," open ",$4,"}"].join("");}
    | "interface" module_name "{" class_statements "}" {$$ = "module type "+$2+" = {"+$4+"}";}
    | "interface" module_name "extends" module_name "{" class_statements "}" {$$ = ["module type ",$2," = {",$6," include ",$4,"}"].join("");}
    | "interface" IDENTIFIER "<" class_type_parameters ">" "{" class_statements "}" {$$ = ["module type ",$2," = {",$4," ",$7,"}"].join("");}
	;
	
func:
    var_name "(" parameters ")" ":" var_name "{" statement "}" {$$ = [$6,$1,"(",$3,")",$8].join(" ");}
    | var_name "(" parameters ")" "{" statement "}" {$$ = ["let",$1,$3,"=",$6].join(" ");}
    | var_name "<" type_parameters ">" "(" parameters ")" "{" statement "}" {$$ = ["let",$1,$3,$6," = ",$9].join(" ");}
    | var_name "<" type_parameters ">" "(" parameters ")" ":" var_name "{" statement "}" {$$ = ["let",$1,$3,$6,":",$9," = ",$11].join(" ");}
    ;

const_or_let: "const" | "let" | "var";

return_statement: 
"return" e  {$$ = $2;};

assign_var: 
   create_var
   | var_name "=" e {$$ = [$1,"=",$3].join(" ");}
   | var_name "++" {$$ = ["let",$1,"=",$1,"+1.0"].join(" ");}
   | var_name "--" {$$ = ["let",$1,"=",$1,"-1.0"].join(" ");}
   | var_name "+=" e {$$ = ["let",$1,"=",$1,"+",$3].join(" ");}
   | var_name "-=" e {$$ = ["let",$1,"=",$1,"-",$3].join(" ");}
   | var_name "*=" e {$$ = ["let",$1,"=",$1,"*",$3].join(" ");}
   | var_name "/=" e {$$ = ["let",$1,"=",$1,"/",$3].join(" ");}
   ;
   
create_var:
	const_or_let var_name ":" type "=" e {$$ = ["let ",$2,":",$4,"=",$6].join(" ");}
   | const_or_let var_name "=" e {$$ = "let "+$2+" = "+$4;}
   | "type" var_name "=" e {$$ = "type "+$2+" = "+$4;}
   ;

e
    :
     e "?" e ":" e {$$ = ["(if ",$1," then ",$3," else ",$5,")"].join("")}
    |e '||' e
        {$$ = [$1,$2,$3].join(" ");}
    |e '|' e
        {$$ = [$1,$2,$3].join(" ");}
    |e '&&' e
        {$$ = [$1,$2,$3].join(" ");}
    |e '&' e
        {$$ = [$1,$2,$3].join(" ");}
    |e '!==' e
        {$$ = [$1,'!=',$3].join(" ");}
    |e '===' e
        {$$ = [$1,'==',$3].join(" ");}
    |e '<=' e
        {$$ = [$1,$2,$3].join(" ");}
    |e '<' e
        {$$ = [$1,$2,$3].join(" ");}
    | e '>=' e
        {$$ = [$1,$2,$3].join(" ");}
    |e '>' e
        {$$ = [$1,$2,$3].join(" ");}
    | e '+' e
        {$$ = [$1,$2,$3].join(" ");}
    | e '-' e
        {$$ = [$1,$2,$3].join(" ");}
    | e '*' e
        {$$ = [$1,$2,$3].join(" ");}
    | e '/' e
        {$$ = [$1,$2,$3].join(" ");}
    | e '%' e
        {$$ = [$1,$2,$3].join(" ");}
    | '-' e %prec UMINUS
        {$$ = "-"+$2;}
    | not_expr
    ;

not_expr: "!" dot_expr {$$ = "!"+$2;} | dot_expr {$$ = $1;};


dot_expr: parentheses_expr  "." dot_expr  {$$ = $1+"."+$3;} | parentheses_expr {$$ =
 $1;};

access_array: parentheses_expr "[" e "]" {$$ = $1+"["+$3+"]";};


parentheses_expr:
    "function" "(" parameters ")" ":" type "{" statement "}" {$$ = ["(\\",$3,":",$6,"->",$8,")"].join("");}
    | "function" "(" parameters ")" "{" statement "}" {$$ = ["(\\",$3,"->",$6,")"].join("");}
    | var_name "(" ")" {$$= $1;} //call a function
    | "new" module_name "(" exprs ")" {$$= [$2,"(",$4,")"].join("");}
    | var_name "(" function_call_parameters ")" {$$= ["(",$1," ",$3,")"].join("");}
    | "Number" "(" exprs ")" {$$= ["float(",$3,")"].join("");}
    | "Math" "." var_name "(" e ")" {$$ = $3+"("+$5+")";}
    | "Math" "." var_name {
		if($3 == "E"){
			$$ = Math.E;
		}
		else if($3 == "LOG10E"){
			$$ = Math.LOG10E;
		}
		else if($3 == "SQRT1_2"){
			$$ = Math.SQRT1_2;
		}
		else if($3 == "SQRT2"){
			$$ = Math.SQRT2;
		}
		else if($3 == "PI"){
			$$ = Math.PI;
		}
	}
	| access_array
    | '(' e ')' {$$ = "("+$1+")";}
    | parentheses_expr_;

parentheses_expr_:
    "{" "}" {$$ = "{}";}
    | "{" key_values "}" {$$ = "{"+$2+"}";}
    | "[" "]" {$$ = "[]";}
    | "[" exprs "]" {$$ = "["+$2+"]";}
    | NUMBER
        {$$ = yytext;}
    | INTEGER
        {$$ = $1+".0";}
    | var_name
    | STRING_LITERAL
        {$$ = yytext;};

key_values: key_values "," key_value {$$ = $1+","+$3;} | key_value {$$ = $1;};
key_value: var_name ":" e {$$ = $1+":"+$3};


parameter:
	var_name {$$ = $1;}
	| var_name ":" type {$$ = "("+$1+":"+$3+")";};
	
parameters: parameter "," parameters {$$ = $1+" "+$3;} | parameter {$$ =
 $1;} | {$$ = ""};
exprs: e "," exprs {$$ = $1+","+$3;} | e {$$ = $1;};

function_call_parameters: e "," function_call_parameters {$$ = $1+" "+$3;} | e {$$ = $1;};

elif: "else" "if" "(" e ")" bracket_statements elif {$$ = ["else if ",$4," then ",$6,$7].join(" ");} | "else" bracket_statements {$$ = ["else ",$2].join("");};
if_statement:
"if" "(" e ")" bracket_statements elif {$$ = ["if ",$3," then ",$5,$6].join(" ");}
|  "if" "(" e ")" bracket_statements {$$ = ["if ",$3," then ",$5].join(" ");};
identifiers: var_name "," identifiers {$$ = $1+","+$3;} | var_name {$$ = $1;};
bracket_statements: "{" statement "}" {$$= $2;} | return_statement ";" {$$ = $1;};

type: type "[" "]" {$$ = "[]"+$1;} | "Array" "<" type ">" {$$ = "[]"+$3;} | "[" tuple_type_ "]" {$$="("+$2+")";} | type_;
type_:"boolean" {$$= "bool";}|"number"{$$= "f32";}|"string"{$$="[]u8"} | var_name {$$ = $1;};

type_parameters: var_name "," type_parameters {$$ = "'"+$1+" "+$3;} | var_name {$$ = "'"+$1;}; 
class_type_parameters: var_name "," class_type_parameters {$$ = "type "+$1+" "+$3;} | var_name {$$ = "type "+$1;};

tuple_type_: type_ "," type_parameters {$$ = $1+","+$3;} | type_ {$$ = $1;}; 

class_statement:
	var_name ":" type ";" {$$ = "val "+$1+":"+$3;}
	| func
	| "static" func {$$ = $2;}
	;

class_statements: class_statement class_statements {$$ = $1+"\\n"+$2;} | class_statement {$$ =
 $1;};
