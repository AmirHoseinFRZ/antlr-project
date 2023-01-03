grammar antlr;

start : (def_lib | def_func)* def_var_or_arr* def_class*;

//libraries//
def_lib : NAME (',' NAME)* '=' lib_statement (',' lib_statement)* ';';
lib_statement :  ('require' | '=>') NAME;
def_func : NAME (',' NAME)* '=' func_statement (',' func_statement)* ';';
func_statement : 'from' NAME ('require' | '=>') NAME;

//defining Variables & arreys//
def_var_or_arr : def_var | def_arr;
def_var : ((ACCESSIBILITY? CONST? DATA_TYPE var (',' var)*) | svar) ';';
var : name ('=' (expression | t_expression))?;
svar : name '=' (expression | t_expression);
def_arr : ((ACCESSIBILITY? DATA_TYPE NAME '[]' '=' (('new' DATA_TYPE '[' INT ']') | initial_arr)) | arr) ';';
initial_arr : ('[' (data_value (',' data_value)*) ']') | '[]';
arr : name '=' (initial_arr | name) ;

//defining function//
func : return_func | void_func;
void_func : ACCESSIBILITY? 'void' NAME '(' (DATA_TYPE NAME (',' DATA_TYPE NAME)*)? ')' 'begin' code* 'end';
return_func : ACCESSIBILITY? DATA_TYPE NAME '(' (DATA_TYPE NAME (',' DATA_TYPE NAME)*)? ')' 'begin' code* return_statment 'end';
return_statment : 'return' ((name (',' name)*) | t_expression | expression) ';';

// function calling
func_call : name '('(expression) (',' (expression))*')';
single_func_call : func_call ';';

//defining class//
def_class : 'class' NAME ('(' NAME ')')? ('implements' (NAME (',' NAME)*))? 'begin' class_body constructor? class_body'end';
class_body : (func | def_var_or_arr)*;
constructor : NAME '('(DATA_TYPE NAME ('=' data_value)? (',' DATA_TYPE NAME ('=' data_value)?)*)?')' 'begin' cons_body 'end';
cons_body : (code | ('this.'NAME '=' (data_value | name) ';'))*;

//object instantiation//
def_obj : object1 | object2;
object1 : ACCESSIBILITY? CONST? NAME NAME ((('=' NAME '('((data_value | name)(',' (data_value | NAME))*)?')')?) | ('=' 'Null')) ';';
object2 : NAME '=' (NAME '('((data_value | name)(',' (data_value | name))*)?')' | 'Null') ';';

//for loop//
for : for1 | for2;
for1 : 'for' '(' DATA_TYPE NAME '=' data_value ';' expression ';' ((name'++') | (name'--')) ')' 'begin' code 'end';
for2 : 'for' NAME 'in' name 'begin' code 'end';

//while loop//
while : while1 | while2;
while1 : 'while' '(' expression ')' 'begin' code 'end';
while2 : 'do' 'begin' code 'end' 'while' '(' expression ')';

//if statement//
if_statement : if (else_if)* else?;
if : 'if' '(' expression ')' 'begin' code 'end';
else_if : 'else if' '(' expression ')' 'begin' code 'end';
else : 'else'  'begin' code 'end';

//switch case//
switch_case: 'switch' (data_value | name) 'begin' ('case' (data_value | name) ':' code ('break' ';')?)+ (default)? 'end';
default : 'default' ':' code ('break' ';')?;

//exceptions//
exception : 'try' 'begin' code 'end' 'catch' '('NAME (',' NAME)*')' 'begin' code  'end';

//ternary expression
t_expression : expression '?' expression ':' expression;
single_t_expression : expression '?' expression ':' expression ';';

//expression//
expression : '('expression')'| expression '**' expression | expression '~' expression | (('-' | '+') expression) |
            (('++' | '--') name) | (name ('++' | '--')) | expression ('*' | '/' | '//' | '%') expression |
            expression ('+' | '-') expression | expression ('<<' | '>>') expression | expression ('&' | '|' | '^') expression |
            expression ('==' | '!=' | '<>') expression |  expression ('<' | '>' | '<=' | '>=') expression |
            expression ('and' | 'or' | '||' | '&&') expression | 'not' expression |
            expression ('+=' | '-=' | '*=' | '/=' | '**=' | '//==') expression | name | data_value | func_call | 'Null';

//another useful parsers//
single_command : expression ';';
name : NAME ('.'NAME)?;
data_value : (INT|STRING|CHAR|DOUBLE|BOOL| SCIENTIFIC_SYMBOL |'Null');
code : (def_var_or_arr | func | def_obj | for | while |
      if_statement | switch_case | exception | single_func_call | single_t_expression |
      single_command)+;

//lexer//
fragment DIGIT: [0-9];
fragment ALPHABET: [A-Za-z];


INT : ('-')? DIGIT+;
CHAR : '\''ALPHABET'\'';
STRING : '"' .*? '"';
DOUBLE : ('-')? INT ('.' INT)?;
SCIENTIFIC_SYMBOL : ('-' | '+')? DIGIT '.' DIGIT+ 'e' INT;
DATA_TYPE : ('int'|'char'|'string'|'double'|'float' | 'bool');
BOOL : ('true' | 'false');
ACCESSIBILITY: ('public'|'private');
CONST: ('const');
NAME: ALPHABET(ALPHABET|DIGIT|'$'|'_')+;
SLCOMENT : ('//' .*? '\n') -> skip;
MLCOMENT : ('/*'.*? '*/') -> skip;
WS: ([ \n\t\r]+) -> skip;
INVALID_NAME : ('require' | 'from' | 'public' | 'private' | 'const' | 'int'|'char'|
              'string'|'double'|'float' | 'bool' | 'implements' | 'begin' | 'end' |
              'return' | 'true' | 'false' | 'Null' | 'for' | 'or' | 'and' | 'in' |
              'do' | 'while' | 'if' | 'else' | 'else if'| 'switch' | 'case' | 'break' |
              'default' | 'try' | 'catch');
