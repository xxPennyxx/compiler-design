grammar hello2;

//Variables and data types
start : program;

program: declarationList
       | functionDeclaration program
       | classDeclaration program
       | objectCreation program
       | statement program
       | arrayDeclaration program
       | commentedProgram
       | errorHandlingProgram
       | vectorDeclaration program
       | fileOperations program
       | pointerDeclaration program
       | scopeResolution program
       | structureDeclaration program
       | memoryAllocationProgram program
       | memoryDeallocationProgram program
       | dynamicMemoryAllocationProgram program
       | stlProgram program
       | breakStatement program
       | continueStatement program
       | includeDirective program
       | nameSpace program
       |;

nameSpace: KW_USING WS KW_NAMESPACE WS KW_STD NEW_LINE;
includeDirective: KW_INCLUDE KW_IOSTREAM NEW_LINE;

declarationList: declaration declarationList
              ?;

statementList: statement statementList
              ?;


declaration: typeSpecifier idList SEMICOLON ;
typeSpecifier : KW_INT | KW_DOUBLE | KW_CHAR | KW_BOOL | KW_FLOAT | KW_LONG | KW_SHORT | KW_UNSIGNED | KW_SIGNED | KW_VOID | userDefinedType ;
userDefinedType : ID ;
idList : ID COMMA idList | ID ;

//Conditional statements
//statement :ifStatement | otherStatement ;
ifStatement : KW_IF LPAREN expression RPAREN block KW_ELSE block | KW_IF LPAREN expression RPAREN block | KW_IF LPAREN expression RPAREN statement KW_ELSE statement; 
block : LBRACE statementList RBRACE ;




expression: expressions ;
expressions: term expressions1;
expressions1: PLUS term expressions1 | MINUS term expressions1 ?;

term: factor term1;
term1: STAR factor term1 | SLASH factor term1 ?;


factor : LPAREN expression RPAREN
       | KW_INT
       | KW_FLOAT
       | ID
       ;

//Loops

//statement : loopStatement | otherStatement ;
loopStatement : forLoop | whileLoop | doWhileLoop ;
forLoop : KW_FOR LPAREN forInitializer SEMICOLON forCondition SEMICOLON forUpdate RPAREN block; 
forInitializer : declaration | expression ?; 
forCondition : expression ?; 
forUpdate : expressionList ?; 
whileLoop : KW_WHILE LPAREN expression RPAREN block; 
doWhileLoop :KW_DO block KW_WHILE LPAREN expression RPAREN SEMICOLON ;

expressionList : expression COMMA expressionList | expression ;
 
 
//Functions
functionDeclaration : returnType functionName LPAREN parameterList RPAREN block | accessSpecifier returnType functionName LPAREN parameterList RPAREN functionBody ;

returnType : typeSpecifier | KW_VOID;
functionName :ID ;
parameterList : parameter | parameter COMMA parameterList ?; 
parameter :typeSpecifier ID;
statement :
expressionStatement | 
ifStatement | 
whileLoop | 
forLoop | 
returnStatement |
expressionStatement | 
assignmentStatement |
arrayAssignment | 
arrayAccess|
errorStatement|
vectorAssignment | 
vectorAccess |
pointerAssignment | 
pointerDereference 
;
expressionStatement : expression SEMICOLON;
returnStatement : KW_RETURN expression SEMICOLON ;
//otherStatement :
 // Define other types of statements allowed in C++ here 
 

//Objects and Properties
classDeclaration : KW_CLASS ID LBRACE classBody RBRACE|
KW_CLASS ID baseClassSpecifier LBRACE classBody RBRACE
;
classBody : memberDeclaration classBody ?; 
memberDeclaration : variableDeclaration | 
typeSpecifier idList SEMICOLON |
functionDeclaration;
variableDeclaration : typeSpecifier ID SEMICOLON; 
objectCreation : ID objectName ASSIGN KW_NEW ID LPAREN RPAREN SEMICOLON ;
objectName : ID; 
assignmentStatement : ID '.' ID ASSIGN expression SEMICOLON ;

//Arrays
arrayDeclaration :typeSpecifier ID LSQUARE arraySize RSQUARE SEMICOLON; 
arraySize : INT | ID; 
arrayAssignment : ID LSQUARE expression RSQUARE ASSIGN expression SEMICOLON; 
arrayAccess :ID LSQUARE expression RSQUARE SEMICOLON; 

//Comments
commentedProgram : comment commentedProgram | code commentedProgram ?; 
comment : singleLineComment | multiLineComment;
singleLineComment :'//' ID '\n' ;
multiLineComment : '/' ID '/' ;
code : ID code ?;

//Virtual
memberFunctionDeclaration : virtualSpecifier returnType functionName LPAREN parameterList RPAREN statement;
virtualSpecifier : KW_VIRTUAL?;  

//Friend
friendDeclaration : KW_FRIEND functionDeclaration | KW_FRIEND KW_CLASS ID SEMICOLON ;
accessSpecifier : KW_PRIVATE | KW_PUBLIC | KW_PROTECTED;

//Error Handling
errorHandlingProgram : tryBlock errorHandlingProgram ?; 
tryBlock : KW_TRY compoundStatement; 
compoundStatement : LBRACE statementList RBRACE; 
errorStatement :throwStatement | catchStatement | finallyStatement; 
throwStatement :KW_THROW expression SEMICOLON; 
catchStatement : KW_CATCH LPAREN exceptionDeclaration RPAREN compoundStatement; 
exceptionDeclaration : typeSpecifier ID; 
finallyStatement : KW_FINALLY compoundStatement ;


//Vector
vectorDeclaration : 'std' ':' ':' 'vector' '<' vectorType '>' ID SEMICOLON; 
vectorType : KW_INT | KW_DOUBLE | KW_CHAR | KW_BOOL | KW_FLOAT | KW_LONG | KW_SHORT | KW_UNSIGNED | KW_SIGNED | userDefinedType ;
vectorAssignment : ID ASSIGN 'std' ':' ':' 'vector' '<' vectorType '>' LPAREN vectorElements RPAREN SEMICOLON; 
vectorAccess :ID LSQUARE expression RSQUARE SEMICOLON ;
vectorElements : expression | expression COMMA vectorElements ?; 
//Expression : // Define your CFG for expressions, e.g., simple arithmetic expressions 

//File handling
fileOperations : fileOpen | fileRead | fileWrite | fileClose;
fileOpen : IFSTREAM ID LPAREN fileName RPAREN SEMICOLON| OFSTREAM ID LPAREN fileName RPAREN SEMICOLON| FSTREAM ID LPAREN fileName RPAREN SEMICOLON;
fileRead : IFSTREAM ID RIGHT_SHIFT ID SEMICOLON;
fileWrite : OFSTREAM ID LEFT_SHIFT ID SEMICOLON ;
fileClose : ID '.' CLOSE LPAREN RPAREN SEMICOLON ;
//statement : ExpressionStatement | OtherStatement;
//ExpressionStatement : Expression SEMICOLON 
//Expression : // Define your CFG for expressions, e.g., simple arithmetic expressions 
//OtherStatement : // Define other types of statements allowed in C++ here 
fileName : STRING ;

//Pointers
pointerDeclaration : typeSpecifier STAR ID SEMICOLON; 
pointerAssignment : ID ASSIGN BIT_AND ID SEMICOLON ;
pointerDereference : ID ASSIGN STAR ID SEMICOLON ;


//Scope resolution
scopeResolution : ID KW_SCOPE ID ;

//Structures
structureDeclaration : KW_STRUCT ID LBRACE memberDeclarations RBRACE SEMICOLON ;
memberDeclarations : memberDeclaration memberDeclarations ?; 


//Inheritance
baseClassSpecifier : ':' accessSpecifier ID ;
functionBody : LBRACE statementList RBRACE ;

//Memory allocation
memoryAllocationProgram : mallocAllocation | newAllocation ;
mallocAllocation : MALLOC LPAREN sizeExpression RPAREN SEMICOLON ;
newAllocation : KW_NEW typeSpecifier SEMICOLON ;
memoryDeallocationProgram : deallocDeallocation | freeDeallocation ;
deallocDeallocation : 'dealloc' LPAREN pointerExpression RPAREN SEMICOLON ;// is there a keyword called dealloc?
freeDeallocation : FREE LPAREN pointerExpression RPAREN SEMICOLON ;
sizeExpression : ; // Define your CFG for expressions determining memory size here 
pointerExpression:STAR ID;

//Dynamic Memory Allocation
dynamicMemoryAllocationProgram : newAllocation | deleteDeallocation ;
deleteDeallocation : KW_DELETE expression SEMICOLON; 

//Standard template libraries
stlProgram : vectorOperation | listOperation | algorithmOperation ;
vectorOperation : 'vector' '<' typeSpecifier '>' ID SEMICOLON 
| 'push_back' LPAREN ID COMMA expression RPAREN SEMICOLON 
| 'pop_back' LPAREN ID RPAREN SEMICOLON 
| 'size' LPAREN ID RPAREN SEMICOLON ;
listOperation : 'list' '<' typeSpecifier '>' ID SEMICOLON 
| 'push_back' LPAREN ID COMMA expression RPAREN SEMICOLON 
| 'push_front' LPAREN ID COMMA expression RPAREN SEMICOLON 
| 'pop_back' LPAREN ID RPAREN SEMICOLON 
| 'pop_front' LPAREN ID RPAREN SEMICOLON 
| 'size' LPAREN ID RPAREN SEMICOLON ;
algorithmOperation : 'sort' LPAREN ID RPAREN SEMICOLON 
| 'find' LPAREN ID COMMA expression RPAREN SEMICOLON ;

//Break and Continue
breakStatement : KW_BREAK SEMICOLON ;
continueStatement : KW_CONTINUE SEMICOLON ;


//Class and struct specific
KW_CLASS	: 'class';
KW_STRUCT	:'struct';//Added 
KW_TYPEDEF	:'typedef';//Added 
KW_ENUM     : 'enum'; 
KW_UNION    : 'union'; 

//Data members and function related keywords
KW_PUBLIC   : 'public'; 
KW_PRIVATE  : 'private';
KW_PROTECTED: 'protected';  
KW_FRIEND   : 'friend'; 
KW_STATIC   : 'static'; 
KW_TEMPLATE : 'template';  
KW_THIS     : 'this';  
KW_VIRTUAL  : 'virtual'; 
KW_VOID  	: 'void'; 
KW_RETURN	: 'return'; 

//Exception handling
KW_CATCH    : 'catch';   
KW_THROW    : 'throw';     
KW_TRY      : 'try';
KW_FINALLY	: 'finally';
  

// Memory Allocation Keywords
MALLOC		: 'malloc';
REALLOC		: 'realloc';
CALLOC		: 'calloc';
FREE		: 'free';
KW_DELETE   : 'delete';   
KW_NEW      : 'new';      

// File handling
KW_FILE     : 'file';   
FSTREAM		: 'fstream';  
IFSTREAM	: 'ifstream';  
OFSTREAM	: 'ofstream';  
OPEN		: 'open';  
CLOSE		: 'close';  
READ		: 'read';  
WRITE		: 'write';

//Conditional keywords
KW_IF		: 'if';
KW_ELSE     : 'else'; 

//Looping
KW_FOR		: 'for';  
KW_WHILE	: 'while';
KW_DO       : 'do'; 
KW_BREAK    : 'break'; 
KW_CONTINUE : 'continue'; 

//Switch case construct
KW_SWITCH	: 'switch'; 
KW_CASE     : 'case'; 
KW_DEFAULT  : 'default'; 


//Data types
KW_INT		: 'int';
KW_CHAR     : 'char'; 
KW_FLOAT	: 'float';
KW_DOUBLE   : 'double'; 
KW_BOOL		: 'bool'; 
KW_STRING	: 'string'; 
KW_LONG		: 'long';    
KW_SHORT    : 'short';        
KW_UNSIGNED	: 'unsigned';
KW_SIGNED	: 'signed';


//Other keywords
KW_ASM		: 'asm'; 
KW_AUTO     : 'auto';
KW_CONST    : 'const'; 
KW_EXTERN   : 'extern'; 
KW_INLINE   : 'inline'; 
KW_GLOBAL   : 'global';   
KW_LOCAL    : 'local'; 
KW_OPERATOR : 'operator';  
KW_USING	: 'using';//Added 
KW_NAMESPACE	: 'namespace';//Added 
KW_COUT		:'cout';//Added 
KW_CIN		:'cin';//Added 
KW_STD      :'std';
KW_INCLUDE	: '#include';//Added 
KW_IOSTREAM	: '<iostream>';//Added 
KW_BITSSTDCPP	:'<bits/stdc++.h>';//Added 


// Whitespace and Comments
WS         : [ \t\r\n]+;
COMMENT    : '//' ~[\r\n]*;
MULTILINE_COMMENT: '/' .? '*/';   

//Constants
MAX_INT: 'INT_MAX'; 
MIN_INT: 'INT_MIN'; 
MAX_FLOAT: 'FLT_MAX'; 
MIN_FLOAT: 'FLT_MIN'; 
MAX_DOUBLE: 'DBL_MAX'; 
MIN_DOUBLE: 'DBL_MIN'; 
MAX_LONG: 'LONG_MAX'; 
MIN_LONG: 'LONG_MIN';

// Operators and Punctuation
KW_HASH     : '#'; 
KW_DOT      : '.'; 
KW_SCOPE   : '::';   
PLUS       : '+';
MINUS      : '-';
STAR       : '*';
SLASH      : '/';
MODULO     : '%';
LT         : '<';
LE         : '<=';
GT         : '>';
GE         : '>=';
EQ         : '==';
NE         : '!=';
AND        : '&&';
OR         : '||';
ASSIGN     : '=';
PLUS_EQ    : '+=';
MINUS_EQ   : '-=';
STAR_EQ    : '*=';
SLASH_EQ   : '/=';
MODULO_EQ  : '%=';
INCREMENT  : '++';
DECREMENT  : '--';
BIT_AND    : '&';
BIT_OR     : '|';
BIT_XOR    : '^';
BIT_NOT    : '~';
LEFT_SHIFT : '<<';
RIGHT_SHIFT: '>>';
NOT        : '!';

// Identifiers and Constants
ID         : [a-zA-Z_][a-zA-Z0-9_]*;
INT        : [0-9]+;
FLOAT      : [0-9]+('.'[0-9]*)?;
STRING     : '"' ~["\r\n]* '"';
CHAR       : '\'' ~[']SEMICOLON;

// Brackets, Semicolon, and Colon
LPAREN     : '(';
RPAREN     : ')';
LSQUARE	   : '[';
RSQUARE	   : ']';
LBRACE     : '{';
RBRACE     : '}';
SEMICOLON  : ';';
COLON      : ':';
COMMA	   : ',';
QUESTIONMARK : '?';


ESCAPE_SEQ : '\\'[abfnrtv'"?\\];
NEW_LINE : '\r\n';