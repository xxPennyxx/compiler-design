grammar hello69;

program: statement+;
statement: classDeclaration
		 | variableDeclaration
		 | functionDeclaration
         | templateDeclaration
         | block
         | expressionStatement
         | fileOperation
         | tryCatchBlock
         | dynamicMemoryAllocation
         | dynamicMemoryDeallocation
         | placementNewExpression
         | placementDeleteExpression
         | operatorNewExpression
         | operatorDeleteExpression
         | smartPointerDeclaration
         | memoryAllocationErrorHandling
         ;
         
block: '{' statement+ '}';

templateDeclaration: 'template' '<' templateParameterList '>' functionDeclaration;
templateParameterList: templateParameter (',' templateParameter)*;
templateParameter: type ID;

classDeclaration: 'class' ID '{' classBody '}';
classBody: member*;

member: visibilitySpecifier variableDeclaration {$visibility = $visibilitySpecifier.text;} ';'
      | visibilitySpecifier functionDeclaration {$visibility = $visibilitySpecifier.text;} ';'
      | friendDeclaration ';'
      ;
friendDeclaration: 'friend' 'class' ID;

visibilitySpecifier: 'private' | 'protected' | 'public';

         
variableDeclaration: type ID {$type = $type.text;} '=' expression ';' {
    // Check for type mismatch during variable assignment
    if (!checkAssignmentCompatibility($type, $expression.type)) {
        System.err.println("Error: Type mismatch in variable assignment for variable '" + $ID.text + "'.");
    }
};
type: 'int' | 'double'|'float'|'string'|'bool'|'char'|'void'|ID|'std::ifstream'|'std::ofstream';
functionDeclaration: type ID '(' parameterList? ')' block {
    // Semantic action to handle virtual functions
    if ("virtual".equals($type.text)) {
        declareVirtualFunction($ID.text);
    } else if ("friend".equals($visibilitySpecifier.text)) {
        declareFriendFunction($type.text, $ID.text);
    }
    else {
        declareFunction($ID.text);
    }
     
};

parameterList: parameter (',' parameter)*;
parameter: type ID;
expressionStatement: expression ';';
expression: primaryExpression
          | expression '+' primaryExpression {$type = checkAddition($expression.type, $primaryExpression.type);}
          | memberAccess {$type = $memberAccess.type;} // Access control check
          | 'throw' expression ';'
          ;
primaryExpression: ID {$type = getVariableType($ID.text);} 
               | INT {$type = 'int';}
               | DOUBLE {$type = 'double';}
               | CHAR {$type = 'char';}
               | FLOAT {$type = 'float';}
               | STRING {$type = 'string';}
               | BOOL {$type = 'bool';}
               | '(' expression ')' {$type = $expression.type;}
               | functionCall
               ;


functionCall: ID '(' argumentList? ')' {$type = resolveFunctionCall($ID.text, $argumentList.types);};
argumentList: expression (',' expression)* {$types = [$expression.type] + $argumentList.types;};
      
      
templateInstantiation: ID '<' typeList '>' {$type = $ID.text + '<' + String.join(",", $typeList.types) + '>';};
typeList: type (',' type)* {$types = [$type.text] + $typeList.types;} | {$types = [];};
      
memberAccess: primaryExpression '.' ID {$type = resolveMemberAccess($primaryExpression.type, $ID.text, $primaryExpression.access);};

fileOperation: fileOpenOperation
             | fileReadOperation
             | fileWriteOperation
             | fileCloseOperation
             ;

fileOpenOperation: 'std::ifstream' ID '(' STRING ')' ';' | 'std::ofstream' ID '(' STRING ')' ';';
fileReadOperation: ID '>>' ID ';';
fileWriteOperation: ID '<<' STRING ';';
fileCloseOperation: ID '.' 'close' '(' ')' ';';
      
tryCatchBlock: 'try' block catchClause+;
catchClause: 'catch' '(' exceptionType ID ')' block
           | 'catch' '(' ')' block; // catch without an exception type for rethrow

exceptionType:ID;
dynamicMemoryAllocation: typePointer ID '=' 'new' type {$type = $typePointer.type;};
dynamicMemoryDeallocation: 'delete' (ID | '[]' ID) ';';
placementNewExpression: typePointer ID '=' 'new' '(' 'ID' ')' type {$type = $typePointer.type;};
placementDeleteExpression: 'delete' '(' 'ID' ')' ';';
operatorNewExpression: 'void*' ID '=' 'operator' 'new' '(' 'sizeof' '(' type ')' ')' ';';
operatorDeleteExpression: 'operator' 'delete' '(' 'ID' ')' ';';
typePointer: type '*';
smartPointerDeclaration: 'std::shared_ptr' '<' type '>' ID '=' 'std::make_shared' '<' type '>' '(' expression ')' ';';
memoryAllocationErrorHandling: typePointer ID '=' 'new' type '[' INT ']' ';'
                              | 'if' '(' ID '==' 'nullptr' ')' block;

               
// Lexer rules
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
DOUBLE	   : INT '.' INT;
STRING     : '"' ~["\r\n]* '"';
CHAR       : '\'' ~[']SEMICOLON;
BOOL	   : 'true' | 'false';


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










// Define additional variables for semantic actions
var String type;
var List<String> types;
var String visibility;

var int currentScopeDepth = 0;

// Semantic actions
var Map<String, Map<String, String>> symbolTable = new HashMap<>();
var Map<String, List<String>> virtualTables = new HashMap<>();
var Set<String> friendClasses = new HashSet<>();
var Map<String, Integer> exceptionTypeOrder = new HashMap<>();


// Semantic actions
@members {
    private String checkAddition(String type1, String type2) {
        if ("double".equals(type1) || "double".equals(type2)) {
            return "double";
        } else {
            return "int";
        }
    }
    
     private String resolveIdentifier(String identifier) {
        // Look up the identifier in the symbol table based on the current scope
        Map<String, String> currentScope = symbolTable.get(currentScopeDepth);
        if (currentScope != null && currentScope.containsKey(identifier)) {
            return currentScope.get(identifier);
        }

        // If not found in the current scope, assume a default type (e.g., 'int')
        return "int";
    }
    
    private void declareFunction(String functionName) {
        // Declare the function in the symbol table based on the current scope and visibility
        Map<String, String> currentScope = symbolTable.get(currentScopeDepth);
        if (currentScope != null) {
            currentScope.put(functionName, $type.text);
        }
    }
    
    private void declareFriendFunction(String friendClassName, String functionName) {
        // Declare the friend function based on the friend class
        friendClasses.add(friendClassName);
        Map<String, String> friendScope = symbolTable.get(friendClassName);
        if (friendScope != null) {
            friendScope.put(functionName, "friend");
        }
    }
    
    private void declareVirtualFunction(String functionName) {
        // Declare the virtual function in the symbol table and create a virtual table entry
        Map<String, String> currentScope = symbolTable.get(currentScopeDepth);
        if (currentScope != null) {
            currentScope.put(functionName, "virtual");
            virtualTables.computeIfAbsent(currentScope.get("class"), k -> new ArrayList<>()).add(functionName);
        }
    }
    
    private String resolveFunctionCall(String functionName, List<String> argumentTypes) {
        // Look up the function in the symbol table based on the current scope
        Map<String, String> currentScope = symbolTable.get(currentScopeDepth);
        if (currentScope != null && currentScope.containsKey(functionName)) {
            // Check if there's a matching function signature
            String expectedSignature = functionName + "(" + String.join(",", argumentTypes) + ")";
            String actualSignature = currentScope.get(functionName);
            if (expectedSignature.equals(actualSignature)) {
                return "resolved";
            }
        }

        // If not found in the current scope, assume a default type (e.g., 'int')
        return "int";
    }

    private String getVariableType(String variableName) {
        // Look up the variable in the symbol table based on the current scope
        Map<String, String> currentScope = symbolTable.get(currentScopeDepth);
        if (currentScope != null && currentScope.containsKey(variableName)) {
            return currentScope.get(variableName);
        }

        // If not found in the current scope, assume a default type (e.g., 'int')
        return "int";
    }
    private String resolveMemberAccess(String objectType, String memberName, String memberVisibility) {
        // Look up the member in the symbol table based on the object type and visibility
        Map<String, String> objectMembers = symbolTable.get(objectType);
        if (objectMembers != null && objectMembers.containsKey(memberName)) {
            String declaredVisibility = objectMembers.get(memberName);
            if (checkAccessControl(memberVisibility, declaredVisibility)) {
                return declaredVisibility;
            } else {
                System.err.println("Error: Access violation. Member '" + memberName + "' is not accessible from the current scope.");
            }
        }

        // If not found in the object's members, assume a default type (e.g., 'int')
        return "int";
    }

    private boolean checkAccessControl(String accessingVisibility, String declaredVisibility) {
        // Check if the accessing visibility is allowed to access the declared visibility
        return "public".equals(declaredVisibility) || ("protected".equals(declaredVisibility) && "protected".equals(accessingVisibility)) || ("private".equals(declaredVisibility) && "private".equals(accessingVisibility));
    }

    // Enter a new scope
    private void enterScope() {
        currentScopeDepth++;
        symbolTable.put(currentScopeDepth, new HashMap<>());
    }

    // Exit the current scope
    private void exitScope() {
        symbolTable.remove(currentScopeDepth);
        currentScopeDepth--;
    }
    private boolean checkAssignmentCompatibility(String targetType, String sourceType) {
        if (!targetType.equals(sourceType)) {
            System.err.println("Error: Type mismatch in assignment. Expected '" + targetType + "', but found '" + sourceType + "'.");
            return false;
        }
        return true;
    }
    private void updateExceptionTypeOrder() {
        if (currentExceptionType != null && !exceptionTypeOrder.containsKey(currentExceptionType)) {
            exceptionTypeOrder.put(currentExceptionType, currentExceptionOrder++);
        }
    }
}

// Custom actions for scope handling
@after {
    if (_localctx.exception == null) {
        if (_localctx.start.getType() == LBRACE) {
            enterScope();
        } else if (_localctx.stop.getType() == RBRACE) {
            exitScope();
        }
    }
}
