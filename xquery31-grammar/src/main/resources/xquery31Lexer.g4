grammar xquery31Lexer; // XQuery 3.1

import xcore;

// XQuery 3.1 grammar, adapted from the original grammar @ https://www.w3.org/TR/xquery-3/#nt-bnf

/* [7] */
Separator
    : ';'
    ;                                // Terminal rule

BoundarySpaceDecl
    : 'declare' 'boundary-space' ('preserve' | 'strip') ; // Terminal rule

ConstructionDecl
    : 'declare' 'construction' ('strip' | 'preserve')
    ;    // Terminal rule

OrderingModeDecl
    : 'declare' 'ordering' ('ordered' | 'unordered')
    ;     // Terminal rule

EmptyOrderDecl
    : 'declare' 'default' 'order' 'empty' ('greatest' | 'least')
    ; // Terminal rule

PreserveMode
    : 'preserve'
    | 'no-preserve'
    ;    // Terminal rule

InheritMode
    : 'inherit'
    | 'no-inherit'
    ;       // Terminal rule

DfPropertyName
    : 'decimal-separator'
    | 'grouping-separator'
    | 'infinity'
    | 'minus-sign'
    | 'NaN'
    | 'percent'
    | 'per-mille'
    | 'zero-digit'
    | 'digit'
    | 'pattern-separator'
    | 'exponent-separator'
    ;                // Terminal rule
AllowingEmpty
    : 'allowing' 'empty' ;                                 // Terminal rule

GeneralComp : '=' | '!=' | '<' | '<=' | '>' | '>=' ;                  // Terminal rule

ValueComp : 'eq' | 'ne' | 'lt' | 'le' | 'gt' | 'ge' ;                 // Terminal rule

NodeComp : 'is' | '<<' | '>>' ;                                       // Terminal rule


ValidationMode : 'lax' | 'strict' ;                                   // Terminal rule

// pragmaContents : (Char* ~ (Char* '#)' Char*)) ; // This is the original rule

PragmaContentsInternal : (Char* '#)' Char*) ;

ForwardAxis : ('child' '::')
    | ('descendant' '::')
    | ('attribute' '::')
    | ('self' '::')
    | ('descendant-or-self' '::')
    | ('following-sibling' '::')
    | ('following' '::') ;                 // Terminal rule

ReverseAxis : ('parent' '::')
    | ('ancestor' '::')
    | ('preceding-sibling' '::')
    | ('preceding' '::')
    | ('ancestor-or-self' '::') ;          // Terminal rule

AbbrevReverseStep : '..' ;                 // Terminal rule

ContextItemExpr : '.' ;                // Terminal rule

ArgumentPlaceholder : '?' ;            // Terminal rule

DirPIContentsInternal : (Char* '?>' Char*) ;        // Terminal rule

// cDataSectionContents : (Char* - (Char* ']]>' Char*))  /* ws: explicit */ ; // This is the original rule

CDataSectionContentsInternal : (Char* ']]>' Char*) ;    // Terminal rule

StringConstructorCharsInternal : (Char* ('`{' | ']``') Char*) ; // Terminal rule

AnyKindTest : 'node' '(' ')' ;                     // Terminal rule

TextTest : 'text' '(' ')' ;                        // Terminal rule

CommentTest : 'comment' '(' ')' ;                  // Terminal rule

NamespaceNodeTest : 'namespace-node' '(' ')' ;     // Terminal rule

AnyFunctionTest : 'function' '(' '*' ')' ;         // Terminal rule
AnyMapTest : 'map' '(' '*' ')' ;                   // Terminal rule
AnyArrayTest : 'array' '(' '*' ')' ;               // Terminal rule

// Terminals:
IntegerLiteral : Digits ;

DecimalLiteral : ('.' Digits) | (Digits '.' [0-9]*) /* ws: explicit */ ;

DoubleLiteral :
        (('.' Digits) | (Digits ('.' [0-9]*)?)) [eE] [+-]? Digits
        /* ws: explicit */ ;

StringLiteral :
        ('"'  (PredefinedEntityRef | CharRef | EscapeQuot | [^\"&])* '"')   |
        ('\'' (PredefinedEntityRef | CharRef | EscapeApos | [^\'&])* '\'')
        /* ws: explicit */ ;

BracedURILiteralInternal : [^&{}] ;

PredefinedEntityRef :
        '&' ('lt' | 'gt' | 'amp' | 'quot' | 'apos') ';'      // Terminal rule
        /* ws: explicit */ ;

EscapeQuot : '""' ;

EscapeApos : '\'\'' ;

// ElementContentChar : (Char - [{}<&]) ;  // This is the original rule
// QuotAttrContentChar : (Char - [\"{}<&]) ;  // This is the original rule
// AposAttrContentChar : (Char - [\'{}<&]) ;  // This is the original rule
ElementContentChar  : (Char ~ [{}<&]) ;

QuotAttrContentChar : (Char ~ [\"{}<&]) ;

AposAttrContentChar : (Char ~ [\'{}<&]) ;
