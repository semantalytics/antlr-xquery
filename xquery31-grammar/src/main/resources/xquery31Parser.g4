grammar xquery31Parser; // XQuery 3.1

options {tokenVocab='xquery31Lexer';}

import xcore;

// XQuery 3.1 grammar, adapted from the original grammar @ https://www.w3.org/TR/xquery-3/#nt-bnf

xquery31
    : module EOF?
    ;

/* [1] */
module
    : versionDecl? (libraryModule | mainModule)
    ;

/* [2] */
versionDecl
    : 'xquery' (('encoding' StringLiteral) | ('version' StringLiteral ('encoding' StringLiteral)?)) Separator
    ;

/* [3] */
mainModule
    : prolog queryBody
    ;
/* [4] */
libraryModule
    : moduleDecl prolog
    ;

/* [5] */
moduleDecl
    : 'module' 'namespace' ncName '=' uriLiteral Separator
    ;

/* [6] */
prolog
    : ((defaultNamespaceDecl | setter | namespaceDecl | importGeneral) Separator)*
      ((contextItemDecl | annotatedDecl | optionDecl) Separator)*
    ;

/* [8] */
setter
    : BoundarySpaceDecl
    | defaultCollationDecl
    | baseURIDecl
    | ConstructionDecl
    | OrderingModeDecl
    | EmptyOrderDecl
    | copyNamespacesDecl
    | decimalFormatDecl
    ;

/* [10] */
defaultCollationDecl
    : 'declare' 'default' 'collation' uriLiteral ;

/* [11] */
baseURIDecl
    : 'declare' 'base-uri' uriLiteral ;

/* [15] */
copyNamespacesDecl
    : 'declare' 'copy-namespaces' PreserveMode ',' InheritMode
    ;

/* [18] */
decimalFormatDecl
    : 'declare' (('decimal-format' eqName)
    | ('default' 'decimal-format')) (DfPropertyName '=' StringLiteral)*
    ;

importGeneral
    : schemaImport
    | moduleImport
    ;

schemaImport
    : 'import' 'schema' schemaPrefix? uriLiteral ('at' uriLiteral (',' uriLiteral)*)?
    ;

schemaPrefix
    : ('namespace' ncName '=') | ('default' 'element' 'namespace')
    ;

moduleImport
    : 'import' 'module' ('namespace' ncName '=')? uriLiteral ('at' uriLiteral (',' uriLiteral)*)?
    ;

namespaceDecl
    : 'declare' 'namespace' ncName '=' uriLiteral
    ;

defaultNamespaceDecl
    : 'declare' 'default' ('element' | 'function') 'namespace' uriLiteral
    ;

annotatedDecl
    : 'declare' annotation* (varDecl | functionDecl)
    ;

annotation
    : '%' eqName ('(' literal (',' literal)* ')')?
    ;

varDecl
    : 'variable' '$' varName typeDeclaration? ((':=' varValue) | ('external' (':=' varDefaultValue)?))
    ;

varValue
    : exprSingle
    ;

varDefaultValue
    : exprSingle
    ;

contextItemDecl
    : 'declare' 'context' 'item' ('as' itemType)? ((':=' varValue) | ('external' (':=' varDefaultValue)?))
    ;

functionDecl
    : 'function' eqName '(' paramList? ')' ('as' sequenceType)?
        (functionBody | 'external')  /* xgc: reserved-function-names */
    ;

paramList
    : param (',' param)*
    ;

param
    : '$' eqName typeDeclaration?
    ;

functionBody
    : enclosedExpr
    ;

enclosedExpr
    : '{' expr? '}'
    ;

optionDecl
    : 'declare' 'option' eqName StringLiteral
    ;

queryBody
    : expr
    ;

expr
    : exprSingle (',' exprSingle)*
    ;

exprSingle
    : flworExpr
    | quantifiedExpr
    | switchExpr
    | typeswitchExpr
    | ifExpr
    | tryCatchExpr
    | orExpr
    ;

flworExpr
    : initialClause (intermediateClause*) returnClause
    ;

initialClause
    : forClause
    | letClause
    | windowClause
    ;

intermediateClause
    : initialClause
    | whereClause
    | groupByClause
    | orderByClause
    | countClause
    ;

forClause
    : 'for' forBinding (',' forBinding)*
    ;

forBinding
    : '$' varName typeDeclaration? AllowingEmpty? positionalVar? 'in' exprSingle
    ;

positionalVar
    : 'at' '$' varName
    ;

letClause
    : 'let' letBinding (',' letBinding)*
    ;

letBinding
    : '$' varName typeDeclaration? ':=' exprSingle
    ;

windowClause
    : 'for' (tumblingWindowClause | slidingWindowClause) ;

tumblingWindowClause
    : 'tumbling' 'window' '$' varName typeDeclaration? 'in' exprSingle windowStartCondition windowEndCondition?
    ;

slidingWindowClause
    : 'sliding' 'window' '$' varName typeDeclaration?
      'in' exprSingle windowStartCondition windowEndCondition
    ;

windowStartCondition
    : 'start' windowVars 'when' exprSingle
    ;

windowEndCondition
    : 'only'? 'end' windowVars 'when' exprSingle
    ;

windowVars
    : ('$' currentItem)? positionalVar? ('previous' '$' previousItem)? ('next' '$' nextItem)?
    ;

currentItem
    : eqName
    ;

previousItem
    : eqName
    ;

nextItem
    : eqName
    ;

countClause
    : 'count' '$' varName
    ;

whereClause
    : 'where' exprSingle ;

groupByClause
    : 'group' 'by' groupingSpecList ;

groupingSpecList
    : groupingSpec (',' groupingSpec)* ;

groupingSpec
    : groupingVariable (typeDeclaration? ':=' exprSingle)? ('collation' uriLiteral)?
    ;

groupingVariable
    : '$' varName ;

orderByClause
    : (('order' 'by') | ('stable' 'order' 'by')) orderSpecList ;

orderSpecList
    : orderSpec (',' orderSpec)* ;

orderSpec
    : exprSingle orderModifier ;

orderModifier
    : ('ascending' | 'descending')?
        ('empty' ('greatest' | 'least'))?
        ('collation' uriLiteral)?
    ;

returnClause
    : 'return' exprSingle
    ;

quantifiedExpr
    :
        ('some' | 'every') '$' varName typeDeclaration?
        'in' exprSingle
        (',' '$' varName typeDeclaration? 'in' exprSingle)*
        'satisfies' exprSingle
    ;

switchExpr
    : 'switch' '(' expr ')' switchCaseClause+ 'default' 'return' exprSingle
    ;

switchCaseClause
    : ('case' switchCaseOperand)+ 'return' exprSingle
    ;

switchCaseOperand
    : exprSingle
    ;

typeswitchExpr
    : 'typeswitch' '(' expr ')' caseClause+ 'default' ('$' varName)?
        'return' exprSingle ;

caseClause
    : 'case' ('$' varName 'as')? sequenceTypeUnion 'return' exprSingle ;

sequenceTypeUnion
    : sequenceType ('|' sequenceType)* ;

ifExpr
    : 'if' '(' expr ')' 'then' exprSingle 'else' exprSingle ;

tryCatchExpr
    : tryClause catchClause+ ;

tryClause
    : 'try' enclosedTryTargetExpr ;

enclosedTryTargetExpr
    : enclosedExpr ;

catchClause
    : 'catch' catchErrorList enclosedExpr ;

catchErrorList
    : nameTest ('|' nameTest)* ;

orExpr
    : andExpr ( 'or' andExpr )* ;

andExpr
    : comparisonExpr ( 'and' comparisonExpr )* ;

comparisonExpr
    : stringConcatExpr ((ValueComp| GeneralComp| NodeComp) stringConcatExpr)? ;

stringConcatExpr
    : rangeExpr ( '||' rangeExpr )* ;

rangeExpr
    : additiveExpr ( 'to' additiveExpr )? ;

additiveExpr
    : multiplicativeExpr ( ('+' | '-') multiplicativeExpr )* ;

multiplicativeExpr
    : unionExpr ( ('*' | 'div' | 'idiv' | 'mod') unionExpr )* ;

unionExpr
    : intersectExceptExpr ( ('union' | '|') intersectExceptExpr )* ;

intersectExceptExpr
    : instanceofExpr ( ('intersect' | 'except') instanceofExpr )* ;

instanceofExpr
    : treatExpr ( 'instance' 'of' sequenceType )? ;

treatExpr
    : castableExpr ( 'treat' 'as' sequenceType )? ;

castableExpr
    : castExpr ( 'castable' 'as' singleType )? ;

castExpr
    : arrowExpr ( 'cast' 'as' singleType )? ;

arrowExpr
    : unaryExpr ( '=>' arrowFunctionSpecifier argumentList )* ;

unaryExpr
    : ('-' | '+')* valueExpr ;

valueExpr
    : validateExpr | extensionExpr | simpleMapExpr ;

validateExpr
    : 'validate' (ValidationMode | ('type' typeName))? enclosedExpr ;

extensionExpr
    : pragma+ enclosedExpr ;

pragma
    : '(#' S? eqName (S pragmaContents)? '#)'  /* ws: explicit */ ;

// pragmaContents : (Char* ~ (Char* '#)' Char*)) ; // This is the original rule
pragmaContents
    : (Char* ~ PragmaContentsInternal) ;

simpleMapExpr
    : pathExpr ('!' pathExpr)* ;

pathExpr
    : ('/' relativePathExpr?)
    | ('//' relativePathExpr)
    | relativePathExpr  /* xgc: leading-lone-slash */
    ;

relativePathExpr
    : stepExpr (('/' | '//') stepExpr)* ;

stepExpr
    : postfixExpr | axisStep ;

axisStep
    : (reverseStep | forwardStep) predicateList ;

forwardStep
    : (ForwardAxis nodeTest) | abbrevForwardStep ;

abbrevForwardStep
    : '@'? nodeTest ;

reverseStep
    : (ReverseAxis nodeTest) | AbbrevReverseStep ;

nodeTest
    : kindTest | nameTest ;

nameTest
    : eqName | wildcard ;

wildcard
    : '*'
    | (ncName ':' '*')
    | ('*' ':' ncName)
    | (bracedURILiteral '*')  /* ws: explicit */ ;

postfixExpr
    : primaryExpr (predicate | argumentList | lookup)* ;

argumentList
    : '(' (argument (',' argument)*)? ')' ;

predicateList
    : predicate* ;

predicate
    : '[' expr ']' ;

lookup
    : '?' keySpecifier ;

keySpecifier
    : ncName | IntegerLiteral | parenthesizedExpr | '*'
    ;

arrowFunctionSpecifier
    : eqName | varRef | parenthesizedExpr
    ;

primaryExpr
    : literal
    | varRef
    | parenthesizedExpr
    | ContextItemExpr
    | functionCall
    | orderedExpr
    | unorderedExpr
    | nodeConstructor
    | functionItemExpr
    | mapConstructor
    | arrayConstructor
    | stringConstructor
    | unaryLookup
    ;

literal
    : numericLiteral
    | StringLiteral
    ;

numericLiteral
    : IntegerLiteral
    | DecimalLiteral
    | DoubleLiteral
    ;

varRef
    : '$' varName ;

varName : eqName ;

parenthesizedExpr
    : '(' expr? ')' ;

orderedExpr
    : 'ordered' enclosedExpr ;

unorderedExpr
    : 'unordered' enclosedExpr ;

functionCall
    : eqName argumentList  /* xgc: reserved-function-names */ ;

/* gn: parens */
argument
    : exprSingle | ArgumentPlaceholder ;

nodeConstructor
    : directConstructor | computedConstructor ;

directConstructor
    : dirElemConstructor | dirCommentConstructor | dirPIConstructor ;

dirElemConstructor
    : '<' qName dirAttributeList
        ('/>' | ('>' dirElemContent* '</' qName S? '>'))  /* ws: explicit */ ;

dirAttributeList
    :
        (S (qName S? '=' S? dirAttributeValue)?)*  /* ws: explicit */ ;

dirAttributeValue
    : ('"'  (EscapeQuot | quotAttrValueContent)* '"')
                  | ('\'' (EscapeApos | aposAttrValueContent)* '\'')  /* ws: explicit */ ;

quotAttrValueContent
    : QuotAttrContentChar | commonContent ;

aposAttrValueContent
    : AposAttrContentChar | commonContent ;

dirElemContent
    : directConstructor
    | cDataSection
    | commonContent
    | ElementContentChar
    ;

commonContent
    : PredefinedEntityRef
    | CharRef
    | '{{' | '}}'
    | enclosedExpr
    ;

dirCommentConstructor
    : '<!--' dirCommentContents '-->'  /* ws: explicit */ ;

// dirCommentContents : ((Char - '-') | ('-' (Char - '-')))*  /* ws: explicit */ ; // This is the original rule
dirCommentContents
    : ((Char ~ '-') | ('-' (Char ~ '-')))*  /* ws: explicit */ ;

dirPIConstructor
    : '<?' piTarget (S dirPIContents)? '?>'  /* ws: explicit */ ;

// dirPIContents : (Char* - (Char* '?>' Char*))  /* ws: explicit */ ;  // This is the original rule
dirPIContents
    : (Char* ~ DirPIContentsInternal)  /* ws: explicit */ ;

cDataSection
    : '<![CDATA[' cDataSectionContents ']]>'  /* ws: explicit */ ;

// cDataSectionContents : (Char* - (Char* ']]>' Char*))  /* ws: explicit */ ; // This is the original rule

cDataSectionContents
    : (Char* ~ CDataSectionContentsInternal)  /* ws: explicit */ ;

computedConstructor
    : compDocConstructor
    | compElemConstructor
    | compAttrConstructor
    | compNamespaceConstructor
    | compTextConstructor
    | compCommentConstructor
    | compPIConstructor
    ;

compDocConstructor
    : 'document' enclosedExpr ;

compElemConstructor
    : 'element' (eqName | ('{' expr '}')) enclosedContentExpr ;

enclosedContentExpr
    : enclosedExpr ;

compAttrConstructor
    : 'attribute' (eqName | ('{' expr '}')) enclosedExpr ;

compNamespaceConstructor
    : 'namespace' (prefix | enclosedPrefixExpr) enclosedURIExpr
    ;

prefix
    : ncName ;

enclosedPrefixExpr
    : enclosedExpr ;

enclosedURIExpr
    : enclosedExpr ;

compTextConstructor
    : 'text' enclosedExpr ;

compCommentConstructor
    : 'comment' enclosedExpr ;

compPIConstructor :
        'processing-instruction' (ncName | ('{' expr '}')) enclosedExpr ;

functionItemExpr
    : namedFunctionRef | inlineFunctionExpr ;

namedFunctionRef
    : eqName '#' IntegerLiteral  /* xgc: reserved-function-names */ ;

inlineFunctionExpr :
        annotation* 'function' '(' paramList? ')'
        ('as' sequenceType)? functionBody ;

mapConstructor :
        'map' '{' (mapConstructorEntry (',' mapConstructorEntry)*)? '}' ;

mapConstructorEntry : mapKeyExpr ':' mapValueExpr ;

mapKeyExpr
    : exprSingle ;

mapValueExpr
    : exprSingle ;

arrayConstructor
    : squareArrayConstructor | curlyArrayConstructor ;

squareArrayConstructor
    : '[' (exprSingle (',' exprSingle)*)? ']' ;

curlyArrayConstructor
    : 'array' '{' expr? '}' ;

stringConstructor
    : '``[' stringConstructorContent ']``'  /* ws: explicit */ ;

stringConstructorContent
    :
        stringConstructorChars
        (stringConstructorInterpolation stringConstructorChars)*
        /* ws: explicit */ ;
// stringConstructorChars :
//         (Char* - (Char* ('`{' | ']``') Char*))  /* ws: explicit */ ;  // This is the original rule

stringConstructorChars
    : (Char* ~ StringConstructorCharsInternal)  /* ws: explicit */ ;

stringConstructorInterpolation
    : '`{' expr? '}`' ;

unaryLookup
    : '?' keySpecifier ;

singleType
    : simpleTypeName '?'? ;

typeDeclaration
    : 'as' sequenceType ;

sequenceType
    : ('empty-sequence' '(' ')') | (itemType occurrenceIndicator?) ;

occurrenceIndicator
    : '?' | '*' | '+'  /* xgc: occurrence-indicators */ ;

itemType
    : kindTest
    | ('item' '(' ')')
    | functionTest
    | mapTest
    | arrayTest
    | atomicOrUnionType
    | parenthesizedItemType
    ;

atomicOrUnionType
    : eqName
    ;

kindTest
    : documentTest
    | elementTest
    | attributeTest
    | schemaElementTest
    | schemaAttributeTest
    | piTest
    | CommentTest
    | TextTest
    | NamespaceNodeTest
    | AnyKindTest ;

documentTest
    : 'document-node' '(' (elementTest | schemaElementTest)? ')' ;

piTest
    : 'processing-instruction' '(' (ncName | StringLiteral)? ')' ;

attributeTest
    : 'attribute' '(' (attribNameOrWildcard (',' typeName)?)? ')' ;

attribNameOrWildcard
    : attributeName | '*' ;

schemaAttributeTest
    : 'schema-attribute' '(' attributeDeclaration ')' ;

attributeDeclaration
    : attributeName ;

elementTest
    : 'element' '(' (elementNameOrWildcard (',' typeName '?'?)?)? ')' ;

elementNameOrWildcard
    : elementName | '*' ;

schemaElementTest
    : 'schema-element' '(' elementDeclaration ')' ;

elementDeclaration
    : elementName ;

attributeName
    : eqName ;

elementName
    : eqName ;

simpleTypeName
    : typeName ;

typeName
    : eqName ;

functionTest
    : annotation* (AnyFunctionTest | typedFunctionTest) ;

typedFunctionTest
    : 'function' '(' (sequenceType (',' sequenceType)*)? ')' 'as' sequenceType ;

mapTest
    : AnyMapTest | typedMapTest ;

typedMapTest
    : 'map' '(' atomicOrUnionType ',' sequenceType ')' ;

arrayTest
    : AnyArrayTest | typedArrayTest ;

typedArrayTest
    : 'array' '(' sequenceType ')' ;

parenthesizedItemType
    : '(' itemType ')' ;

uriLiteral
    : StringLiteral ;

eqName
    : qName
    | uriQualifiedName
    ;

// Terminals:
uriQualifiedName
    : bracedURILiteral ncName /* ws: explicit */ ;

bracedURILiteral
    : 'Q' '{' (PredefinedEntityRef | CharRef | BracedURILiteralInternal)* '}'
        /* ws: explicit */ ;

