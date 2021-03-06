grammar XQuery30;

xquery
    : module EOF
    ;

module
    : versionDecl? ( libraryModule | mainModuleSequence )
    ;

versionDecl
    : 'xquery' ( 'encoding' StringLiteral | 'version' StringLiteral ( 'encoding' StringLiteral )? ) SEPARATOR
    ;

mainModuleSequence
    : MainModule ( ';' VersionDecl? MainModule)* ';'?
    ;

mainModule
    : Prolog QueryBody
    ;

libraryModule
    : ModuleDecl Prolog
    ;

moduleDecl
    : 'module' 'namespace' NCName '=' URILiteral Separator
    ;

prolog
    : ( ( DefaultNamespaceDecl | Setter | NamespaceDecl | Import | FTOptionDecl ) Separator )* ( ( ContextItemDecl | AnnotatedDecl | OptionDecl ) Separator )*
    ;

SEPARATOR
    : ';'
    ;

setter
    : boundarySpaceDecl
    | defaultCollationDecl
    | baseURIDecl
    | constructionDecl
    | orderingModeDecl
    | emptyOrderDecl
    | revalidationDecl
    | copyNamespacesDecl
    | decimalFormatDecl
    ;

boundarySpaceDecl
    : 'declare' 'boundary-space' ( 'preserve' | 'strip' )
    ;

DefaultCollationDecl
    : 'declare' 'default' 'collation' URILiteral
    ;

BaseURIDecl
    : 'declare' 'base-uri' URILiteral
    ;

ConstructionDecl
    : 'declare' 'construction' ( 'strip' | 'preserve' )
    ;

OrderingModeDecl
    : 'declare' 'ordering' ( 'ordered' | 'unordered' )
    ;

EmptyOrderDecl
    : 'declare' 'default' 'order' 'empty' ( 'greatest' | 'least' )
    ;

CopyNamespacesDecl
    : 'declare' 'copy-namespaces' PreserveMode ',' InheritMode
    ;

PreserveMode
    : 'preserve'
    | 'no-preserve'
    ;

InheritMode
    : 'inherit'
    | 'no-inherit'
    ;

DecimalFormatDecl
    : 'declare' ( 'decimal-format' EQName | 'default' 'decimal-format' ) ( DFPropertyName '=' StringLiteral )*
    ;

DFPropertyName
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
    ;

Import
    : SchemaImport
    | ModuleImport
    | ImportStylesheetDecl
    ;

SchemaImport
    : 'import' 'schema' SchemaPrefix? URILiteral ( 'at' URILiteral ( ',' URILiteral )* )?
    ;

SchemaPrefix
    : 'namespace' NCName '='
    | 'default' 'element' 'namespace'
    ;

ModuleImport
    : 'import' 'module' ( 'namespace' NCName '=' )? URILiteral ( 'at' URILiteral ( ',' URILiteral )* )?
    ;

NamespaceDecl
    : 'declare' 'namespace' NCName '=' URILiteral
    ;

DefaultNamespaceDecl
    : 'declare' 'default' ( 'element' | 'function' ) 'namespace' URILiteral
    ;

ImportStylesheetDecl
    : 'import' 'stylesheet' 'at' StringLiteral
    ;

AnnotatedDecl
    : 'declare' Annotation* ( VarDecl | FunctionDecl )
    ;

Annotation
    : '%' EQName ( '(' Literal ( ',' Literal )* ')' )? | 'private'
    ;

VarDecl
    : 'variable' '$' VarName TypeDeclaration? ( ': =' VarValue | 'external' ( ': =' VarDefaultValue )? )
    ;

VarValue
    : ExprSingle
    ;

VarDefaultValue
    : ExprSingle
    ;

ContextItemDecl
    : 'declare' 'context' 'item' ( 'as' ItemType )? ( ': =' VarValue | 'external' ( ': =' VarDefaultValue )? )
    ;

FunctionDecl
    : 'function' EQName '(' ParamList? ')' ( 'as' SequenceType )? ( FunctionBody | 'external' )
    ;

ParamList
    : Param ( ',' Param )*
    ;

Param
    : '$' EQName TypeDeclaration?
    ;

FunctionBody
    : EnclosedExpr
    ;

EnclosedExpr
    : '{' Expr '}'
    ;

OptionDecl
    : 'declare' 'option' EQName StringLiteral
    ;

QueryBody
    : Expr
    ;

Expr
    : ExprSingle ( ',' ExprSingle )*
    ;

ExprSingle
    : FLWORExpr
    | QuantifiedExpr
    | SwitchExpr
    | TypeswitchExpr
    | IfExpr
    | OrExpr
    | TryCatchExpr
    | InsertExpr
    | DeleteExpr
    | RenameExpr
    | ReplaceExpr
    | TransformExpr
    ;

EnclosedExprExtended
    : '{' Expr? '}'
    ;

FLWORExpr
    : InitialClause IntermediateClause* ReturnClause
    ;

InitialClause
    : ForClause
    | LetClause
    | WindowClause
    ;

IntermediateClause
    : InitialClause
    | WhereClause
    | GroupByClause
    | OrderByClause
    | CountClause
    ;

ForClause
    : 'for' ForBinding ( ',' ForBinding )*
    ;

ForBinding
    : '$' VarName TypeDeclaration? AllowingEmpty? PositionalVar? 'in' ExprSingle
    ;

AllowingEmpty
    : 'allowing' 'empty'
    ;

PositionalVar
    : 'at' '$' VarName
    ;

LetClause
    : 'let' LetBinding ( ',' LetBinding )*
    ;

LetBinding
    : '$' VarName TypeDeclaration? ': =' ExprSingle
    ;

WindowClause
    : 'for' ( TumblingWindowClause | SlidingWindowClause )
    ;

TumblingWindowClause
    : 'tumbling' 'window' '$' VarName TypeDeclaration? 'in' ExprSingle WindowStartCondition WindowEndCondition?
    ;

SlidingWindowClause
    : 'sliding' 'window' '$' VarName TypeDeclaration? 'in' ExprSingle WindowStartCondition WindowEndCondition
    ;

WindowStartCondition
    : 'start' WindowVars 'when' ExprSingle
    ;

WindowEndCondition
    : 'only'? 'end' WindowVars 'when' ExprSingle
    ;

WindowVars
    : ( '$' CurrentItem )? PositionalVar? ( 'previous' '$' PreviousItem )? ( 'next' '$' NextItem )?
    ;

CurrentItem
    : EQName
    ;

PreviousItem
    : EQName
    ;

NextItem : EQName
    ;

CountClause
    : 'count' '$' VarName
    ;

WhereClause
    : 'where' ExprSingle
    ;

GroupByClause
    : 'group' 'by' GroupingSpecList
    ;

GroupingSpecList
    : GroupingSpec ( ',' GroupingSpec )*
    ;

GroupingSpec
    : '$' VarName ( 'collation' URILiteral )?
    ;

OrderByClause
    : ( 'order' 'by' | 'stable' 'order' 'by' ) OrderSpecList
    ;

OrderSpecList
    : OrderSpec ( ',' OrderSpec )*
    ;

OrderSpec
    : ExprSingle OrderModifier
    ;

OrderModifier
    : ( 'ascending' | 'descending' )? ( 'empty' ( 'greatest' | 'least' ) )? ( 'collation' URILiteral )?
    ;

ReturnClause
    : 'return' ExprSingle
    ;

QuantifiedExpr
    : ( 'some' | 'every' ) '$' VarName TypeDeclaration? 'in' ExprSingle ( ',' '$' VarName TypeDeclaration? 'in' ExprSingle )* 'satisfies' ExprSingle
    ;

SwitchExpr
    : 'switch' '(' Expr ')' SwitchCaseClause+ 'default' 'return' ExprSingle
    ;

SwitchCaseClause
    : ( 'case' SwitchCaseOperand )+ 'return' ExprSingle
    ;

SwitchCaseOperand
    : ExprSingle
    ;

TypeswitchExpr
    : 'typeswitch' '(' Expr ')' CaseClause+ 'default' ( '$' VarName )? 'return' ExprSingle
    ;

CaseClause
    : 'case' ( '$' VarName 'as' )? SequenceTypeUnion 'return' ExprSingle
    ;

SequenceTypeUnion
    : SequenceType ( '|' SequenceType )*
    ;

IfExpr
    : 'if' '(' Expr ')' 'then' ExprSingle 'else' ExprSingle
    ;

TryCatchExpr
    : TryClause CatchClause+
    ;

TryClause
    : 'try' '{' TryTargetExpr '}'
    ;

TryTargetExpr
    : Expr
    ;

CatchClause
    : 'catch' (CatchErrorList | '(' '$' VarName ')') EnclosedExprExtended
    ;

CatchErrorList
    : NameTest ( '|' NameTest )*
    ;

OrExpr   : AndExpr ( 'or' AndExpr )*
    ;
AndExpr  : ComparisonExpr ( 'and' ComparisonExpr )*
    ;


ComparisonExpr	   : FTContainsExpr ( (ValueComp
    | GeneralComp
    | NodeComp) FTContainsExpr )?
    ;

FTContainsExpr	   : StringConcatExpr ( 'contains' 'text' FTSelection FTIgnoreOption? )?
    ;



StringConcatExpr : RangeExpr ( '||' RangeExpr )*
    ;

RangeExpr
    : AdditiveExpr ( 'to' AdditiveExpr )?
    ;
AdditiveExpr
    : MultiplicativeExpr ( ( '+' | '-' ) MultiplicativeExpr )*
    ;

MultiplicativeExpr
    : UnionExpr ( ( '*' | 'div' | 'idiv' | 'mod' ) UnionExpr )*
    ;

UnionExpr
    : IntersectExceptExpr ( ( 'union' | '|' ) IntersectExceptExpr )*
    ;

IntersectExceptExpr
    : InstanceofExpr ( ( 'intersect' | 'except' ) InstanceofExpr )*
    ;

InstanceofExpr
    : TreatExpr ( 'instance' 'of' SequenceType )?
    ;

TreatExpr
    : CastableExpr ( 'treat' 'as' SequenceType )?
    ;

CastableExpr
    : CastExpr ( 'castable' 'as' SingleType )?
    ;

CastExpr : UnaryExpr ( 'cast' 'as' SingleType )?
    ;

UnaryExpr
    : ( '-' | '+' )* ValueExpr
    ;

ValueExpr
    : ValidateExpr
    | PathExpr
    | ExtensionExpr
    ;

GeneralComp
    : '='
    | '!='
    | '<'
    | '<='
    | '>'
    | '>='
    ;

ValueComp
    : 'eq'
    | 'ne'
    | 'lt'
    | 'le'
    | 'gt'
    | 'ge'
    ;

NodeComp : 'is'
    | '<<'
    | '>>'
    ;

ValidateExpr
    : 'validate' ( ValidationMode | ('as'|'type') TypeName )? 'full'? '{' Expr '}'
    ;

ValidationMode
    : 'lax'
    | 'strict'
    ;

ExtensionExpr
    : Pragma+ '{' Expr? '}'
    ;

Pragma   : '(#' S? EQName ( S PragmaContents )? '#)'
          /* ws: explicit */
    ;

PathExpr : '/' ( RelativePathExpr / )
    | '//' RelativePathExpr
    | RelativePathExpr
    ;

RelativePathExpr
    : StepExpr ( ( '/' | '//' | '!' ) StepExpr )*
    ;

StepExpr : PostfixExpr
    | AxisStep
    ;

AxisStep : ( ReverseStep | ForwardStep ) PredicateList
    ;

ForwardStep
    : ForwardAxis NodeTest
    | AbbrevForwardStep
    ;

ForwardAxis
    : 'child' ': : '
    | 'descendant' ': : '
    | 'attribute' ': : '
    | 'namespace' ': : '
    | 'self' ': : '
    | 'property' ': : '
    | 'descendant-or-self' ': : '
    | 'following-sibling' ': : '
    | 'following' ': : '
    ;

AbbrevForwardStep
    : '@'? NodeTest
    ;

ReverseStep
    : ReverseAxis NodeTest
    | AbbrevReverseStep
    ;

ReverseAxis
    : 'parent' ': : '
    | 'ancestor' ': : '
    | 'preceding-sibling' ': : '
    | 'preceding' ': : '
    | 'ancestor-or-self' ': : '
    ;

AbbrevReverseStep
    : '..'
    ;

NodeTest : KindTest
    | NameTest
    ;

NameTest : EQName
    | Wildcard
    ;

PostfixExpr
    : PrimaryExpr ( Predicate | ArgumentList )*
    ;

ArgumentList
    : '(' ( Argument ( ',' Argument )* )? ')'
    ;

PredicateList
    : Predicate*
    ;

Predicate
    : '[' Expr ']'
    ;

PrimaryExpr
    : Literal
    | VarRef
    | ParenthesizedExpr
    | ContextItemExpr
    | FunctionCall
    | OrderedExpr
    | UnorderedExpr
    | Constructor
    | FunctionItemExpr
    ;

Literal  : NumericLiteral
    | StringLiteral
    ;

NumericLiteral
    : IntegerLiteral
    | DecimalLiteral
    | DoubleLiteral
    ;

VarRef   : '$' VarName
    ;

VarName  : EQName
    ;

ParenthesizedExpr
    : '(' Expr? ')'
    ;

ContextItemExpr
    : '.'
    ;

OrderedExpr
    : 'ordered' '{' Expr '}'
    ;

UnorderedExpr
    : 'unordered' '{' Expr '}'
    ;

FunctionCall
    : FunctionName ArgumentList
    ;

Argument : ExprSingle
    | ArgumentPlaceholder
    ;

ArgumentPlaceholder
    : '?'
    ;

Constructor
    : DirectConstructor
    | ComputedConstructor
    ;

DirectConstructor
    : DirElemConstructor
    | DirCommentConstructor
    | DirPIConstructor
    ;

DirElemConstructor
    : '<' QName DirAttributeList ( '/>' | '>' DirElemContent* '</' QName S? '>' )
          /* ws: explicit */
    ;

DirAttributeList
    : ( S ( QName S? '=' S? DirAttributeValue )? )*
          /* ws: explicit */
    ;

DirAttributeValue
    : '"' ( EscapeQuot | QuotAttrValueContent )* '"'
    | '\'' ( EscapeApos | AposAttrValueContent )* '\''
          /* ws: explicit */
    ;

QuotAttrValueContent
    : QuotAttrContentChar
    | CommonContent
    ;

AposAttrValueContent
    : AposAttrContentChar
    | CommonContent
    ;

DirElemContent
    : DirectConstructor
    | CDataSection
    | CommonContent
    | ElementContentChar
    ;

CommonContent
    : PredefinedEntityRef
    | CharRef
    | '{{'
    | '}}'
    | EnclosedExpr
    ;

DirCommentConstructor
    : '<!--' DirCommentContents '-->'
          /* ws: explicit */
    ;

DirPIConstructor
    : '<?' PITarget ( S DirPIContents )? '?>'
          /* ws: explicit */
    ;

CDataSection
    : '<![CDATA[' CDataSectionContents ']]>'
          /* ws: explicit */
    ;

ComputedConstructor
    : CompDocConstructor
    | CompElemConstructor
    | CompAttrConstructor
    | CompTextConstructor
    | CompCommentConstructor
    | CompPIConstructor
    | CompBinaryConstructor
    | CompNamespaceConstructor
    ;


CompBinaryConstructor : 'binary' EnclosedExprExtended
    ;


CompDocConstructor
    : 'document' '{' Expr '}'
    ;

CompElemConstructor
    : 'element' ( EQName | '{' Expr '}' ) '{' ContentExpr? '}'
    ;

ContentExpr
    : Expr
    ;

CompAttrConstructor
    : 'attribute' ( EQName | '{' Expr '}' ) '{' Expr? '}'
    ;

CompNamespaceConstructor
    : 'namespace' ( Prefix | '{' PrefixExpr '}' ) EnclosedExprExtended
    ;

Prefix   : NCName
    ;

PrefixExpr
    : Expr
    ;

CompTextConstructor
    : 'text' EnclosedExprExtended
    ;

CompCommentConstructor
    : 'comment' '{' Expr '}'
    ;

CompPIConstructor
    : 'processing-instruction' ( NCName | '{' Expr '}' ) '{' Expr? '}'
    ;

FunctionItemExpr
    : LiteralFunctionItem
    | InlineFunction
    ;

LiteralFunctionItem
    : EQName '#' IntegerLiteral
    ;

InlineFunction
    : 'function' '(' ParamList? ')' ( 'as' SequenceType )? EnclosedExpr
    ;

SingleType
    : AtomicOrUnionType ('?'|'*'^OccurrenceIndicator)?
    ;

TypeDeclaration
    : 'as' SequenceType
    ;

SequenceType
    : 'empty-sequence' '(' ')'
    | ItemType ( OccurrenceIndicator / )
    ;

OccurrenceIndicator
    : '?'
    | '*'^OccurrenceIndicator
    | '+'
    ;

ItemType : KindTest
    | 'item' '(' ')'
    | FunctionTest
    | AtomicOrUnionType
    | ParenthesizedItemType
    ;

AtomicOrUnionType
    : EQName
    ;

KindTest : DocumentTest
    | ElementTest
    | AttributeTest
    | SchemaElementTest
    | SchemaAttributeTest
    | PITest
    | CommentTest
    | TextTest
    | NamespaceNodeTest
    | AnyKindTest
    | BinaryTest
    ;

BinaryTest
    : 'binary' '(' ')'
    ;

AnyKindTest
    : 'node' '(' ')'
    ;

DocumentTest
    : 'document-node' '(' ( ElementTest | SchemaElementTest )? ')'
    ;

TextTest : 'text' '(' ')'
    ;

CommentTest
    : 'comment' '(' ')'
    ;

NamespaceNodeTest
    : 'namespace-node' '(' ')'
    ;

PITest   : 'processing-instruction' '(' ( NCName | StringLiteral )? ')'
    ;

AttributeTest
    : 'attribute' '(' ( AttribNameOrWildcard ( ',' TypeName )? )? ')'
    ;

AttribNameOrWildcard
    : AttributeName
    | '*'
    ;

SchemaAttributeTest
    : 'schema-attribute' '(' AttributeDeclaration ')'
    ;

AttributeDeclaration
    : AttributeName
    ;

ElementTest
    : 'element' '(' ( ElementNameOrWildcard ( ',' TypeName '?'? )? )? ')'
    ;

ElementNameOrWildcard
    : ElementName
    | '*'
    ;

SchemaElementTest
    : 'schema-element' '(' ElementDeclaration ')'
    ;

ElementDeclaration
    : ElementName
    ;

AttributeName
    : EQName
    ;

ElementName
    : EQName
    ;

TypeName : EQName
    ;

FunctionTest
    : Annotation* ( AnyFunctionTest | TypedFunctionTest )
    ;

AnyFunctionTest
    : 'function' '(' '*' ')'
    ;

TypedFunctionTest
    : 'function' '(' ( SequenceType ( ',' SequenceType )* )? ')' 'as' SequenceType
    ;

ParenthesizedItemType
    : '(' ItemType ')'
    ;

RevalidationDecl
    : 'declare' 'revalidation' ( 'strict' | 'lax' | 'skip' )
    ;

InsertExprTargetChoice
    : ( 'as' ( 'first' | 'last' ) )? 'into'
    | 'after'
    | 'before'
    ;

InsertExpr
    : 'insert' ( 'node' | 'nodes' ) SourceExpr InsertExprTargetChoice TargetExpr
    ;

DeleteExpr
    : 'delete' ( 'node' | 'nodes' ) TargetExpr
    ;

ReplaceExpr
    : 'replace' ( 'value' 'of' )? 'node' TargetExpr 'with' ExprSingle
    ;

RenameExpr
    : 'rename' 'node' TargetExpr 'as' NewNameExpr
    ;

SourceExpr
    : ExprSingle
    ;

TargetExpr
    : ExprSingle
    ;

NewNameExpr
    : ExprSingle
    ;

TransformExpr
    : 'copy' '$' VarName ': =' ExprSingle ( ',' '$' VarName ': =' ExprSingle )* 'modify' ExprSingle 'return' ExprSingle
    ;

FTOptionDecl
    : 'declare' 'ft-option' FTMatchOptions
    ;

FTSelection
    : FTOr FTPosFilter*
    ;

FTWeight
    : 'weight' '{' Expr '}'
    ;

FTOr
    : FTAnd ( 'ftor' FTAnd )*
    ;

FTAnd
    : FTMildNot ( 'ftand' FTMildNot )*
    ;

FTMildNot
    : FTUnaryNot ( 'not' 'in' FTUnaryNot )*
    ;

FTUnaryNot
    : ('ftnot')? FTPrimaryWithOptions
    ;

FTPrimaryWithOptions
    : FTPrimary FTMatchOptions? FTWeight?
    ;

FTPrimary
    : (FTWords FTTimes?) | ('(' FTSelection ')') | FTExtensionSelection
    ;

FTWords
    : FTWordsValue FTAnyallOption?
    ;

FTWordsValue
    : StringLiteral | ('{' Expr '}')
    ;

FTExtensionSelection
    : Pragma+ '{' FTSelection? '}'
    ;

FTAnyallOption
    : ('any' 'word'?) | ('all' 'words'?) | 'phrase'
    ;

FTTimes
    : 'occurs' FTRange 'times'
    ;

FTRange
    : ('exactly' AdditiveExpr)
    | ('at' 'least' AdditiveExpr)
    | ('at' 'most' AdditiveExpr)
    | ('from' AdditiveExpr 'to' AdditiveExpr)
    ;

FTPosFilter
    : FTOrder | FTWindow | FTDistance | FTScope | FTContent
    ;

FTOrder
    : 'ordered'
    ;

FTWindow
    : 'window' AdditiveExpr FTUnit
    ;

FTDistance
    : 'distance' FTRange FTUnit
    ;

FTUnit
    : 'words' | 'sentences' | 'paragraphs'
    ;

FTScope
    : ('same' | 'different') FTBigUnit
    ;

FTBigUnit
    : 'sentence' | 'paragraph'
    ;

FTContent
    : ('at' 'start') | ('at' 'end') | ('entire' 'content')
    ;

FTMatchOptions
    : ('using' FTMatchOption)+
    ;

FTMatchOption
    : FTLanguageOption
    | FTWildCardOption
    | FTThesaurusOption
    | FTStemOption
    | FTCaseOption
    | FTDiacriticsOption
    | FTStopWordOption
    | FTExtensionOption
    ;

FTCaseOption
    : ('case' 'insensitive')
    | ('case' 'sensitive')
    | 'lowercase'
    | 'uppercase'
    ;

FTDiacriticsOption
    : ('diacritics' 'insensitive')
    | ('diacritics' 'sensitive')
    ;

FTStemOption
    : 'stemming' | ('no' 'stemming')
    ;

FTThesaurusOption
    : ('thesaurus' (FTThesaurusID | 'default'))
    | ('thesaurus' '(' (FTThesaurusID | 'default') (',' FTThesaurusID)* ')')
    | ('no' 'thesaurus')
    ;

FTThesaurusID
    : 'at' URILiteral ('relationship' StringLiteral)? (FTLiteralRange 'levels')?
    ;

FTLiteralRange
    : ('exactly' IntegerLiteral)
    | ('at' 'least' IntegerLiteral)
    | ('at' 'most' IntegerLiteral)
    | ('from' IntegerLiteral 'to' IntegerLiteral)
    ;

FTStopWordOption
    : ('stop' 'words' FTStopWords FTStopWordsInclExcl*)
    | ('stop' 'words' 'default' FTStopWordsInclExcl*)
    | ('no' 'stop' 'words')
    ;

FTStopWords
    : ('at' URILiteral)
    | ('(' StringLiteral (',' StringLiteral)* ')')
    ;

FTStopWordsInclExcl
    : ('union' | 'except') FTStopWords
    ;

FTLanguageOption
    : 'language' StringLiteral
    ;

FTWildCardOption
    : 'wildcards' | ('no' 'wildcards')
    ;

FTExtensionOption
    : 'option' EQName StringLiteral
    ;

FTIgnoreOption
    : 'without' 'content' UnionExpr
    ;

eqName
    : qName
    | uriQualifiedName
    ;

qName
    : functionQName
    | 'attribute'
    | 'binary'
    | 'comment'
    | 'document-node'
    | 'element'
    | 'empty-sequence'
    | 'function'
    | 'if'
    | 'item'
    | 'namespace-node'
    | 'node'
    | 'processing-instruction'
    | 'schema-attribute'
    | 'schema-element'
    | 'switch'
    | 'text'
    | 'typeswitch'
    ;

functionName
    : functionQName
    | uriQualifiedName
    ;

FunctionQName
    : QName^Token
    | 'contains'
    | 'paragraphs'
    | 'sentences'
    | 'times'
    | 'words'
    | 'after'
    | 'as'
    | 'before'
    | 'copy'
    | 'delete'
    | 'first'
    | 'insert'
    | 'into'
    | 'last'
    | 'modify'
    | 'rename'
    | 'replace'
    | 'with'
    | 'private'
    | 'ancestor'
    | 'ancestor-or-self'
    | 'and'
    | 'ascending'
    | 'case'
    | 'cast'
    | 'castable'
    | 'catch'
    | 'child'
    | 'collation'
    | 'count'
    | 'declare'
    | 'default'
    | 'descendant'
    | 'descendant-or-self'
    | 'descending'
    | 'div'
    | 'document'
    | 'else'
    | 'empty'
    | 'end'
    | 'eq'
    | 'every'
    | 'except'
    | 'following'
    | 'following-sibling'
    | 'for'
    | 'ge'
    | 'group'
    | 'gt'
    | 'idiv'
    | 'import'
    | 'instance'
    | 'intersect'
    | 'is'
    | 'le'
    | 'let'
    | 'lt'
    | 'mod'
    | 'module'
    | 'namespace'
    | 'ne'
    | 'only'
    | 'or'
    | 'order'
    | 'ordered'
    | 'parent'
    | 'preceding'
    | 'preceding-sibling'
    | 'property'
    | 'return'
    | 'satisfies'
    | 'self'
    | 'some'
    | 'stable'
    | 'start'
    | 'to'
    | 'treat'
    | 'try'
    | 'union'
    | 'unordered'
    | 'validate'
    | 'where'
    | 'xquery'
ncName   : NCName^Token
    | 'contains'
    | 'paragraphs'
    | 'sentences'
    | 'times'
    | 'words'
    | 'after'
    | 'as'
    | 'before'
    | 'into'
    | 'modify'
    | 'with'
    | 'and'
    | 'ascending'
    | 'case'
    | 'cast'
    | 'castable'
    | 'collation'
    | 'count'
    | 'default'
    | 'descending'
    | 'div'
    | 'else'
    | 'empty'
    | 'end'
    | 'eq'
    | 'except'
    | 'for'
    | 'ge'
    | 'group'
    | 'gt'
    | 'idiv'
    | 'instance'
    | 'intersect'
    | 'is'
    | 'le'
    | 'let'
    | 'lt'
    | 'mod'
    | 'ne'
    | 'only'
    | 'or'
    | 'order'
    | 'return'
    | 'satisfies'
    | 'stable'
    | 'start'
    | 'to'
    | 'treat'
    | 'try'
    | 'union'
    | 'where'
    ;
Whitespace
    : S^WS
    | Comment
          /* ws: definition */

Comment  : '(: ' ( CommentContents | Comment )* ': )'
          /* ws: explicit */

<?TOKENS?>

EOF      : $

PragmaContents
    : ( Char* - ( Char* '#)' Char* ) ) & '#'

DirCommentContents
    : ( ( Char - '-' ) | '-' ( Char - '-' ) )*
          /* ws: explicit */

DirPIContents
    : ( Char* - ( Char* '?>' Char* ) ) & '?'
          /* ws: explicit */

CDataSectionContents
    : ( Char* - ( Char* ']]>' Char* ) ) & ']]'
          /* ws: explicit */
    ;

wildcard : '*'
    | NCName ': ' '*'
    | '*' ': ' NCName
    | URILiteral ': ' '*'
          /* ws: explicit */
    ;

uriQualifiedName
    : URILiteral ': ' NCName
          /* ws: explicit */
    ;

uriLiteral
    : StringLiteral
    ;

integerLiteral
    : Digits
    ;

decimalLiteral
    : '.' Digits
    | Digits '.' [0-9]*
          /* ws: explicit */
    ;

doubleLiteral
    : ( '.' Digits | Digits ( '.' [0-9]* )? ) [eE] [+#x2D]? Digits
          /* ws: explicit */
    ;

stringLiteral
    : '"' ( PredefinedEntityRef | CharRef | EscapeQuot | [^\"&] )* '"'
    | '\'' ( PredefinedEntityRef | CharRef | EscapeApos | [^\'&] )* '\''
          /* ws: explicit */
    ;

predefinedEntityRef
    : '&' [A-Za-z]+ ';'
          /* ws: explicit */
    ;

EscapeQuot
    : '""'
    ;

EscapeApos
    : '\'\''
    ;

ElementContentChar
    : Char - [{}<&]
    ;

QuotAttrContentChar
    : Char - ["{}<&]
    ;

AposAttrContentChar
    : Char - ['{}<&]
    ;

PITarget
    : Name - ( ( 'X' | 'x' ) ( 'M' | 'm' ) ( 'L' | 'l' ) )
    ;

NameStartChar
    : ': '
    | [A-Z]
    | '_'
    | [a-z]
    | [#x00C0-#x00D6]
    | [#x00D8-#x00F6]
    | [#x00F8-#x02FF]
    | [#x0370-#x037D]
    | [#x037F-#x1FFF]
    | [#x200C-#x200D]
    | [#x2070-#x218F]
    | [#x2C00-#x2FEF]
    | [#x3001-#xD7FF]
    | [#xF900-#xFDCF]
    | [#xFDF0-#xFFFD]
    | [#x10000-#xEFFFF]
    ;

NameChar
    : NameStartChar
    | '-'
    | '.'
    | [0-9]
    | #x00B7
    | [#x0300-#x036F]
    | [#x203F-#x2040]
    ;

Name
    : NameStartChar NameChar*
    ;

CharRef  : '&#' [0-9]+ ';'
    | '&#x' [0-9a-fA-F]+ ';'
    ;

NCName   : Name - ( Char* ': ' Char* )
;

QName    : PrefixedName
    | UnprefixedName
    ;

PrefixedName
    : Prefix ': ' LocalPart
    ;

UnprefixedName
    : LocalPart
    ;

Prefix
    : NCName
    ;

LocalPart
    : NCName
    ;

S
    : ( #x0020 | #x0009 | #x000D | #x000A )+
    ;

Char
    : #x0009
    | #x000A
    | #x000D
    | [#x0020-#xD7FF]
    | [#xE000-#xFFFD]
    | [#x10000-#x10FFFF]
    ;

Digits
    : [0-9]+
    ;

CommentContents
    : ( ( [^(: ] | '('+ [^(: ] | ': '+ [^: )] )+ | '(' ) '('* & '('
    | ( ( [^(: ] | '('+ [^(: ] | ': '+ [^: )] )+ | ': ' ) ': '* & ': '
'*'       << Wildcard '*'^OccurrenceIndicator
QName^Token
          << 'contains' 'paragraphs' 'sentences' 'times' 'words' 'after' 'as' 'before' 'copy' 'delete' 'first' 'insert' 'into' 'last' 'modify' 'rename' 'replace' 'with' 'private' 'ancestor' 'ancestor-or-self' 'and' 'ascending' 'attribute' 'binary' 'catch' 'case' 'cast' 'castable' 'child' 'collation' 'comment' 'count' 'declare' 'default' 'descendant' 'descendant-or-self' 'descending' 'div' 'document' 'document-node' 'element' 'else' 'empty' 'empty-sequence' 'end' 'eq' 'every' 'except' 'following' 'following-sibling' 'for' 'function' 'ge' 'group' 'gt' 'idiv' 'if' 'import' 'instance' 'intersect' 'is' 'item' 'le' 'let' 'lt' 'mod' 'module' 'namespace' 'namespace-node' 'ne' 'node' 'only' 'or' 'order' 'ordered' 'parent' 'preceding' 'preceding-sibling' 'processing-instruction' 'property' 'return' 'satisfies' 'schema-attribute' 'schema-element' 'self' 'some' 'stable' 'start' 'switch' 'text' 'to' 'treat' 'try' 'typeswitch' 'union' 'unordered' 'validate' 'where' 'xquery'
NCName^Token
          << 'contains' 'paragraphs' 'sentences' 'times' 'words' 'after' 'as' 'before' 'into' 'modify' 'with' 'try' 'and' 'ascending' 'case' 'cast' 'castable' 'collation' 'count' 'default' 'descending' 'div' 'else' 'empty' 'end' 'eq' 'except' 'for' 'ge' 'group' 'gt' 'idiv' 'instance' 'intersect' 'is' 'le' 'let' 'lt' 'mod' 'ne' 'only' 'or' 'order' 'return' 'satisfies' 'stable' 'start' 'to' 'treat' 'union' 'where'
