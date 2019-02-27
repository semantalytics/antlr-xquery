grammar xquery10;

/* [1] */
IntegerLiteral
    : Digits
    ;

/* [2] */
DecimalLiteral
    : ('.' Digits) |  (Digits '.' [0-9]*)
    ;

/* [3] */
DoubleLiteral
    : (('.' Digits) |  (Digits ('.' [0-9]*)?)) ('e' | 'E') ('+' | '-')? Digits
    ;

/* [4] */
StringLiteral
    //(ws: significant)
    : ('"' (('"' '"') |  [^"])* '"') |  ('\'' (('\'' '\'') |  [^'])* '\'')
    ;

/* [5] */
URLLiteral
    //(ws: significant)
    : ('"' (('"' '"') |  [^"])* '"') |  ('\'' (('\'' '\'') |  [^'])* '\'')
    ;

/* [6] */
ExprComment
    : '{--' [^}]* '--}'
    ;

/* [7] */
S
    : WhitespaceChar+
    ;

/* [8] */
Nmstart
    : Letter
    |  '_'
    ;

/* [9] */
Nmchar
    : Letter
    | CombiningChar
    |  Extender
    |  Digit
    |  '.'
    |  '-'
    |  '_'
    ;

/* [10] */
Digits
    : [0-9]+
    ;

/* [11] */
EscapeQuot
    : '"' '"'
    ;

/* [12] */
PITarget
    : NCName
    ;

/* [13] */
VarName
    : QName
    ;

/* [14] */
NCNameForPrefix
    : Nmstart Nmchar*
    ;

/* [15] */
PredefinedEntityRef
    : '&' ('lt' |  'gt' | 'amp' | 'quot' | 'apos') ';'
    ;

/* [16] */
HexDigits
    : ([0-9] | [a-f] | [A-F])+
    ;

/* [17] */
CharRef
    : '&#' (Digits |  ('x' HexDigits)) ';'
    ;

/* [18] */
EscapeApos
    : '\'\''
    ;

/* [19] */
Char
    : '\u0009'
    | '\u000D'
    | '\u000A'
    | '\u0020'..'\uFFFD'
    ;

/* [20] */
WhitespaceChar
    : '\u0009'
    | '\u000D'
    | '\u000A'
    | '\u0020'
    ;












queryList
    : module ( '%%%' ( module )? )* EOF
    ;

module
    : ( versionDecl )? ( libraryModule | mainModule )
    ;

versionDecl
    : "xquery" ( "version" <StringLiteral> ( "encoding" <StringLiteral> )? ) Separator
    ;

mainModule
    : Prolog QueryBody
    ;

libraryModule 	::= 	ModuleDecl Prolog
ModuleDecl 	::= 	"module" "namespace" NCName "=" URILiteral Separator
Prolog 	::= 	( ( DefaultNamespaceDecl | Setter | NamespaceDecl | Import | FTOptionDecl ) Separator )* ( ( VarDecl | FunctionDecl | OptionDecl ) Separator )*
Separator 	::= 	";"
Setter 	::= 	( BoundarySpaceDecl | DefaultCollationDecl | BaseURIDecl | ConstructionDecl | OrderingModeDecl | EmptyOrderDecl | CopyNamespacesDecl )
BoundarySpaceDecl 	::= 	"declare" "boundary-space" ( "preserve" | "strip" )
DefaultCollationDecl 	::= 	"declare" "default" "collation" URILiteral
BaseURIDecl 	::= 	"declare" "base-uri" URILiteral
ConstructionDecl 	::= 	"declare" "construction" ( "strip" | "preserve" )
OrderingModeDecl 	::= 	"declare" "ordering" ( "ordered" | "unordered" )
EmptyOrderDecl 	::= 	"declare" "default" "order" "empty" ( <Greatest> | <Least> )
CopyNamespacesDecl 	::= 	"declare" "copy-namespaces" PreserveMode "," InheritMode
PreserveMode 	::= 	( "preserve" | "no-preserve" )
InheritMode 	::= 	( "inherit" | "no-inherit" )
Import 	::= 	( SchemaImport | ModuleImport )
SchemaImport 	::= 	"import" "schema" ( SchemaPrefix )? URILiteral ( "at" URILiteral ( "," URILiteral )* )?
SchemaPrefix 	::= 	( ( "namespace" NCName "=" ) | ( "default" "element" "namespace" ) )
ModuleImport 	::= 	"import" "module" ( "namespace" NCName "=" )? URILiteral ( "at" URILiteral ( "," URILiteral )* )?
NamespaceDecl 	::= 	"declare" "namespace" NCName "=" URILiteral
DefaultNamespaceDecl 	::= 	"declare" "default" ( "element" | "function" ) "namespace" URILiteral
FTOptionDecl 	::= 	"declare" "ft-option" FTMatchOptions
VarDecl 	::= 	"declare" "variable" "$" QName ( TypeDeclaration )? ( ( ":=" ExprSingle ) | ( <External> ) )
FunctionDecl 	::= 	"declare" "function" FunctionQName "(" ( ParamList )? ")" ( "as" SequenceType )? ( EnclosedExpr | <External> )
ParamList 	::= 	Param ( "," Param )*
Param 	::= 	"$" QName ( TypeDeclaration )?
EnclosedExpr 	::= 	<Lbrace> Expr <Rbrace>
OptionDecl 	::= 	"declare" "option" QName <StringLiteral>
QueryBody 	::= 	Expr
Expr 	::= 	ExprSingle ( "," ExprSingle )*
ExprSingle 	::= 	( FLWORExpr10 | QuantifiedExpr | TypeswitchExpr | IfExpr | OrExpr )
FLWORExpr10 	::= 	( ( ForClause | LetClause ) )+ ( WhereClause )? ( OrderByClause )? "return" ExprSingle
ForClause 	::= 	"for" "$" VarName ( TypeDeclaration )? ( PositionalVar )? ( FTScoreVar )? "in" ExprSingle ( "," "$" VarName ( TypeDeclaration )? ( PositionalVar )? ( FTScoreVar )? "in" ExprSingle )*
PositionalVar 	::= 	"at" "$" VarName
FTScoreVar 	::= 	"score" "$" VarName
LetClause 	::= 	"let" ( ( "$" VarName ( TypeDeclaration )? ) | FTScoreVar ) ":=" ExprSingle ( "," ( ( "$" VarName ( TypeDeclaration )? ) | FTScoreVar ) ":=" ExprSingle )*
WhereClause 	::= 	"where" ExprSingle
OrderByClause 	::= 	( ( "order" "by" ) | ( "stable" "order" "by" ) ) OrderSpecList
OrderSpecList 	::= 	OrderSpec ( "," OrderSpec )*
OrderSpec 	::= 	ExprSingle OrderModifier
OrderModifier 	::= 	( ( <Ascending> | <Descending> ) )? ( "empty" ( <Greatest> | <Least> ) )? ( "collation" URILiteral )?
QuantifiedExpr 	::= 	( "some" | "every" ) "$" VarName ( TypeDeclaration )? "in" ExprSingle ( "," "$" VarName ( TypeDeclaration )? "in" ExprSingle )* "satisfies" ExprSingle
TypeswitchExpr 	::= 	"typeswitch" "(" Expr ")" ( CaseClause )+ "default" ( "$" VarName )? "return" ExprSingle
CaseClause 	::= 	"case" ( "$" VarName "as" )? SequenceType "return" ExprSingle
IfExpr 	::= 	"if" "(" Expr ")" "then" ExprSingle "else" ExprSingle
OperatorExpr 	::= 	OrExpr
OrExpr 	::= 	AndExpr ( "or" AndExpr )*
AndExpr 	::= 	ComparisonExpr ( "and" ComparisonExpr )*
ComparisonExpr 	::= 	FTContainsExpr ( ( ValueComp | GeneralComp | NodeComp ) FTContainsExpr )?
FTContainsExpr 	::= 	RangeExpr ( ( "contains" "text" FTSelection ( FTIgnoreOption )? ) )?
RangeExpr 	::= 	AdditiveExpr ( "to" AdditiveExpr )?
AdditiveExpr 	::= 	MultiplicativeExpr ( ( <Plus> | <Minus> ) MultiplicativeExpr )*
MultiplicativeExpr 	::= 	UnionExpr ( ( "*" | "div" | "idiv" | "mod" ) UnionExpr )*
UnionExpr 	::= 	IntersectExceptExpr ( ( "union" | "|" ) IntersectExceptExpr )*
IntersectExceptExpr 	::= 	InstanceofExpr ( ( "intersect" | "except" ) InstanceofExpr )*
InstanceofExpr 	::= 	TreatExpr ( ( "instance" "of" SequenceType ) )?
TreatExpr 	::= 	CastableExpr ( ( "treat" "as" SequenceType ) )?
CastableExpr 	::= 	CastExpr ( ( "castable" "as" SingleType ) )?
CastExpr 	::= 	UnaryExpr ( ( "cast" "as" SingleType ) )?
UnaryExpr 	::= 	( ( <Minus> | <Plus> ) )* ValueExpr
ValueExpr 	::= 	( ValidateExpr | PathExpr | ExtensionExpr )
GeneralComp 	::= 	( "=" | "!=" | <LeftAngleBracket> | "<=" | ">" | ">=" )
ValueComp 	::= 	( "eq" | "ne" | "lt" | "le" | "gt" | "ge" )
NodeComp 	::= 	( "is" | "<<" | ">>" )
ValidateExpr 	::= 	"validate" ( ValidationMode )? <Lbrace> Expr <Rbrace>
ValidationMode 	::= 	( "lax" | "strict" )
ExtensionExpr 	::= 	( Pragma )+ <Lbrace> ( Expr )? <Rbrace>
Pragma 	::= 	<PragmaOpen> ( <S> )? QName ( <S> PragmaContents )? <PragmaClose>
PragmaContents 	::= 	( <Char> )*
PathExpr 	::= 	( ( <Slash> ( RelativePathExpr )? ) | ( <SlashSlash> RelativePathExpr ) | RelativePathExpr )
RelativePathExpr 	::= 	StepExpr ( ( <Slash> | <SlashSlash> ) StepExpr )*
StepExpr 	::= 	( FilterExpr | AxisStep )
AxisStep 	::= 	( ReverseStep | ForwardStep ) PredicateList
ForwardStep 	::= 	( ( ForwardAxis NodeTest ) | AbbrevForwardStep )
ForwardAxis 	::= 	( ( "child" "::" ) | ( "descendant" "::" ) | ( "attribute" "::" ) | ( "self" "::" ) | ( "descendant-or-self" "::" ) | ( "following-sibling" "::" ) | ( "following" "::" ) )
AbbrevForwardStep 	::= 	( "@" )? NodeTest
ReverseStep 	::= 	( ( ReverseAxis NodeTest ) | AbbrevReverseStep )
ReverseAxis 	::= 	( ( "parent" "::" ) | ( "ancestor" "::" ) | ( "preceding-sibling" "::" ) | ( "preceding" "::" ) | ( "ancestor-or-self" "::" ) )
AbbrevReverseStep 	::= 	".."
NodeTest 	::= 	( KindTest | NameTest )
NameTest 	::= 	( QName | Wildcard )
Wildcard 	::= 	( "*" | <NCNameColonStar> | <StarColonNCName> )
FilterExpr 	::= 	PrimaryExpr PredicateList
PredicateList 	::= 	( Predicate )*
Predicate 	::= 	"[" Expr "]"
PrimaryExpr 	::= 	( Literal | VarRef | ParenthesizedExpr | ContextItemExpr | FunctionCall | OrderedExpr | UnorderedExpr | Constructor )
Literal 	::= 	( NumericLiteral | <StringLiteral> )
NumericLiteral 	::= 	( <IntegerLiteral> | <DecimalLiteral> | <DoubleLiteral> )
VarRef 	::= 	"$" VarName
VarName 	::= 	QName
ParenthesizedExpr 	::= 	"(" ( Expr )? ")"
ContextItemExpr 	::= 	"."
OrderedExpr 	::= 	"ordered" <Lbrace> Expr <Rbrace>
UnorderedExpr 	::= 	"unordered" <Lbrace> Expr <Rbrace>
FunctionCall 	::= 	FunctionQName ( "(" ( ExprSingle ( "," ExprSingle )* )? ")" )
Constructor 	::= 	( DirectConstructor | ComputedConstructor )
DirectConstructor 	::= 	( DirElemConstructor | DirCommentConstructor | DirPIConstructor )
DirElemConstructor 	::= 	<LeftAngleBracket> <TagQName> DirAttributeList ( <EmptyTagClose> | ( <StartTagClose> ( DirElemContent )* <EndTagOpen> <EndTagQName> ( <S> )? <EndTagClose> ) )
DirAttributeList 	::= 	( <S> ( <TagQName> ( <S> )? <ValueIndicator> ( <S> )? DirAttributeValue )? )*
DirAttributeValue 	::= 	( ( <OpenQuot> ( ( <EscapeQuot> | QuotAttrValueContent ) )* <CloseQuot> ) | ( <OpenApos> ( ( <EscapeApos> | AposAttrValueContent ) )* <CloseApos> ) )
QuotAttrValueContent 	::= 	( <QuotAttrContentChar> | CommonContent )
AposAttrValueContent 	::= 	( <AposAttrContentChar> | CommonContent )
DirElemContent 	::= 	( DirectConstructor | CDataSection | CommonContent | <ElementContentChar> )
CommonContent 	::= 	( <PredefinedEntityRef> | <CharRef> | <LCurlyBraceEscape> | <RCurlyBraceEscape> | EnclosedExpr )
DirCommentConstructor 	::= 	<DirCommentStart> DirCommentContents <DirCommentEnd>
DirCommentContents 	::= 	( ( <DirCommentContentChar> | <DirCommentContentDashChar> ) )*
DirPIConstructor 	::= 	<ProcessingInstructionStart> <PITarget> ( <S> DirPIContents )? <ProcessingInstructionEnd>
DirPIContents 	::= 	( <Char> )*
CDataSection 	::= 	<CdataSectionStart> CDataSectionContents <CdataSectionEnd>
CDataSectionContents 	::= 	( <Char> )*
ComputedConstructor 	::= 	( CompDocConstructor | CompElemConstructor | CompAttrConstructor | CompTextConstructor | CompCommentConstructor | CompPIConstructor )
CompDocConstructor 	::= 	"document" <Lbrace> Expr <Rbrace>
CompElemConstructor 	::= 	"element" ( QName | ( <Lbrace> Expr <Rbrace> ) ) <Lbrace> ( ContentExpr )? <Rbrace>
ContentExpr 	::= 	Expr
CompAttrConstructor 	::= 	"attribute" ( QName | ( <Lbrace> Expr <Rbrace> ) ) <Lbrace> ( Expr )? <Rbrace>
CompTextConstructor 	::= 	"text" <Lbrace> Expr <Rbrace>
CompCommentConstructor 	::= 	"comment" <Lbrace> Expr <Rbrace>
CompPIConstructor 	::= 	"processing-instruction" ( NCName | ( <Lbrace> Expr <Rbrace> ) ) <Lbrace> ( Expr )? <Rbrace>
SingleType 	::= 	AtomicType ( "?" )?
TypeDeclaration 	::= 	"as" SequenceType
SequenceType 	::= 	( ( "empty-sequence" "(" ")" ) | ( ItemType ( OccurrenceIndicator )? ) )
OccurrenceIndicator 	::= 	( "?" | "*" | <Plus> )
ItemType 	::= 	( KindTest | ( "item" "(" ")" ) | AtomicType )
AtomicType 	::= 	QName
KindTest 	::= 	( DocumentTest | ElementTest | AttributeTest | SchemaElementTest | SchemaAttributeTest | PITest | CommentTest | TextTest | AnyKindTest )
AnyKindTest 	::= 	"node" "(" ")"
DocumentTest 	::= 	"document-node" "(" ( ( ElementTest | SchemaElementTest ) )? ")"
TextTest 	::= 	"text" "(" ")"
CommentTest 	::= 	"comment" "(" ")"
PITest 	::= 	"processing-instruction" "(" ( ( NCName | <StringLiteral> ) )? ")"
AttributeTest 	::= 	"attribute" "(" ( AttribNameOrWildcard ( "," TypeName )? )? ")"
AttribNameOrWildcard 	::= 	( AttributeName | "*" )
SchemaAttributeTest 	::= 	"schema-attribute" "(" AttributeDeclaration ")"
AttributeDeclaration 	::= 	AttributeName
ElementTest 	::= 	"element" "(" ( ElementNameOrWildcard ( "," TypeName ( "?" )? )? )? ")"
ElementNameOrWildcard 	::= 	( ElementName | "*" )
SchemaElementTest 	::= 	"schema-element" "(" ElementDeclaration ")"
ElementDeclaration 	::= 	ElementName
AttributeName 	::= 	QName
ElementName 	::= 	QName
TypeName 	::= 	QName
URILiteral 	::= 	<StringLiteral>
FTSelection 	::= 	FTOr ( FTPosFilter )*
FTWeight 	::= 	"weight" <Lbrace> Expr <Rbrace>
FTOrExpr 	::= 	FTOr
FTOr 	::= 	FTAnd ( "ftor" FTAnd )*
FTAnd 	::= 	FTMildNot ( "ftand" FTMildNot )*
FTMildNot 	::= 	FTUnaryNot ( ( "not" "in" ) FTUnaryNot )*
FTUnaryNot 	::= 	( ( "ftnot" ) )? FTPrimaryWithOptions
FTPrimaryWithOptions 	::= 	( FTPrimary ( FTMatchOptions )? ( FTWeight )? )
FTPrimary 	::= 	( ( FTWords ( FTTimes )? ) | ( "(" FTSelection ")" ) | FTExtensionSelection )
FTWords 	::= 	FTWordsValue ( FTAnyallOption )?
FTWordsValue 	::= 	( <StringLiteral> | ( <Lbrace> Expr <Rbrace> ) )
FTExtensionSelection 	::= 	( Pragma )+ <Lbrace> ( FTSelection )? <Rbrace>
FTAnyallOption 	::= 	( ( "any" ( "word" )? ) | ( "all" ( "words" )? ) | "phrase" )
FTTimes 	::= 	"occurs" FTRange "times"
FTRange 	::= 	( ( "exactly" AdditiveExpr ) | ( "at" <Least> AdditiveExpr ) | ( "at" "most" AdditiveExpr ) | ( "from" AdditiveExpr "to" AdditiveExpr ) )
FTPosFilter 	::= 	( FTOrder | FTWindow | FTDistance | FTScope | FTContent )
FTOrder 	::= 	"ordered"
FTWindow 	::= 	"window" AdditiveExpr FTUnit
FTDistance 	::= 	"distance" FTRange FTUnit
FTUnit 	::= 	( "words" | "sentences" | "paragraphs" )
FTScope 	::= 	( "same" | "different" ) FTBigUnit
FTBigUnit 	::= 	( "sentence" | "paragraph" )
FTContent 	::= 	( ( "at" "start" ) | ( "at" "end" ) | ( "entire" "content" ) )
FTMatchOptions 	::= 	( "using" FTMatchOption )+
FTMatchOption 	::= 	( FTLanguageOption | FTWildCardOption | FTThesaurusOption | FTStemOption | FTCaseOption | FTDiacriticsOption | FTStopWordOption | FTExtensionOption )
FTCaseOption 	::= 	( ( "case" "insensitive" ) | ( "case" "sensitive" ) | "lowercase" | "uppercase" )
FTDiacriticsOption 	::= 	( ( "diacritics" "insensitive" ) | ( "diacritics" "sensitive" ) )
FTStemOption 	::= 	( "stemming" | ( "no" "stemming" ) )
FTThesaurusOption 	::= 	( ( "thesaurus" ( FTThesaurusID | "default" ) ) | ( "thesaurus" "(" ( FTThesaurusID | "default" ) ( "," FTThesaurusID )* ")" ) | ( "no" "thesaurus" ) )
FTThesaurusID 	::= 	"at" URILiteral ( "relationship" <StringLiteral> )? ( FTLiteralRange "levels" )?
FTLiteralRange 	::= 	( ( "exactly" <IntegerLiteral> ) | ( "at" <Least> <IntegerLiteral> ) | ( "at" "most" <IntegerLiteral> ) | ( "from" <IntegerLiteral> "to" <IntegerLiteral> ) )
FTStopWordOption 	::= 	( ( "stop" "words" FTStopWords ( FTStopWordsInclExcl )* ) | ( "stop" "words" "default" ( FTStopWordsInclExcl )* ) | ( "no" "stop" "words" ) )
FTStopWords 	::= 	( ( "at" URILiteral ) | ( "(" <StringLiteral> ( "," <StringLiteral> )* ")" ) )
FTStopWordsInclExcl 	::= 	( "union" | "except" ) FTStopWords
FTLanguageOption 	::= 	"language" <StringLiteral>
FTWildCardOption 	::= 	( "wildcards" | ( "no" "wildcards" ) )
FTExtensionOption 	::= 	"option" QName <StringLiteral>
FTIgnoreOption 	::= 	"without" "content" UnionExpr
NCName 	::= 	QName
QName 	::= 	( FunctionQName | "attribute" | "comment" | "document-node" | "element" | "empty-sequence" | "if" | "item" | "node" | "processing-instruction" | "schema-attribute" | "schema-element" | "text" | "typeswitch" )
FunctionQName 	::= 	( <QNameToken> | <Ascending> | <Descending> | <External> | <Greatest> | <Least> | "all" | "ancestor" | "ancestor-or-self" | "and" | "any" | "as" | "at" | "base-uri" | "boundary-space" | "by" | "case" | "cast" | "castable" | "child" | "collation" | "construction" | "contains" | "content" | "copy-namespaces" | "declare" | "default" | "descendant" | "descendant-or-self" | "diacritics" | "different" | "distance" | "div" | "document" | "else" | "empty" | "encoding" | "end" | "entire" | "eq" | "every" | "exactly" | "except" | "following" | "following-sibling" | "for" | "from" | "ft-option" | "ftand" | "ftnot" | "ftor" | "function" | "ge" | "gt" | "idiv" | "import" | "in" | "inherit" | "insensitive" | "instance" | "intersect" | "is" | "language" | "lax" | "le" | "let" | "levels" | "lowercase" | "lt" | "mod" | "module" | "most" | "namespace" | "ne" | "no" | "no-inherit" | "no-preserve" | "not" | "occurs" | "of" | "option" | "or" | "order" | "ordered" | "ordering" | "paragraph" | "paragraphs" | "parent" | "phrase" | "preceding" | "preceding-sibling" | "preserve" | "relationship" | "return" | "same" | "satisfies" | "schema" | "score" | "self" | "sensitive" | "sentence" | "sentences" | "some" | "stable" | "start" | "stemming" | "stop" | "strict" | "strip" | "then" | "thesaurus" | "times" | "to" | "treat" | "union" | "unordered" | "uppercase" | "using" | "validate" | "variable" | "version" | "weight" | "where" | "wildcards" | "window" | "without" | "word" | "words" | "xquery" )