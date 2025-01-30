# D Shark yo90c
D Grammar
Contents [hide]

    Interpolation Expression Sequence
    Modules
    Declarations
    Types
    Attributes
    Pragmas
    Expressions
    Statements
    Arrays
    Structs and Unions
    Classes
    Interfaces
    Enums
    Functions
    Templates
    Template Mixins
    Conditional Compilation
    Traits
    Unit Tests
    D x86 Inline Assembler
    Named Character Entities
    Application Binary Interface
    ImportC

Lexical Syntax

Refer to the page for lexical syntax.
Interpolation Expression Sequence

InterpolationExpressionSequence:
    InterpolatedDoubleQuotedLiteral
    InterpolatedWysiwygLiteral
    InterpolatedTokenLiteral

InterpolatedDoubleQuotedLiteral:
    i" InterpolatedDoubleQuotedCharactersopt "

InterpolatedDoubleQuotedCharacters:
    InterpolatedDoubleQuotedCharacter
    InterpolatedDoubleQuotedCharacter InterpolatedDoubleQuotedCharacters

InterpolatedDoubleQuotedCharacter:
    DoubleQuotedCharacter
    InterpolationEscapeSequence
    InterpolationExpression

InterpolationEscapeSequence:
    EscapeSequence
    \$

InterpolationExpression:
    $( AssignExpression )

InterpolatedWysiwygLiteral:
    i` InterpolatedWysiwygCharactersopt `

InterpolatedWysiwygCharacters:
    InterpolatedWysiwygCharacter
    InterpolatedWysiwygCharacter InterpolatedWysiwygCharacters

InterpolatedWysiwygCharacter:
    WysiwygCharacter
    InterpolationExpression

InterpolatedTokenLiteral:
    iq{ InterpolatedTokenStringTokensopt }

InterpolatedTokenStringTokens:
    InterpolatedTokenStringToken
    InterpolatedTokenStringToken InterpolatedTokenStringTokens

InterpolatedTokenStringToken:
    InterpolatedTokenNoBraces
    { InterpolatedTokenStringTokensopt }

InterpolatedTokenNoBraces:
    TokenNoBraces
    InterpolationExpression

Modules

Module:
    ModuleDeclaration
    ModuleDeclaration DeclDefs
    DeclDefs

DeclDefs:
    DeclDef
    DeclDef DeclDefs

DeclDef:
    AttributeSpecifier
    Declaration
    Constructor
    Destructor
    Postblit
    Invariant
    UnitTest
    AliasThis
    StaticConstructor
    StaticDestructor
    SharedStaticConstructor
    SharedStaticDestructor
    ConditionalDeclaration
    DebugSpecification
    VersionSpecification
    MixinDeclaration
    EmptyDeclaration

EmptyDeclaration:
    ;

ModuleDeclaration:
    ModuleAttributesopt module ModuleFullyQualifiedName ;

ModuleAttributes:
    ModuleAttribute
    ModuleAttribute ModuleAttributes

ModuleAttribute:
    DeprecatedAttribute
    UserDefinedAttribute

ModuleFullyQualifiedName:
    ModuleName
    Packages . ModuleName

ModuleName:
    Identifier

Packages:
    PackageName
    Packages . PackageName

PackageName:
    Identifier

ImportDeclaration:
    import ImportList ;
    static import ImportList ;

ImportList:
    Import
    ImportBindings
    Import , ImportList

Import:
    ModuleFullyQualifiedName
    ModuleAliasIdentifier = ModuleFullyQualifiedName

ImportBindings:
    Import : ImportBindList

ImportBindList:
    ImportBind
    ImportBind , ImportBindList

ImportBind:
    Identifier
    Identifier = Identifier

ModuleAliasIdentifier:
    Identifier

MixinDeclaration:
    mixin ( ArgumentList ) ;

Declarations

Declaration:
    FuncDeclaration
    VarDeclarations
    AliasDeclaration
    AliasAssign
    AggregateDeclaration
    EnumDeclaration
    ImportDeclaration
    ConditionalDeclaration
    StaticForeachDeclaration
    StaticAssert
    TemplateDeclaration
    TemplateMixinDeclaration
    TemplateMixin

AggregateDeclaration:
    ClassDeclaration
    InterfaceDeclaration
    StructDeclaration
    UnionDeclaration

VarDeclarations:
    StorageClassesopt BasicType TypeSuffixesopt IdentifierInitializers ;
    AutoDeclaration

IdentifierInitializers: 
    IdentifierInitializer
    IdentifierInitializer , IdentifierInitializers

IdentifierInitializer: 
    Identifier
    Identifier TemplateParametersopt = Initializer

Declarator: 
    TypeSuffixesopt Identifier

StorageClasses:
    StorageClass
    StorageClass StorageClasses

StorageClass:
    LinkageAttribute
    AlignAttribute
    AtAttribute
    deprecated
    enum
    static
    extern
    abstract
    final
    override
    synchronized
    auto
    scope
    const
    immutable
    inout
    shared
    __gshared
    Property
    nothrow
    pure
    ref

Initializer:
    VoidInitializer
    NonVoidInitializer

NonVoidInitializer:
    ArrayInitializer
    StructInitializer
    AssignExpression

VoidInitializer:
    void

AutoDeclaration:
    StorageClasses AutoAssignments ;

AutoAssignments:
    AutoAssignment
    AutoAssignments , AutoAssignment

AutoAssignment:
    Identifier TemplateParametersopt = Initializer

AliasDeclaration:
    alias StorageClassesopt BasicType TypeSuffixesopt Identifiers ;
    alias StorageClassesopt BasicType FuncDeclarator ;
    alias AliasAssignments ;

Identifiers:
    Identifier
    Identifier , Identifiers

AliasAssignments:
    AliasAssignment
    AliasAssignments , AliasAssignment

AliasAssignment:
    Identifier TemplateParametersopt = StorageClassesopt Type
    Identifier TemplateParametersopt = FunctionLiteral
    Identifier TemplateParametersopt = StorageClassesopt Type Parameters MemberFunctionAttributesopt

AliasAssign:
    Identifier = Type

AliasReassignment:
    Identifier = StorageClassesopt Type
    Identifier = FunctionLiteral
    Identifier = StorageClassesopt BasicType Parameters MemberFunctionAttributesopt

Types

Type:
    TypeCtorsopt BasicType TypeSuffixesopt

TypeCtors:
    TypeCtor
    TypeCtor TypeCtors

TypeCtor:
    const
    immutable
    inout
    shared

BasicType:
    FundamentalType
    . QualifiedIdentifier
    QualifiedIdentifier
    Typeof
    Typeof . QualifiedIdentifier
    TypeCtor ( Type )
    Vector
    TraitsExpression
    MixinType

Vector:
    __vector ( VectorBaseType )

VectorBaseType:
    Type

FundamentalType:
    bool
    byte
    ubyte
    short
    ushort
    int
    uint
    long
    ulong
    cent
    ucent
    char
    wchar
    dchar
    float
    double
    real
    ifloat
    idouble
    ireal
    cfloat
    cdouble
    creal
    void

TypeSuffixes:
    TypeSuffix TypeSuffixesopt

TypeSuffix:
    *
    [ ]
    [ AssignExpression ]
    [ AssignExpression .. AssignExpression ]
    [ Type ]
    delegate Parameters MemberFunctionAttributesopt
    function Parameters FunctionAttributesopt

QualifiedIdentifier:
    Identifier
    Identifier . QualifiedIdentifier
    TemplateInstance
    TemplateInstance . QualifiedIdentifier
    Identifier [ AssignExpression ]
    Identifier [ AssignExpression ] . QualifiedIdentifier

StorageClassesopt Type Parameters FunctionAttributesopt

Type function Parameters FunctionAttributesopt
Type delegate Parameters MemberFunctionAttributesopt

Typeof:
    typeof ( Expression )
    typeof ( return )

MixinType:
    mixin ( ArgumentList )

Attributes

AttributeSpecifier:
    Attribute :
    Attribute DeclarationBlock

Attribute:
    AlignAttribute
    AtAttribute
    DeprecatedAttribute
    FunctionAttributeKwd
    LinkageAttribute
    Pragma
    VisibilityAttribute
    abstract
    auto
    const
    final
    __gshared
    extern
    immutable
    inout
    override
    ref
    return
    scope
    shared
    static
    synchronized

FunctionAttributeKwd:
    nothrow
    pure

AtAttribute:
    @ disable
    @ __future
    @ nogc
    @ live
    Property
    @ safe
    @ system
    @ trusted
    UserDefinedAttribute

Property:
    @ property

DeclarationBlock:
    DeclDef
    { DeclDefsopt }

LinkageAttribute:
    extern ( LinkageType )
    extern ( C ++ , )
    extern ( C ++ , QualifiedIdentifier )
    extern ( C ++ , NamespaceList )
    extern ( C ++ , class )
    extern ( C ++ , struct )

LinkageType:
    C
    C ++
    D
    Windows
    System
    Objective - C

NamespaceList:
    ConditionalExpression
    ConditionalExpression,
    ConditionalExpression, NamespaceList

AlignAttribute:
    align
    align ( AssignExpression )

DeprecatedAttribute:
    deprecated
    deprecated ( AssignExpression )

VisibilityAttribute:
    export
    package
    package ( QualifiedIdentifier )
    private
    protected
    public

UserDefinedAttribute:
    @ ( TemplateArgumentList )
    @ TemplateSingleArgument
    @ Identifier ( NamedArgumentListopt )
    @ TemplateInstance
    @ TemplateInstance ( NamedArgumentListopt )

Pragmas

PragmaDeclaration:
    Pragma ;
    Pragma DeclarationBlock

PragmaStatement:
    Pragma ;
    Pragma NoScopeStatement

Pragma:
    pragma ( Identifier )
    pragma ( Identifier , ArgumentList )

Expressions

Expression:
    CommaExpression

CommaExpression:
    AssignExpression
    CommaExpression , AssignExpression

AssignExpression:
    ConditionalExpression
    ConditionalExpression = AssignExpression
    ConditionalExpression += AssignExpression
    ConditionalExpression -= AssignExpression
    ConditionalExpression *= AssignExpression
    ConditionalExpression /= AssignExpression
    ConditionalExpression %= AssignExpression
    ConditionalExpression &= AssignExpression
    ConditionalExpression |= AssignExpression
    ConditionalExpression ^= AssignExpression
    ConditionalExpression ~= AssignExpression
    ConditionalExpression <<= AssignExpression
    ConditionalExpression >>= AssignExpression
    ConditionalExpression >>>= AssignExpression
    ConditionalExpression ^^= AssignExpression

ConditionalExpression:
    OrOrExpression
    OrOrExpression ? Expression : ConditionalExpression

OrOrExpression:
    AndAndExpression
    OrOrExpression || AndAndExpression

AndAndExpression:
    OrExpression
    AndAndExpression && OrExpression

OrExpression:
    XorExpression
    OrExpression | XorExpression

XorExpression:
    AndExpression
    XorExpression ^ AndExpression

AndExpression:
    CmpExpression
    AndExpression & CmpExpression

CmpExpression:
    EqualExpression
    IdentityExpression
    RelExpression
    InExpression
    ShiftExpression

EqualExpression:
    ShiftExpression == ShiftExpression
    ShiftExpression != ShiftExpression

IdentityExpression:
    ShiftExpression is ShiftExpression
    ShiftExpression ! is ShiftExpression

RelExpression:
    ShiftExpression < ShiftExpression
    ShiftExpression <= ShiftExpression
    ShiftExpression > ShiftExpression
    ShiftExpression >= ShiftExpression

InExpression:
    ShiftExpression in ShiftExpression
    ShiftExpression ! in ShiftExpression

ShiftExpression:
    AddExpression
    ShiftExpression << AddExpression
    ShiftExpression >> AddExpression
    ShiftExpression >>> AddExpression

AddExpression:
    MulExpression
    AddExpression + MulExpression
    AddExpression - MulExpression
    AddExpression ~ MulExpression

MulExpression:
    UnaryExpression
    MulExpression * UnaryExpression
    MulExpression / UnaryExpression
    MulExpression % UnaryExpression

UnaryExpression:
    & UnaryExpression
    ++ UnaryExpression
    -- UnaryExpression
    * UnaryExpression
    - UnaryExpression
    + UnaryExpression
    ! UnaryExpression
    ComplementExpression
    DeleteExpression
    CastExpression
    ThrowExpression
    PowExpression

ComplementExpression:
    ~ UnaryExpression

DeleteExpression:
    delete UnaryExpression

CastExpression:
    cast ( Type ) UnaryExpression
    CastQual

CastQual:
    cast ( TypeCtorsopt ) UnaryExpression

ThrowExpression:
    throw AssignExpression

PowExpression:
    PostfixExpression
    PostfixExpression ^^ UnaryExpression

PostfixExpression:
    PrimaryExpression
    PostfixExpression . Identifier
    PostfixExpression . TemplateInstance
    PostfixExpression . NewExpression
    PostfixExpression ++
    PostfixExpression --
    PostfixExpression ( NamedArgumentListopt )
    TypeCtorsopt BasicType ( NamedArgumentListopt )
    PostfixExpression IndexOperation
    PostfixExpression SliceOperation

ArgumentList:
    AssignExpression
    AssignExpression ,
    AssignExpression , ArgumentList

NamedArgumentList:
    NamedArgument
    NamedArgument ,
    NamedArgument , NamedArgumentList

NamedArgument:
    Identifier : AssignExpression
    AssignExpression

IndexOperation:
    [ ArgumentList ]

SliceOperation:
    [ ]
    [ Slice ]
    [ Slice , ]

Slice:
    AssignExpression
    AssignExpression , Slice
    AssignExpression .. AssignExpression
    AssignExpression .. AssignExpression , Slice

PrimaryExpression:
    Identifier
    . Identifier
    TemplateInstance
    . TemplateInstance
    $
    LiteralExpression
    AssertExpression
    MixinExpression
    ImportExpression
    NewExpression
    FundamentalType . Identifier
    TypeCtoropt ( Type ) . Identifier
    ( Type ) . TemplateInstance
    FundamentalType ( NamedArgumentListopt )
    TypeCtoropt ( Type ) ( NamedArgumentListopt )
    Typeof
    TypeidExpression
    IsExpression
    ( Expression )
    SpecialKeyword
    TraitsExpression

LiteralExpression:
    this
    super
    null
    true
    false
    IntegerLiteral
    FloatLiteral
    CharacterLiteral
    StringLiteral
    InterpolationExpressionSequence
    ArrayLiteral
    AssocArrayLiteral
    FunctionLiteral

ArrayLiteral:
    [ ArgumentListopt ]

AssocArrayLiteral:
    [ KeyValuePairs ]

KeyValuePairs:
    KeyValuePair
    KeyValuePair , KeyValuePairs

KeyValuePair:
    KeyExpression : ValueExpression

KeyExpression:
    AssignExpression

ValueExpression:
    AssignExpression

FunctionLiteral:
    function RefOrAutoRefopt Typeopt ParameterWithAttributesopt FunctionLiteralBody
    delegate RefOrAutoRefopt Typeopt ParameterWithMemberAttributesopt FunctionLiteralBody
    RefOrAutoRefopt ParameterWithMemberAttributes FunctionLiteralBody
    BlockStatement
    Identifier => AssignExpression

ParameterWithAttributes:
    Parameters FunctionAttributesopt

ParameterWithMemberAttributes:
    Parameters MemberFunctionAttributesopt

FunctionLiteralBody:
    => AssignExpression
    SpecifiedFunctionBody

RefOrAutoRef:
    ref
    auto ref

AssertExpression:
    assert ( AssertArguments )

AssertArguments:
    AssignExpression
    AssignExpression ,
    AssignExpression , AssignExpression
    AssignExpression , AssignExpression ,

MixinExpression:
    mixin ( ArgumentList )

ImportExpression:
    import ( AssignExpression )

NewExpression:
    new Type
    new Type [ AssignExpression ]
    new Type ( NamedArgumentListopt )
    NewAnonClassExpression

TypeidExpression:
    typeid ( Type )
    typeid ( Expression )

IsExpression:
    is ( Type )
    is ( Type : TypeSpecialization )
    is ( Type == TypeSpecialization )
    is ( Type : TypeSpecialization , TemplateParameterList )
    is ( Type == TypeSpecialization , TemplateParameterList )
    is ( Type Identifier )
    is ( Type Identifier : TypeSpecialization )
    is ( Type Identifier == TypeSpecialization )
    is ( Type Identifier : TypeSpecialization , TemplateParameterList )
    is ( Type Identifier == TypeSpecialization , TemplateParameterList )


TypeSpecialization:
    Type
    TypeCtor
    struct
    union
    class
    interface
    enum
    __vector
    function
    delegate
    super
    return
    __parameters
    module
    package

is ( Type : TypeSpecialization , TemplateParameterList )
is ( Type == TypeSpecialization , TemplateParameterList )
is ( Type Identifier : TypeSpecialization , TemplateParameterList )
is ( Type Identifier == TypeSpecialization , TemplateParameterList )

SpecialKeyword:
    __FILE__
    __FILE_FULL_PATH__
    __MODULE__
    __LINE__
    __FUNCTION__
    __PRETTY_FUNCTION__

Statements

Statement:
    EmptyStatement
    NonEmptyStatement
    ScopeBlockStatement

EmptyStatement:
    ;

NoScopeNonEmptyStatement:
    NonEmptyStatement
    BlockStatement

NoScopeStatement:
    EmptyStatement
    NonEmptyStatement
    BlockStatement

NonEmptyOrScopeBlockStatement:
    NonEmptyStatement
    ScopeBlockStatement

NonEmptyStatement:
    NonEmptyStatementNoCaseNoDefault
    CaseStatement
    CaseRangeStatement
    DefaultStatement

NonEmptyStatementNoCaseNoDefault:
    LabeledStatement
    ExpressionStatement
    DeclarationStatement
    IfStatement
    WhileStatement
    DoStatement
    ForStatement
    ForeachStatement
    SwitchStatement
    FinalSwitchStatement
    ContinueStatement
    BreakStatement
    ReturnStatement
    GotoStatement
    WithStatement
    SynchronizedStatement
    TryStatement
    ScopeGuardStatement
    AsmStatement
    MixinStatement
    ForeachRangeStatement
    PragmaStatement
    ConditionalStatement
    StaticForeachStatement
    ImportDeclaration

ScopeStatement:
    NonEmptyStatement
    BlockStatement

ScopeBlockStatement:
    BlockStatement

LabeledStatement:
    Identifier :
    Identifier : Statement

BlockStatement:
    { }
    { StatementList }

StatementList:
    Statement
    Statement StatementList

ExpressionStatement:
    Expression ;

DeclarationStatement:
    StorageClassesopt Declaration

IfStatement:
    if ( IfCondition ) ThenStatement
    if ( IfCondition ) ThenStatement else ElseStatement

IfCondition:
    Expression
    auto Identifier = Expression
    scope Identifier = Expression
    TypeCtors Identifier = Expression
    TypeCtorsopt BasicType Declarator = Expression

ThenStatement:
    ScopeStatement

ElseStatement:
    ScopeStatement

WhileStatement:
    while ( IfCondition ) ScopeStatement

DoStatement:
    do ScopeStatement  while ( Expression ) ;

ForStatement:
    for ( Initialize Testopt ; Incrementopt ) ScopeStatement

Initialize:
    ;
    NoScopeNonEmptyStatement

Test:
    Expression

Increment:
    Expression

AggregateForeach:
    Foreach ( ForeachTypeList ; ForeachAggregate )

ForeachStatement:
    AggregateForeach NoScopeNonEmptyStatement

Foreach:
    foreach
    foreach_reverse

ForeachTypeList:
    ForeachType
    ForeachType , ForeachTypeList

ForeachType:
    ForeachTypeAttributesopt BasicType Declarator
    ForeachTypeAttributesopt Identifier
    ForeachTypeAttributesopt alias Identifier

ForeachTypeAttributes:
    ForeachTypeAttribute
    ForeachTypeAttribute ForeachTypeAttributes

ForeachTypeAttribute:
    enum
    ref
    scope
    TypeCtor

ForeachAggregate:
    Expression

OpApplyDeclaration:
    int opApply ( scope int delegate ( OpApplyParameters ) dg ) ;

OpApplyParameters:
    OpApplyParameter
    OpApplyParameter, OpApplyParameters

OpApplyParameter:
    ForeachTypeAttributesopt BasicType Declarator

RangeForeach:
    Foreach ( ForeachType ; LwrExpression .. UprExpression )

LwrExpression:
    Expression

UprExpression:
    Expression

ForeachRangeStatement:
    RangeForeach ScopeStatement

SwitchStatement:
    switch ( IfCondition ) ScopeStatement

CaseStatement:
    case ArgumentList : ScopeStatementListopt

DefaultStatement:
    default : ScopeStatementListopt

ScopeStatementList:
    StatementListNoCaseNoDefault

StatementListNoCaseNoDefault:
    StatementNoCaseNoDefault
    StatementNoCaseNoDefault StatementListNoCaseNoDefault

StatementNoCaseNoDefault:
    EmptyStatement
    NonEmptyStatementNoCaseNoDefault
    ScopeBlockStatement

CaseRangeStatement:
    case FirstExp : .. case LastExp : ScopeStatementListopt

FirstExp:
    AssignExpression

LastExp:
    AssignExpression

FinalSwitchStatement:
    final switch ( IfCondition ) ScopeStatement

ContinueStatement:
    continue Identifieropt ;

BreakStatement:
    break Identifieropt ;

ReturnStatement:
    return Expressionopt ;

GotoStatement:
    goto Identifier ;
    goto default ;
    goto case ;
    goto case Expression ;

WithStatement:
    with ( Expression ) ScopeStatement
    with ( Symbol ) ScopeStatement
    with ( TemplateInstance ) ScopeStatement

SynchronizedStatement:
    synchronized ScopeStatement
    synchronized ( Expression ) ScopeStatement

TryStatement:
    try ScopeStatement Catches
    try ScopeStatement Catches FinallyStatement
    try ScopeStatement FinallyStatement

Catches:
    Catch
    Catch Catches

Catch:
    catch ( CatchParameter ) NoScopeNonEmptyStatement

CatchParameter:
    BasicType Identifieropt

FinallyStatement:
    finally NoScopeNonEmptyStatement

ScopeGuardStatement:
    scope ( exit ) NonEmptyOrScopeBlockStatement
    scope ( success ) NonEmptyOrScopeBlockStatement
    scope ( failure ) NonEmptyOrScopeBlockStatement

AsmStatement:
    asm FunctionAttributesopt { AsmInstructionListopt }

AsmInstructionList:
    AsmInstruction ;
    AsmInstruction ; AsmInstructionList

MixinStatement:
    mixin ( ArgumentList ) ;

Arrays

ArrayInitializer:
    [ ArrayElementInitializersopt ]

ArrayElementInitializers:
    ArrayElementInitializer
    ArrayElementInitializer ,
    ArrayElementInitializer , ArrayElementInitializers

ArrayElementInitializer:
    NonVoidInitializer
    AssignExpression : NonVoidInitializer

Structs and Unions

StructDeclaration:
    struct Identifier ;
    struct Identifier AggregateBody
    StructTemplateDeclaration
    AnonStructDeclaration

AnonStructDeclaration:
    struct AggregateBody

UnionDeclaration:
    union Identifier ;
    union Identifier AggregateBody
    UnionTemplateDeclaration
    AnonUnionDeclaration

AnonUnionDeclaration:
    union AggregateBody

AggregateBody:
    { DeclDefsopt }

StructInitializer:
    { StructMemberInitializersopt }

StructMemberInitializers:
    StructMemberInitializer
    StructMemberInitializer ,
    StructMemberInitializer , StructMemberInitializers

StructMemberInitializer:
    NonVoidInitializer
    Identifier : NonVoidInitializer

Postblit:
    this ( this ) MemberFunctionAttributesopt FunctionBody

Invariant:
    invariant ( ) BlockStatement
    invariant BlockStatement
    invariant ( AssertArguments ) ;

AliasThis:
    alias Identifier this ;
    alias this = Identifier ;

Classes

ClassDeclaration:
    class Identifier ;
    class Identifier BaseClassListopt AggregateBody
    ClassTemplateDeclaration

BaseClassList:
    : SuperClassOrInterface
    : SuperClassOrInterface , Interfaces

SuperClassOrInterface:
    BasicType

Interfaces:
    Interface
    Interface , Interfaces

Interface:
    BasicType

Constructor:
    this Parameters MemberFunctionAttributesopt FunctionBody
    ConstructorTemplate

Destructor:
    ~ this ( ) MemberFunctionAttributesopt FunctionBody

StaticConstructor:
    static this ( ) MemberFunctionAttributesopt FunctionBody

StaticDestructor:
    static ~ this ( ) MemberFunctionAttributesopt FunctionBody

SharedStaticConstructor:
    shared static this ( ) MemberFunctionAttributesopt FunctionBody

SharedStaticDestructor:
    shared static ~ this ( ) MemberFunctionAttributesopt FunctionBody

Invariant:
    invariant ( ) BlockStatement
    invariant BlockStatement
    invariant ( AssertArguments ) ;

NewAnonClassExpression:
    new class ConstructorArgsopt AnonBaseClassListopt AggregateBody

ConstructorArgs:
    ( NamedArgumentListopt )

AnonBaseClassList:
    SuperClassOrInterface
    SuperClassOrInterface , Interfaces

class Identifier : AnonBaseClassList AggregateBody
// ...
new Identifier ConstructorArgs

Interfaces

InterfaceDeclaration:
    interface Identifier ;
    interface Identifier BaseInterfaceListopt AggregateBody
    InterfaceTemplateDeclaration

BaseInterfaceList:
    : Interfaces

Enums

EnumDeclaration:
    enum Identifier EnumBody
    enum Identifier : EnumBaseType EnumBody
    AnonymousEnumDeclaration

EnumBaseType:
    Type

EnumBody:
    { EnumMembers }
    ;

EnumMembers:
    EnumMember
    EnumMember ,
    EnumMember , EnumMembers

EnumMember:
    EnumMemberAttributesopt Identifier
    EnumMemberAttributesopt Identifier = AssignExpression

EnumMemberAttributes:
    EnumMemberAttribute
    EnumMemberAttribute EnumMemberAttributes

EnumMemberAttribute:
    DeprecatedAttribute
    UserDefinedAttribute
    @disable

AnonymousEnumDeclaration:
    enum : EnumBaseType { EnumMembers }
    enum { AnonymousEnumMembers }

AnonymousEnumMembers:
    AnonymousEnumMember
    AnonymousEnumMember ,
    AnonymousEnumMember , AnonymousEnumMembers

AnonymousEnumMember:
    EnumMember
    EnumMemberAttributesopt Type Identifier = AssignExpression

Functions

FuncDeclaration:
    StorageClassesopt BasicType FuncDeclarator FunctionBody
    AutoFuncDeclaration

AutoFuncDeclaration:
    StorageClasses Identifier FuncDeclaratorSuffix FunctionBody

FuncDeclarator:
    TypeSuffixesopt Identifier FuncDeclaratorSuffix

FuncDeclaratorSuffix:
    Parameters MemberFunctionAttributesopt
    TemplateParameters Parameters MemberFunctionAttributesopt Constraintopt

Parameters:
    ( ParameterListopt )

ParameterList:
    Parameter
    Parameter , ParameterListopt
    VariadicArgumentsAttributesopt ...

Parameter:
    ParameterDeclaration
    ParameterDeclaration ...
    ParameterDeclaration = AssignExpression

ParameterDeclaration:
    ParameterAttributesopt BasicType Declarator
    ParameterAttributesopt Type

ParameterAttributes:
    ParameterStorageClass
    UserDefinedAttribute
    ParameterAttributes ParameterStorageClass
    ParameterAttributes UserDefinedAttribute

ParameterStorageClass:
    auto
    TypeCtor
    final
    in
    lazy
    out
    ref
    return
    scope

VariadicArgumentsAttributes:
    VariadicArgumentsAttribute
    VariadicArgumentsAttribute VariadicArgumentsAttributes

VariadicArgumentsAttribute:
    const
    immutable
    return
    scope
    shared

FunctionAttributes:
    FunctionAttribute
    FunctionAttribute FunctionAttributes

FunctionAttribute:
    FunctionAttributeKwd
    Property
    AtAttribute

MemberFunctionAttributes:
    MemberFunctionAttribute
    MemberFunctionAttribute MemberFunctionAttributes

MemberFunctionAttribute:
    const
    immutable
    inout
    return
    scope
    shared
    FunctionAttribute

FunctionBody:
    SpecifiedFunctionBody
    ShortenedFunctionBody
    MissingFunctionBody

SpecifiedFunctionBody:
    doopt BlockStatement
    FunctionContractsopt InOutContractExpression doopt BlockStatement
    FunctionContractsopt InOutStatement do BlockStatement

ShortenedFunctionBody:
    InOutContractExpressionsopt => AssignExpression ;

MissingFunctionBody:
    ;
    FunctionContractsopt InOutContractExpression ;
    FunctionContractsopt InOutStatement

FunctionContracts:
    FunctionContract
    FunctionContract FunctionContracts

FunctionContract:
    InOutContractExpression
    InOutStatement

InOutContractExpressions:
    InOutContractExpression
    InOutContractExpression InOutContractExpressions

InOutContractExpression:
    InContractExpression
    OutContractExpression

InOutStatement:
    InStatement
    OutStatement

InContractExpression:
    in ( AssertArguments )

InStatement:
    in BlockStatement

OutContractExpression:
    out ( ; AssertArguments )
    out ( Identifier ; AssertArguments )

OutStatement:
    out BlockStatement
    out ( Identifier ) BlockStatement

        MainFunction:
            MainReturnDecl main() MainFunctionBody
            MainReturnDecl main(string[] Identifier) MainFunctionBody

        MainReturnDecl:
            void
            int
            noreturn
            auto

        MainFunctionBody:
            ShortenedFunctionBody
            SpecifiedFunctionBody
        

        CMainFunction:
            extern (C) MainReturnDecl main(CmainParametersopt) BlockStatement

        CmainParameters:
            int Identifier, char** Identifier
            int Identifier, char** Identifier, char** Identifier
        

Templates

TemplateDeclaration:
    template Identifier TemplateParameters Constraintopt { DeclDefsopt }

TemplateParameters:
    ( TemplateParameterListopt )

TemplateParameterList:
    TemplateParameter
    TemplateParameter ,
    TemplateParameter , TemplateParameterList

TemplateInstance:
    Identifier TemplateArguments

TemplateArguments:
    ! ( TemplateArgumentListopt )
    ! TemplateSingleArgument

TemplateArgumentList:
    TemplateArgument
    TemplateArgument ,
    TemplateArgument , TemplateArgumentList

TemplateSingleArgument:
    Identifier
    FundamentalType
    CharacterLiteral
    StringLiteral
    InterpolationExpressionSequence
    IntegerLiteral
    FloatLiteral
    true
    false
    null
    this
    SpecialKeyword

TemplateArgument:
    Type
    AssignExpression
    Symbol

Symbol:
    SymbolTail
    . SymbolTail

SymbolTail:
    Identifier
    Identifier . SymbolTail
    TemplateInstance
    TemplateInstance . SymbolTail

TemplateParameter:
    TemplateTypeParameter
    TemplateValueParameter
    TemplateAliasParameter
    TemplateSequenceParameter
    TemplateThisParameter

TemplateTypeParameter:
    Identifier
    Identifier TemplateTypeParameterSpecialization
    Identifier TemplateTypeParameterDefault
    Identifier TemplateTypeParameterSpecialization TemplateTypeParameterDefault

TemplateTypeParameterSpecialization:
    : Type

TemplateTypeParameterDefault:
    = Type

TemplateThisParameter:
    this TemplateTypeParameter

TemplateValueParameter:
    BasicType Declarator
    BasicType Declarator TemplateValueParameterSpecialization
    BasicType Declarator TemplateValueParameterDefault
    BasicType Declarator TemplateValueParameterSpecialization TemplateValueParameterDefault

TemplateValueParameterSpecialization:
    : ConditionalExpression

TemplateValueParameterDefault:
    = AssignExpression
    = SpecialKeyword

TemplateAliasParameter:
    alias Identifier TemplateAliasParameterSpecializationopt TemplateAliasParameterDefaultopt
    alias BasicType Declarator TemplateAliasParameterSpecializationopt TemplateAliasParameterDefaultopt

TemplateAliasParameterSpecialization:
    : Type
    : ConditionalExpression

TemplateAliasParameterDefault:
    = Type
    = ConditionalExpression

TemplateSequenceParameter:
    Identifier ...

ClassTemplateDeclaration:
    class Identifier TemplateParameters ;
    class Identifier TemplateParameters Constraintopt BaseClassListopt AggregateBody
    class Identifier TemplateParameters BaseClassListopt Constraintopt AggregateBody

InterfaceTemplateDeclaration:
    interface Identifier TemplateParameters ;
    interface Identifier TemplateParameters Constraintopt BaseInterfaceListopt AggregateBody
    interface Identifier TemplateParameters BaseInterfaceList Constraint AggregateBody

StructTemplateDeclaration:
    struct Identifier TemplateParameters ;
    struct Identifier TemplateParameters Constraintopt AggregateBody

UnionTemplateDeclaration:
    union Identifier TemplateParameters ;
    union Identifier TemplateParameters Constraintopt AggregateBody

ConstructorTemplate:
    this TemplateParameters Parameters MemberFunctionAttributesopt Constraintopt FunctionBody

Constraint:
    if ( Expression )

Template Mixins

TemplateMixinDeclaration:
    mixin template Identifier TemplateParameters Constraintopt { DeclDefsopt }

TemplateMixin:
    mixin MixinTemplateName TemplateArgumentsopt Identifieropt ;

MixinTemplateName:
    . MixinQualifiedIdentifier
    MixinQualifiedIdentifier
    Typeof . MixinQualifiedIdentifier

MixinQualifiedIdentifier:
    Identifier
    Identifier . MixinQualifiedIdentifier
    TemplateInstance . MixinQualifiedIdentifier

Conditional Compilation

ConditionalDeclaration:
    Condition DeclarationBlock
    Condition DeclarationBlock else DeclarationBlock
    Condition : DeclDefsopt
    Condition DeclarationBlock else : DeclDefsopt

ConditionalStatement:
    Condition NoScopeNonEmptyStatement
    Condition NoScopeNonEmptyStatement else NoScopeNonEmptyStatement

Condition:
    VersionCondition
    DebugCondition
    StaticIfCondition

VersionCondition:
    version ( Identifier )
    version ( unittest )
    version ( assert )

VersionSpecification:
    version = Identifier ;

DebugCondition:
    debug
    debug ( Identifier )

DebugSpecification:
    debug = Identifier ;

StaticIfCondition:
    static if ( AssignExpression )

StaticForeach:
    static AggregateForeach
    static RangeForeach

StaticForeachDeclaration:
    StaticForeach DeclarationBlock
    StaticForeach : DeclDefsopt

StaticForeachStatement:
    StaticForeach NoScopeNonEmptyStatement

StaticAssert:
    static assert ( ArgumentList ) ;

Traits

TraitsExpression:
    __traits ( TraitsKeyword , TraitsArguments )

TraitsKeyword:
    isAbstractClass
    isArithmetic
    isAssociativeArray
    isFinalClass
    isPOD
    isNested
    isFuture
    isDeprecated
    isFloating
    isIntegral
    isScalar
    isStaticArray
    isUnsigned
    isDisabled
    isVirtualFunction
    isVirtualMethod
    isAbstractFunction
    isFinalFunction
    isStaticFunction
    isOverrideFunction
    isTemplate
    isRef
    isOut
    isLazy
    isReturnOnStack
    isCopyable
    isZeroInit
    isModule
    isPackage
    hasMember
    hasCopyConstructor
    hasPostblit
    identifier
    getAliasThis
    getAttributes
    getFunctionAttributes
    getFunctionVariadicStyle
    getLinkage
    getLocation
    getMember
    getOverloads
    getParameterStorageClasses
    getPointerBitmap
    getCppNamespaces
    getVisibility
    getProtection
    getTargetInfo
    getVirtualFunctions
    getVirtualMethods
    getUnitTests
    parent
    child
    classInstanceSize
    classInstanceAlignment
    getVirtualIndex
    allMembers
    derivedMembers
    isSame
    compiles
    toType
    initSymbol
    parameters
    fullyQualifiedName

TraitsArguments:
    TraitsArgument
    TraitsArgument , TraitsArguments

TraitsArgument:
    AssignExpression
    Type

Unit Tests

UnitTest:
    unittest BlockStatement

D x86 Inline Assembler

AsmStatement:
    asm FunctionAttributesopt { AsmInstructionListopt }

AsmInstructionList:
    AsmInstruction ;
    AsmInstruction ; AsmInstructionList

AsmInstruction:
    Identifier : AsmInstruction
    align IntegerExpression
    even
    naked
    db Operands
    ds Operands
    di Operands
    dl Operands
    df Operands
    dd Operands
    de Operands
    db StringLiteral
    ds StringLiteral
    di StringLiteral
    dl StringLiteral
    dw StringLiteral
    dq StringLiteral
    Opcode
    Opcode Operands

Opcode:
    Identifier
    int
    in
    out

Operands:
    Operand
    Operand , Operands

IntegerExpression:
    IntegerLiteral
    Identifier

Register:
    AL
    AH
    AX
    EAX

    BL
    BH
    BX
    EBX

    CL
    CH
    CX
    ECX

    DL
    DH
    DX
    EDX

    BP
    EBP

    SP
    ESP

    DI
    EDI

    SI
    ESI

    ES
    CS
    SS
    DS
    GS
    FS

    CR0
    CR2
    CR3
    CR4

    DR0
    DR1
    DR2
    DR3
    DR6
    DR7

    TR3
    TR4
    TR5
    TR6
    TR7

    ST

    ST(0)
    ST(1)
    ST(2)
    ST(3)
    ST(4)
    ST(5)
    ST(6)
    ST(7)

    MM0
    MM1
    MM2
    MM3
    MM4
    MM5
    MM6
    MM7

    XMM0
    XMM1
    XMM2
    XMM3
    XMM4
    XMM5
    XMM6
    XMM7

Register64:
    RAX
    RBX
    RCX
    RDX

    BPL
    RBP

    SPL
    RSP

    DIL
    RDI

    SIL
    RSI

    R8B
    R8W
    R8D
    R8

    R9B
    R9W
    R9D
    R9

    R10B
    R10W
    R10D
    R10

    R11B
    R11W
    R11D
    R11

    R12B
    R12W
    R12D
    R12

    R13B
    R13W
    R13D
    R13

    R14B
    R14W
    R14D
    R14

    R15B
    R15W
    R15D
    R15

    XMM8
    XMM9
    XMM10
    XMM11
    XMM12
    XMM13
    XMM14
    XMM15

    YMM0
    YMM1
    YMM2
    YMM3
    YMM4
    YMM5
    YMM6
    YMM7

    YMM8
    YMM9
    YMM10
    YMM11
    YMM12
    YMM13
    YMM14
    YMM15

Operand:
    AsmExp

AsmExp:
    AsmLogOrExp
    AsmLogOrExp ? AsmExp : AsmExp

AsmLogOrExp:
    AsmLogAndExp
    AsmLogOrExp || AsmLogAndExp

AsmLogAndExp:
    AsmOrExp
    AsmLogAndExp && AsmOrExp

AsmOrExp:
    AsmXorExp
    AsmOrExp | AsmXorExp

AsmXorExp:
    AsmAndExp
    AsmXorExp ^ AsmAndExp

AsmAndExp:
    AsmEqualExp
    AsmAndExp & AsmEqualExp

AsmEqualExp:
    AsmRelExp
    AsmEqualExp == AsmRelExp
    AsmEqualExp != AsmRelExp

AsmRelExp:
    AsmShiftExp
    AsmRelExp < AsmShiftExp
    AsmRelExp <= AsmShiftExp
    AsmRelExp > AsmShiftExp
    AsmRelExp >= AsmShiftExp

AsmShiftExp:
    AsmAddExp
    AsmShiftExp << AsmAddExp
    AsmShiftExp >> AsmAddExp
    AsmShiftExp >>> AsmAddExp

AsmAddExp:
    AsmMulExp
    AsmAddExp + AsmMulExp
    AsmAddExp - AsmMulExp

AsmMulExp:
    AsmBrExp
    AsmMulExp * AsmBrExp
    AsmMulExp / AsmBrExp
    AsmMulExp % AsmBrExp

AsmBrExp:
    AsmUnaExp
    AsmBrExp [ AsmExp ]

AsmUnaExp:
    AsmTypePrefix AsmExp
    offsetof AsmExp
    seg AsmExp
    + AsmUnaExp
    - AsmUnaExp
    ! AsmUnaExp
    ~ AsmUnaExp
    AsmPrimaryExp

AsmPrimaryExp:
    IntegerLiteral
    FloatLiteral
    __LOCAL_SIZE
    $
    Register
    Register : AsmExp
    Register64
    Register64 : AsmExp
    DotIdentifier
    this

DotIdentifier:
    Identifier
    Identifier . DotIdentifier
    FundamentalType . Identifier

AsmTypePrefix:
    near ptr
    far ptr
    word ptr
    dword ptr
    qword ptr
    FundamentalType ptr

GccAsmStatement:
    asm FunctionAttributesopt { GccAsmInstructionList }

GccAsmInstructionList:
    GccAsmInstruction ;
    GccAsmInstruction ; GccAsmInstructionList

GccAsmInstruction:
    GccBasicAsmInstruction
    GccExtAsmInstruction
    GccGotoAsmInstruction

GccBasicAsmInstruction:
    AssignExpression

GccExtAsmInstruction:
    AssignExpression : GccAsmOperandsopt
    AssignExpression : GccAsmOperandsopt : GccAsmOperandsopt
    AssignExpression : GccAsmOperandsopt : GccAsmOperandsopt : GccAsmClobbersopt

GccGotoAsmInstruction:
    AssignExpression : : GccAsmOperandsopt : GccAsmClobbersopt : GccAsmGotoLabelsopt

GccAsmOperands:
    GccSymbolicNameopt StringLiteral ( AssignExpression )
    GccSymbolicNameopt StringLiteral ( AssignExpression ) , GccAsmOperands

GccSymbolicName:
    [ Identifier ]

GccAsmClobbers:
    StringLiteral
    StringLiteral , GccAsmClobbers

GccAsmGotoLabels:
    Identifier
    Identifier , GccAsmGotoLabels

Named Character Entities

NamedCharacterEntity:
    & Identifier ;

Application Binary Interface

MangledName:
    _D QualifiedName Type
    _D QualifiedName Z        // Internal

QualifiedName:
    SymbolFunctionName
    SymbolFunctionName QualifiedName

SymbolFunctionName:
    SymbolName
    SymbolName TypeFunctionNoReturn
    SymbolName M TypeModifiersopt TypeFunctionNoReturn

SymbolName:
    LName
    TemplateInstanceName
    IdentifierBackRef
    0                         // anonymous symbols

TemplateInstanceName:
    TemplateID LName TemplateArgs Z

TemplateID:
    __T
    __U        // for symbols declared inside template constraint

TemplateArgs:
    TemplateArg
    TemplateArg TemplateArgs

TemplateArg:
    TemplateArgX
    H TemplateArgX

TemplateArgX:
    T Type
    V Type Value
    S QualifiedName
    X Number ExternallyMangledName

Values:
    Value
    Value Values

Value:
    n
    i Number
    N Number
    e HexFloat
    c HexFloat c HexFloat
    CharWidth Number _ HexDigits
    A Number Values
    S Number Values
    f MangledName

HexFloat:
    NAN
    INF
    NINF
    N HexDigits P Exponent
    HexDigits P Exponent

Exponent:
    N Number
    Number

HexDigits:
    HexDigit
    HexDigit HexDigits

HexDigit:
    Digit
    A
    B
    C
    D
    E
    F

CharWidth:
    a
    w
    d

Name:
    Namestart
    Namestart Namechars

Namestart:
    _
    Alpha

Namechar:
    Namestart
    Digit

Namechars:
    Namechar
    Namechar Namechars

LName:
    Number Name
    Number __S Number    // function-local parent symbols

Number:
    Digit
    Digit Number

Digit:
    0
    1
    2
    3
    4
    5
    6
    7
    8
    9

TypeBackRef:
    Q NumberBackRef

IdentifierBackRef:
    Q NumberBackRef

NumberBackRef:
    lower-case-letter
    upper-case-letter NumberBackRef

Type:
    TypeModifiersopt TypeX
    TypeBackRef

TypeX:
    TypeArray
    TypeStaticArray
    TypeAssocArray
    TypePointer
    TypeFunction
    TypeIdent
    TypeClass
    TypeStruct
    TypeEnum
    TypeTypedef
    TypeDelegate
    TypeVoid
    TypeByte
    TypeUbyte
    TypeShort
    TypeUshort
    TypeInt
    TypeUint
    TypeLong
    TypeUlong
    TypeCent
    TypeUcent
    TypeFloat
    TypeDouble
    TypeReal
    TypeIfloat
    TypeIdouble
    TypeIreal
    TypeCfloat
    TypeCdouble
    TypeCreal
    TypeBool
    TypeChar
    TypeWchar
    TypeDchar
    TypeNoreturn
    TypeNull
    TypeTuple
    TypeVector

TypeModifiers:
    Const
    Wild
    Wild Const
    Shared
    Shared Const
    Shared Wild
    Shared Wild Const
    Immutable

Shared:
    O

Const:
    x

Immutable:
    y

Wild:
    Ng

TypeArray:
    A Type

TypeStaticArray:
    G Number Type

TypeAssocArray:
    H Type Type

TypePointer:
    P Type

TypeVector:
    Nh Type

TypeFunction:
    TypeFunctionNoReturn Type

TypeFunctionNoReturn:
    CallConvention FuncAttrsopt Parametersopt ParamClose

CallConvention:
    F       // D
    U       // C
    W       // Windows
    R       // C++
    Y       // Objective-C

FuncAttrs:
    FuncAttr
    FuncAttr FuncAttrs

FuncAttr:
    FuncAttrPure
    FuncAttrNothrow
    FuncAttrRef
    FuncAttrProperty
    FuncAttrNogc
    FuncAttrReturn
    FuncAttrScope
    FuncAttrTrusted
    FuncAttrSafe
    FuncAttrLive

FuncAttrPure:
    Na

FuncAttrNogc:
    Ni

FuncAttrNothrow:
    Nb

FuncAttrProperty:
    Nd

FuncAttrRef:
    Nc

FuncAttrReturn:
    Nj

FuncAttrScope:
    Nl

FuncAttrTrusted:
    Ne

FuncAttrSafe:
    Nf

FuncAttrLive:
    Nm

Parameters:
    Parameter
    Parameter Parameters

Parameter:
    Parameter2
    M Parameter2     // scope
    Nk Parameter2    // return

Parameter2:
    Type
    I Type     // in
    J Type     // out
    K Type     // ref
    L Type     // lazy

ParamClose:
    X     // variadic T t...) style
    Y     // variadic T t,...) style
    Z     // not variadic

TypeIdent:
    I QualifiedName

TypeClass:
    C QualifiedName

TypeStruct:
    S QualifiedName

TypeEnum:
    E QualifiedName

TypeTypedef:
    T QualifiedName

TypeDelegate:
    D TypeModifiersopt TypeFunction

TypeVoid:
    v

TypeByte:
    g

TypeUbyte:
    h

TypeShort:
    s

TypeUshort:
    t

TypeInt:
    i

TypeUint:
    k

TypeLong:
    l

TypeUlong:
    m

TypeCent:
    zi

TypeUcent:
    zk

TypeFloat:
    f

TypeDouble:
    d

TypeReal:
    e

TypeIfloat:
    o

TypeIdouble:
    p

TypeIreal:
    j

TypeCfloat:
    q

TypeCdouble:
    r

TypeCreal:
    c

TypeBool:
    b

TypeChar:
    a

TypeWchar:
    u

TypeDchar:
    w

TypeNoreturn:
    Nn

TypeNull:
    n

TypeTuple:
    B Parameters Z

ImportC

#pragma STDC FENV_ACCESS on-off-switch

on-off-switch:
    ON
    OFF
    DEFAULT

EnumDeclaration:
    enum Identifier : EnumBaseType EnumBody

EnumBaseType:
    Type

type-specifier:
    typeof-specifier

typeof-specifier:
    typeof ( expression )
    typeof ( type-name )

CImportDeclaration:
    __import ImportList ;

