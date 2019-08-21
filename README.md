# Go Parser

This is the delivery for the third homework assignment of Compilers Construction of Innopolis University, Fall 2018

## Authors

- Nikita Vilunov
- Ivan Lyagaev
- 5 Red Bull cans, 9 cups of coffee and a lot of "free" time 

## BNF

Can be found [here](https://golang.org/ref/spec)

## Usage

- Place the source of your program in folder `input`
- Run the tokenizer using by instruction below
- Find the list of tokens in folder `output`

## Requirements

- cargo >= 1.37
- rustc >= 1.37

## Running with Cargo

**Building and running:**
```sh
cargo run
```

**Running tests:**
```sh
cargo test
```

**Building documentation:**
```sh
cargo doc
```

The documentation index will be located at `target/doc/goparse/index.html`

# Grammar

## Terminals

```
string_literal
literal - including string_literal
identifier
bin_op
assign_op
```

## Rules

```
Program => PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" }
PackageClause => "package" identifier
ImportDecl => "import" ImportSpec
ImportDecl => "import" "(" { ImportSpec ";" } ")"
ImportSpec => string_literal
ImportSpec => "." string_literal
ImportSpec => identifier string_literal
TopLevelDecl => Declaration
TopLevelDecl => FunctionDeclaration

PrimaryExpression => literal
PrimaryExpression => FullIdentifier
PrimaryExpression => CompositeLiteral
PrimaryExpression => "(" Expression ")"
PrimaryExpression => Type "(" Expression [","] ")"
PrimaryExpression => Type "." identifier
PrimaryExpressionMod => "." identifier
PrimaryExpressionMod => "[" Expression "]"
PrimaryExpressionMod => "[" [Expression] ":"" [Expression] "]"
PrimaryExpressionMod => "[" [Expression] ":"" Expression : Expression "]"
PrimaryExpressionMod => "." Type
PrimaryExpressionMod => "(" ")"
PrimaryExpressionMod => "(" ExpressionList ["..."] [","] ")"
PrimaryExpressionMod => "(" Type ["," ExpressionList] ["..."] [","] ")"
UnaryExpression = { unary_op } PrimaryExpression { PrimaryExpressionMod }
Expression => UnaryExpression { bin_op UnaryExpression }
CompositeLiteral => [ "[" "..." "]" ] Type LiteralValue
LiteralValue => "{" [ LiteralElement { "," LiteralElement } [","] ] "}"
LiteralElement => [ (Expression|LiteralValue) ":" ] (Expression|LiteralValue)

FullIdentifier => identifier
FullIdentifier => identifier "." identifier
Type => "(" Type ")"
Type => FullIdentifier
Type => "[" Expression "]" Type
Type => "[]" Type
Type => "*" Type
Type => "chan" Type
Type => "chan" "<-" Type
Type => "<-" "chan" Type
Type => "map" "[" Type "]" Type
Type => "struct" "{" { FieldDecl ";" } "}"
FieldDecl => IdentifierList Type [string_literal]
FieldDecl => ["*"] FullIdentifier [string_literal]
Type => "func" Signature
Type => "interface" "{" { MethodSpec ";" } "}"
MethodSpec => identifier Signature
MethodSpec => FullIdentifier


IdentifierList => identifier {"," identifier}
ExpressionList => Expression {"," Expression}
ParametersSpec => [IdentifierList] ["..."] Type
Parameters => "(" [ParametersSpec {"," ParameterSpec}[","]] ")"
SignatureResult => Type
SignatureResult => Parameters
Signature =>  Parameters [SignatureResult]
FunctionDeclaration => "func" [Parameters] identifier Signature [Block]
ConstantSpec => IdentifierList [[Type] "=" ExpressionList]
ConstantDeclaration => "const" (ConstantSpec| "(" { ConstantSpec ";"} ")")
TypeSpec => identifier ["="] Type
TypeDeclaration => "type" (TypeSpec | "(" { TypeSpec ";"} ")")
VariableSpec => IdentifierList (Type ["=" ExpressionList]| "=" ExpressionList)
VariableDeclaration => "var" (VariableSpec | "(" { VariableSpec ";" } ")")
Declaration => ConstantDeclaration | VariableDeclaration | TypeDeclaration
TopLevelDeclaration => FunctionDeclaration | Declaration

AssignStatement => ExpressionList assign_op ExpressionList
SendStatement => Expression "<-" Expression
IncDecStatement => Expression ("++"|"--")
ShortVarDeclaration => IdentifierList ":=" ExpressionList
SimpleStatement => Expression | SendStatement | IncDecStatement | AssignStatement | ShortVarDeclaration

LabelStatement => identifier ":" Statement
GoStatement => "go" Expression
ReturnStatement => "return" [ExpressionList]
BreakStatement => "break" [identifier]
ContinueStatement => "continue" [identifier]
GotoStatement => "goto" identifier
Block => "{" {Statement ";" } "}"
IfStatement => "if" [SimpleStatement ";"] Expression Block ["else" (IfStatement | Block)]
ExpressionSwitchStatement => "switch" [SimpleStatement ";"][Expression]"{" { ExpressionCaseClause }"}"
ExpressionCaseClause => ("case" ExpressionList | "default") ":" StatementList
StatementList => { Statement ";"}
TypeSwitchStatement => "switch" [SimpleStatement ";"] TypeSwitchGuard
TypeSwitchGuard => [identifier ":="] PrimaryExpression ".(type)"
TypeList => Type {"," Type }
TypeSwitchCase => ("case" TypeList | "default") ":" StatementList
ReceiveStatement => [ExpressionList "=" | IdentifierList ":="] RecvExpr
SelectStatement => "select" "{"{ ("case" (SendStatemet | ReceiveStatement)|"default") ":" StatementList }"}"
RangeClause => [ ExpressionList "=" | IdentifierList ":="] "range" Expression
ForClause => [SimpleStament]";"[Expression]";"[SimpleStament]
ForStatement => "for" [Expression | ForClause | RangeClause] Block
DeferExpression => "defer" Expression
Statement => Declaration | LabelStatement | SimpleStatement 
Statement => ReturnStatement | BreakStatement | ContinueStatement 
Statement => GotoStatement | "fallthrough" | Block
Statement => IfStatement | (ExpressionSwitchClause | TypeSwithCase)
Statement => SelectStatement | ForStatement | DeferExpression
```
