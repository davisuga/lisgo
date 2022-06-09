type typ = Golang.Types.t
type identifier = string
type comment = CompilerDirective of string | Comment of string
type stringLiteral = RawStringLiteral of string | InterpretedStringLiteral of string

type basicLiteral =
  | IntLiteral of int
  | FloatLiteral of float
  | RuneLiteral of char
  | StringLiteral of stringLiteral

and literalKey =
  | LiteralKeyName of identifier
  | LiteralKeyExpression of expression
  | LiteralKeyValue of literalValueElements

and literalValueElements = LiteralValueElements of literalElement list

and literalElement =
  | UnkeyedElement of expression
  | KeyedElement of literalKey * expression
  | LiteralComment of comment

and functionParameter =
  | FunctionParameter of identifier * typ
  | VariadicFunctionParameter of identifier * typ

and functionSignature = FunctionSignature of functionParameter list * typ list

and literal =
  | BasicLiteral of basicLiteral
  | CompositeLiteral of typ * literalValueElements
  | FunctionLiteral of functionSignature * block

and operand =
  | Literal of literal
  | OperandName of identifier
  | QualifiedOperandName of identifier * identifier
  | GroupedExpression of expression

and sliceExpression =
  | ClosedSlice of expression * expression
  | LowerBoundSlice of expression
  | UpperBoundSlice of expression

and argument = Argument of expression | VariadicSliceArgument of expression

and primaryExpression =
  | Operand of operand
  | Conversion of typ * expression
  | Selector of primaryExpression * identifier
  | Index of primaryExpression * expression
  | Slice of primaryExpression * sliceExpression
  | Application of primaryExpression * argument list

and unaryOperator = UnaryPlus | UnaryNegation | Not | BitwiseComplement | AddressOf

and binaryOperator =
  | Or
  | And
  | EQ
  | NEQ
  | LT
  | LTE
  | GT
  | GTE
  | Sum
  | Difference
  | Product
  | Quotient
  | Remainder
  | BitwiseOR
  | BitwiseXOR
  | BitwiseAND
  | BitwiseClear
  | LeftShift
  | RightShift

and expression =
  | Expression of primaryExpression
  | UnaryOp of unaryOperator * primaryExpression
  | BinaryOp of binaryOperator * expression * expression

and assignOp = Assign | AssignSum | AssignProduct

and simpleStmt =
  | ExpressionStmt of expression
  | Assignment of expression list * assignOp * expression list
  | SendStmt of expression * expression

and ifStmt = If of expression * block | IfElse of expression * block * elseBranch
and elseBranch = ElseBlock of block | ElseIf of ifStmt

and varDeclaration =
  | VarDeclZeroValue of identifier * typ
  | VarDeclInitializer of identifier * typ * expression

and declaration =
  | TypeDecl of identifier * typ
  | ConstDecl of identifier * typ * expression
  | VarDecl of varDeclaration

and stmt =
  | DeclarationStmt of comment option * declaration
  | IfStmt of ifStmt
  | ReturnStmt of expression list
  | BlockStmt of block
  | GoStmt of expression
  | SimpleStmt of simpleStmt
  | StmtComment of comment

and block = Block of stmt list

and topLevelDeclaration =
  | Decl of declaration
  | FunctionDecl of identifier * functionSignature * block
  | TopLevelComment of comment

and packageClause = PackageClause of identifier
and importDecl = ImportDecl of identifier * stringLiteral

and sourceFile =
  | SourceFile of packageClause * importDecl list * topLevelDeclaration list
