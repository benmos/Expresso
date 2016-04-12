{-
A Java-compatible intermediate language - minimise use of statements

-}

module JavaSyn where

import           Data.Maybe
import           Text.PrettyPrint.Leijen


type JPackageName = String
type JTypeName    = String
type JName        = String

data JCompilationUnit
  = JCompilationUnit JPackageName [JDecl] -- imports, comments, sourcefile?
    deriving (Show)

data JDecl
  = JField  [JModifier] JType JName (Maybe JExpr)
  | JConstr [JModifier] JTypeName [JParam] [JStatement]
  | JMethod [JModifier] JType JName [JParam] [JStatement]
  | JClass  [JModifier] JTypeName [JTypeName] [JDecl] -- modifiers, name, extending, defs
   deriving (Show)

data JStatement
  = JReturn JExpr
  | JBlock [JStatement]
  | JEffect JExpr
  | JVarDecl JName JType JExpr
  | JAssignableDecl JName JType JExpr
  | JIfThenElse JExpr JStatement JStatement
--  | Switch
  | JThrow JExpr
  | Comment String
  | LineComment String JStatement
  | BlankLine
    deriving (Show)

data JParam
 = JParam [JModifier] JType JName
   deriving (Show)

data JExpr
  = JIdent JName
  | JLiteral JLiteral -- JType Object -- TODO
  | JMethodInvoke JExpr JName [JExpr] -- object, method, args
--  | JMethodInvoke [JExpr] JExpr Name [JExpr] -- typeargs, object, method, args
  | JCond JExpr JExpr JExpr -- test, then, else
  | JFieldAccess JExpr JName
  | JArrayAccess JExpr JExpr -- indexed, index
  | JAssign JExpr JExpr -- lhs, rhs
  | JInstanceOf JExpr JType
  | JUOp String JExpr
  | JBinOp String JExpr JExpr
--  | JError TypeName [Expr] -- shouldn't need this
  | JNew JType [JExpr] -- type, args
  | JNewClass JType [JExpr] [JDecl] -- anonymous innerclass
  | JNewArray JType [JExpr] [JExpr] -- elem type, dims, elems
--  |  JDK8: JLambda ( params : [JVariableDecl], body: JExpr, targetType: Type)
  | JLambda [JParam] [JStatement]
  | JNull
--  JArrayTypeTree -- An array type, A[] (elemtype: JExpr)
--  JPrimitiveTypeTree -- identifies a basic type (typetag : Int)
--  JTypeApply --  parameterized type, T<...>
  | JTypeCast JType JExpr
--  JWildcard
  | ExprComment String JExpr
    deriving (Show)

data JModifier
  = JPublic
  | JPrivate
  | JStatic
  | JAbstract
  | JFinal
  deriving (Show)

data JType
  = JPrimType  JPrimType
  | JArrayType JType
  | JType      JTypeName
    deriving (Show)

data JPrimType
  = JInt
  | JBoolean
  | JChar
  | JLong
  | JFloat
  | JDouble
  | JByte
  | JVoid
    deriving (Show)

-- | Literals -- TODO
data JLiteral
        = JLBool         Bool         -- ^ Boolean.
        | JLInt          Int          -- ^ An integer.
--        | JLLong         Integer      -- ^ A long integer.
        | JLFloat        Float        -- ^ A single precision floating point number.
        | JLDouble       Double       -- ^ A double precision floating point number.
        | JLChar         Char         -- ^ A character.
        | JLString       String       -- ^ A string.
    deriving (Show)

------------------------------------------------------------
-- Pretty Printing

instance Pretty JCompilationUnit where
  ppr (JCompilationUnit package decls) =
      text "package" <+> text package <> text ";" </> stack (map ppr decls)

instance Pretty JDecl where
  ppr (JField mods ty name e1) = spread (map ppr mods) <+> ppr ty <+> text name
         <> optional (isJust e1) (text "=" <+> (ppr . fromJust) e1)
         <> text ";"
  ppr (JConstr mods tyname params stmts) =
        spread (map ppr mods) <+> text tyname <> pprArgs params <+> block stmts
  ppr (JMethod mods ty name params stmts) =
        spread (map ppr mods) <+> ppr ty <+> text name <> pprArgs params </> block stmts
  ppr (JClass mods tyname extends decls) =
        spread (map ppr mods) <+> text "class" <+> text tyname
        <> optional (not . null $ extends) (text "extends" <+> spread (map text extends))
        <+> block decls

instance Pretty JExpr where
    ppr (JIdent name)  = text name
    ppr (JLiteral lit) = ppr lit
    ppr (JMethodInvoke obj name args) = ppr obj
                                        <> text "." <> text name <> pprArgs args
    ppr (JCond e1 e2 e3) = ppr e1
                           <+> nest 4 (text "?" <+> ppr e2
                                       <+/> (text ":" <+> ppr e3))
    ppr (JFieldAccess e1 name) = ppr e1 <> text "." <> text name
    ppr (JArrayAccess e1 e2) = ppr e1 <> (bracket "[" (ppr e2) "]")
    ppr (JAssign e1 e2) = ppr e1 <+> text "=" <+> ppr e2
    ppr (JInstanceOf e1 ty) = ppr e1 <+> text "instanceof" <+> ppr ty
    ppr (JUOp op e1) = text op <+> ppr e1
    ppr (JBinOp op e1 e2) = ppr e1 <+> text op <+> ppr e2
    ppr (JNew ty args) = text "new" <+> ppr ty <> pprArgs args

    ppr (JNewClass ty args decls) = text "new"
                            <+> ppr ty
                            <>  pprArgs args
                            <+> block decls
    ppr (JNewArray ty dims elems) = text "new"
                      <+> ppr ty
                      <> (bracket "[" (sep "," (map ppr dims)) "]")
                      <> optional (not . null $ elems) (text "="
                                  <+> (bracket "{" (sep "," (map ppr elems)) "}"))

    ppr (JTypeCast ty e) = parens (parens (ppr ty) <> ppr e)
    ppr (JLambda params stmts) = (pprArgs params) <+> text "->" <+> block stmts
    ppr JNull = text "null"

instance Pretty JStatement where
    ppr (JReturn e) = text "return" <+> ppr e <> text ";"
    ppr (JEffect e) = ppr e <> text ";"
    ppr (JVarDecl nm ty e) = text "final" <+> ppr ty <+> text nm <+> text "=" <+> ppr e
    ppr (JAssignableDecl nm ty e) = ppr ty <+> text nm <+> text "=" <+> ppr e
    ppr (JIfThenElse ce te fe) = text "if" <+> parens (ppr ce)
                                 <+> ppr te </> text "else" <+> ppr fe
    ppr (JThrow e) = text "throw" <+> ppr e
    ppr (JBlock e) = block e
    -- TODO

instance Pretty JType  where
    ppr (JPrimType primty) = ppr primty
    ppr (JArrayType ty) = ppr ty <> text "[]"
    ppr (JType tyname) = text tyname

instance Pretty JParam where
    ppr (JParam mods ty name) = spread (map ppr mods) <+> ppr ty <+> text name

instance Pretty JPrimType where
    ppr JInt = text "int"
    ppr JBoolean = text "boolean"
    ppr JChar = text "char"
    ppr JLong = text "long"
    ppr JFloat = text "float"
    ppr JDouble = text "double"
    ppr JByte = text "byte"
    ppr JVoid = text "void"

instance Pretty JModifier where
    ppr JPublic = text "public"
    ppr JPrivate = text "private"
    ppr JStatic = text "static"
    ppr JAbstract = text "abstract"
    ppr JFinal = text "final"

instance Pretty JLiteral where
    ppr (JLBool x)   = text $ show x -- TODO pretty instance for prims?
    ppr (JLInt x)    = text $ show x
    ppr (JLFloat x)  = text $ show x
    ppr (JLDouble x) = text $ show x
    ppr (JLChar x)   = text $ show x
    ppr (JLString x) = text $ show x

-- TODO should work like parens
block :: Pretty a => [a] -> DOC
block [] = text "{" <+> text "}"
block xs = text "{" <> (nest 2 (line <> stack (map ppr xs))) </> text "}"

pprArgs args = parens (sep "," (map ppr args))

-- TODO include in the pp library?
sep :: String -> [DOC] -> DOC
sep s [] = nil
sep s [d] = d
sep s (d:d':ds) = d <> text s <+/> d' <> sep s ds

-- TODO include in the pp library? TODO rewrite/rename
optional :: Bool -> DOC -> DOC
optional b d = if b then nil <+> d else nil

-----------------------
-- Testing
test = JCompilationUnit "Main" [clazz]
clazz = JClass [JPublic, JFinal] "Cons" ["List"] [field1, field2, constr]
field1 = JField [JPublic, JFinal] (JType "Object") "x1" Nothing
field2 = JField [JPublic, JFinal] (JType "List") "x2" Nothing
constr = JConstr [JPublic] "Cons" [
    JParam [JFinal] (JType "Object") "x1_",
    JParam [JFinal] (JType "List") "x2_" ] [
           JEffect (JAssign (JFieldAccess (JIdent "this") "x1") (JIdent "x1_")),
           JEffect (JAssign (JFieldAccess (JIdent "this") "x2") (JIdent "x2_"))]
str = pretty 60 (ppr test)
