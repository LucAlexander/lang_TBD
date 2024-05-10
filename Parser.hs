module Main where

import System.IO
import System.Environment
import Control.Monad
import Data.Char (isLetter, isDigit)
import qualified Data.Text as T
import Data.Text (Text, pack, unpack, splitOn)
import Text.Parsec
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Number
import Data.Functor.Foldable
import Numeric
import Text.Pretty.Simple

data Program = Program [Module] [TypeClass] [Data] [Alias] [Term] deriving (Show)
type Scope = [Expression]
type Binding = String

data Lambda = Lambda [Pattern] Expression deriving (Show)

data Term = Term {
    typeCons :: Type,
    termName :: Binding,
    guardedExprs :: [Lambda]
} deriving (Show)

data TypeClass = TypeClass [CustomType] CustomType [(Type, Binding)]
               | Implementation CustomType CustomType Generics [Term] deriving (Show)

type Generics = [CustomType]

data Expression = Closure Term
                | Application Expression Expression
                | Anonymous Lambda
                | BoundName Binding
                | BoundOperator Binding
                | StructAccess Binding
                | Return Expression
                | Block Scope
                | If Expression Expression Expression
                | Else Expression
                | Case Expression [Match]
                | LiteralPattern Pattern
                | Nop deriving (Show)

data Match = Match Pattern Expression deriving (Show)

data Pattern = Shape [Pattern]
             | ReferenceShape Binding [Pattern]
             | TypeAnchor CustomType
             | LitAnchor Literal
             | Anchor Binding deriving (Show)

data Type = TermType Type Type
          | I8 | I16 | I32 | I64
          | U8 | U16 | U32 | U64
          | F32 | F64
          | Chr
          | Bl
          | UsrType CustomType [Type]
          | TypeErr String deriving (Show)

type CustomType = String

data Data = Product CustomType Generics [Data]
          | Sum CustomType Generics [Data]
          | Record Type Binding deriving (Show)

data Alias = Alias String Type deriving (Show)

data Definition = TermDef Term
                | AliasDef Alias
                | TypeClassDef TypeClass
                | DataDef Data deriving (Show)

data Literal = Integral Int
             | Float Double
             | CharString String
             | CharSingle Char deriving (Show)

data Module = Include Filename deriving (Show)

type Filename = String

suffix :: String
suffix = ".idk"

series :: Parsec String () String -> Parsec String () a -> Parsec String () [a]
series separator parser = sepBy (parser <* spaces) (separator <* spaces)

space_series :: Parsec String () a -> Parsec String () [a]
space_series parser = series (many $ oneOf " \n\t\r") parser

data_type :: Parsec String () Type
data_type = descend <$> (series (string "->") expansion)
  where descend :: [Type] -> Type
        descend [] = TypeErr "invalid type"
        descend [x] = x
        descend (x:xs) = TermType x $ descend xs
        expansion :: Parsec String () Type
        expansion = (char '(' *> data_type <* char ')')
                <|> type_atom
                <?> "valid term type or atom type"

type_atom :: Parsec String () Type
type_atom = Chr <$ (try $ string "char")
        <|> Bl <$ (try $ string "bool")
        <|> F64 <$ (try $ string "double")
        <|> F32 <$ (try $ string "float")
        <|> I8 <$ (try $ string "int8")
        <|> I16 <$ (try $ string "int16")
        <|> I32 <$ (try $ string "int32")
        <|> I64 <$ (try $ string "int64")
        <|> U8 <$ (try $ string "uint8")
        <|> U16 <$ (try $ string "uint16")
        <|> U32 <$ (try $ string "uint32")
        <|> U64 <$ (try $ string "uint64")
        <|> (UsrType <$> try (adt_name) <*> (spaces *> (space_series type_atom)))

iden_chars :: String
iden_chars = ['a'..'z'] ++ "_"

iden_chars_rest :: String
iden_chars_rest = iden_chars ++ ['0'..'9'] ++ ['A'..'Z']

identifier :: Parsec String () String
identifier = (:) <$> (oneOf iden_chars)
                 <*> (many $ oneOf $ iden_chars_rest)

operator_identifier :: Parsec String () String
operator_identifier =
    let allowed = "<.=>|!^*&#-+@%$~?"
        reserved = ".|?+-*/@$"
     in try ((:) <$> (oneOf allowed) <*> (many1 $ oneOf allowed))
    <|> try ((:) <$> (oneOf reserved <* notFollowedBy (oneOf reserved)) <*> pure [])

adt_name :: Parsec String () String
adt_name = (:) <$> oneOf ['A'..'Z']
               <*> (many $ oneOf $ iden_chars_rest)

comment :: Parsec String () String
comment = (try $ string "//" *> (many anyChar) <* char '\n')
      <|> (try $ string "/*" *> (many anyChar) <* string "*/")

literal :: Parsec String () Literal
literal = (CharString <$> try (char '"' *> many charac <* char '"'))
      <|> (CharSingle <$> try (char '\'' *> anyChar <* char '\''))
      <|> (Float <$> try (ap sign floating))
      <|> (Integral <$> try int)
  where charac :: Parsec String () Char
        charac = (noneOf "\\\"\0\n\r\v\t\b\f") -- TODO excapes?

term :: Parsec String () Term
term = Term <$> data_type
            <*> ((operator_identifier <|> identifier) <* spaces)
            <*> series (string "|") lambda

lambda :: Parsec String () Lambda
lambda = Lambda <$> (space_series pattern)
                <*> (char '=' *> spaces *> term_set <* spaces)
  where term_set = block_expression <|> control_flow <|> application_expression

anonymous_lambda :: Parsec String () Expression
anonymous_lambda = Anonymous <$> (Lambda <$> (char '\\' *> space_series pattern)
                                         <*> (char '=' *> spaces *> anon_set <* spaces))
  where anon_set = block_expression <|> applicable_control_flow <|> application

block_expression :: Parsec String () Expression
block_expression = (Block <$> (char '{' *> spaces *> (many expression <* spaces) <* spaces <* char '}'))

application_expression :: Parsec String () Expression
application_expression = applChain <$> (application_seq <* (char ';' <* spaces))

application :: Parsec String () Expression
application = applChain <$> application_seq

application_seq :: Parsec String () [Expression]
application_seq = many1 (subexpr <* spaces)
  where subexpr :: Parsec String () Expression
        subexpr = (applChain <$> (char '(' *> spaces *> application_seq <* spaces <* char ')'))
              <|> try return_expression
              <|> try structure_access
              <|> try anonymous_lambda
              <|> applicable_control_flow
              <|> (BoundName <$> (try identifier))
              <|> (LiteralPattern <$> pattern)
              <|> (BoundOperator <$> operator_identifier)
              <?> "bound name for application"

applChain :: [Expression] -> Expression
applChain a = (descend . reverse) $ abstract a
  where descend :: [Expression] -> Expression
        descend [x] = x
        descend (x:xs) =
          case x of 
               BoundOperator _ -> Application x (descend xs)
               _ -> Application (descend xs) x
        abstract :: [Expression] -> [Expression]
        abstract [x] = [x]
        abstract (x:xs) = 
          case x of
               BoundOperator "$" -> [applChain xs]
               _ -> x:(abstract xs)

structure_access :: Parsec String () Expression
structure_access = StructAccess <$> (char '@'*> identifier)

return_expression :: Parsec String () Expression
return_expression = Return <$> (string "return" *> spaces *> application)

expression :: Parsec String () Expression
expression = try control_flow
         <|> block_expression
         <|> try application_expression
         <|> (Closure <$> (try term))
         <?> "valid expression"

applicable_control_flow :: Parsec String () Expression
applicable_control_flow = If <$> try (string "if" *> spaces *> application <* spaces)
                  <*> (spaces *> block_expression <* spaces)
                  <*> ((Else <$> (try (spaces *> string "else" *> spaces *> elsexpr <* spaces))) <|> (pure Nop))
           <|> Case <$> (try (string "case" *> spaces *> application <* spaces))
                    <*> (spaces *> match_block <* spaces)
           <?> "valid control flow structure"
  where elsexpr :: Parsec String () Expression
        elsexpr = try applicable_control_flow
              <|> block_expression
              <|> application 
              <?> "valid expression for else: control flow, block, application"

control_flow :: Parsec String () Expression
control_flow = If <$> try (string "if" *> spaces *> application <* spaces)
                  <*> (spaces *> block_expression <* spaces)
                  <*> ((Else <$> (try (spaces *> string "else" *> spaces *> elsexpr <* spaces))) <|> (pure Nop))
           <|> Case <$> (try (string "case" *> spaces *> application <* spaces))
                    <*> (spaces *> match_block <* spaces)
           <?> "valid control flow structure"
  where elsexpr :: Parsec String () Expression
        elsexpr = try control_flow
              <|> block_expression
              <|> application_expression
              <?> "valid expression for else: control flow, block, application"

match_block :: Parsec String () [Match]
match_block = (char '{' *> spaces) *> (many1 match) <* (spaces <* char '}')

match :: Parsec String () Match
match = Match <$> (pattern <* spaces) <*> (string "->" *> spaces *> match_set <* spaces)
  where match_set :: Parsec String () Expression
        match_set = control_flow
                <|> block_expression
                <|> application_expression
                <?> "valid expression for match: control flow, block, application"

pattern :: Parsec String () Pattern
pattern = (ReferenceShape <$> (try (identifier <* (spaces *> char '@' <* spaces)))
                          <*> grouping)
      <|> (Shape <$> (unbounded <|> grouping))
      <|> LitAnchor <$> literal
      <|> Anchor <$> identifier
      <?> "valid pattern"
  where grouping :: Parsec String () [Pattern]
        grouping = (char '(' *> spaces) *> (((:) <$> pattern <*> pure []) <|> unbounded) <* (spaces <* char ')')
        unbounded :: Parsec String () [Pattern]
        unbounded = (:) <$> typeMatch
                        <*> (many (pattern <* spaces))
        typeMatch :: Parsec String () Pattern
        typeMatch = TypeAnchor <$> (adt_name <* spaces)

include :: Parsec String () Module
include = Include <$> (string "include" *> spaces *> filename <* spaces <* char ';' <* spaces)

filename :: Parsec String () String
filename = (++) <$> (many $ oneOf iden_chars_rest)
                <*> string suffix

alias_definition :: Parsec String () Alias
alias_definition = Alias <$> (string "type" *> spaces *> (adt_name <* spaces <* char '=' <* spaces))
                         <*> (data_type <* spaces <* char ';')

typeclass_definition :: Parsec String () TypeClass
typeclass_definition = TypeClass <$> (string "typeclass" *> spaces *> depends <* spaces)
                                 <*> adt_name
                                 <*> members
                   <|> Implementation <$> (string "implement" *> spaces *> adt_name <* spaces)
                                      <*> (adt_name <* spaces)
                                      <*> template_params
                                      <*> impl
                   <?> "valid typeclass declaration or implementation"
  where depends :: Parsec String () [CustomType]
        depends = ((char '(' *> spaces)
                *> (series (string "," ) adt_name)
                <* (spaces *> char ')' *> spaces *> string "=>"))
              <|> (pure [])
        members :: Parsec String () [(Type, CustomType)]
        members = (spaces *> char '{' *> spaces)
               *> space_series ((,) <$> data_type
                                    <*> ((identifier <|> operator_identifier) <* spaces <* char ';'))
               <* (spaces <* char '}')
        impl :: Parsec String () [Term]
        impl = ((char '{' *> spaces) *> (many (term <* spaces)) <* char '}')

data_definition :: Parsec String () Data
data_definition = (string "data") *> spaces *> adt
  where adt :: Parsec String () Data
        adt = try (Product <$> typename
                           <*> template_params
                           <*> (lbrack *> (series (string "|") adt) <* rbrack))
          <|> Sum <$> typename
                  <*> template_params
                  <*> ((lbrack *> many ((record <* spaces)) <* rbrack) <|> (pure []))
          <?> "valid data structure"
        record :: Parsec String () Data
        record = Record <$> (data_type <* spaces) <*> (identifier <* spaces <* char ';')
        typename :: Parsec String () CustomType
        typename = adt_name <* spaces
        lbrack = (char '{') *> spaces
        rbrack = spaces <* (char '}')

template_params :: Parsec String () Generics
template_params = many (adt_name <* spaces) <* spaces

programFile :: Parsec String () Program
programFile = categorize <$> (Program <$> (many (try include)) 
                                      <*> pure []
                                      <*> pure []
                                      <*> pure []
                                      <*> pure [])
                         <*> (many definitions)
  where categorize :: Program -> [Definition] -> Program
        categorize p [] = p
        categorize (Program mods tcs dat als trms) [x] =
          case x of TypeClassDef c -> Program mods (c:tcs) dat als trms
                    DataDef d -> Program mods tcs (d:dat) als trms
                    AliasDef a -> Program mods tcs dat (a:als) trms
                    TermDef t -> Program mods tcs dat als (t:trms)
        categorize p (x:xs) = categorize (categorize p [x]) xs
        definitions :: Parsec String () Definition
        definitions = (DataDef <$> try (spaces *> data_definition <* spaces))
                  <|> (TypeClassDef <$> try (spaces *> typeclass_definition))
                  <|> (AliasDef <$> try (spaces *> alias_definition <* spaces))
                  <|> (TermDef <$> (spaces *> term <* spaces))
                  <?> "valid definition"

remove_line_comments :: String -> String
remove_line_comments program = (unpack . (T.intercalate $ pack "\n"))
                             $ fmap head
                             $ fmap (splitOn $ pack "//")
                             $ splitOn (pack "\n")
                             $ pack program

remove_block_comments :: String -> String
remove_block_comments program = (unpack . T.concat)
                              $ fmap T.concat
                              $ fill_lines
                              $ fmap (splitOn $ pack "*/")
                              $ splitOn (pack "/*")
                              $ pack program
  where fill_lines :: [[Text]] -> [[Text]]
        fill_lines x = head x : (fmap (\xs -> pack (take ((length $ splitOn (pack "\n") $ head xs) - 1) $ repeat '\n') : (tail xs)) $ tail x)

parseProgram :: String -> IO ()
parseProgram program =
  case parse programFile "(unknown)" (remove_line_comments $ remove_block_comments program) of
       Left e -> putStrLn "Parse Error"
              >> print e
       Right (Program m d c a t) -> mapM_ pPrint m >> (putStrLn "\n")
                                 >> mapM_ pPrint d >> (putStrLn "\n")
                                 >> mapM_ pPrint c >> (putStrLn "\n")
                                 >> mapM_ pPrint a >> (putStrLn "\n")
                                 >> mapM_ pPrint t

main :: IO ()
main = getArgs >>= \args ->
       case length args of
            0 -> putStrLn "provide source file"
            _ -> readFile (head args) >>= parseProgram
