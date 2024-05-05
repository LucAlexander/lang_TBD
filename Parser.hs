module Main where

import System.IO
import System.Environment
import Control.Monad
import Data.Char (isLetter, isDigit)
import Text.Parsec
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Number
import Data.Functor.Foldable
import Numeric

data Program = Program [Module] [Data] [Alias] [Term] deriving (Show)
type Scope = [Expression]
type Binding = String

data Term = Term {
    typeCons :: Type,
    name :: Binding,
    agumentNames :: [Pattern],
    evaluation :: Expression
} deriving (Show)

data Expression = Closure Term
                | Application Expression Expression
                | BoundName Binding
                | BoundOperator Binding
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
          | UsrType CustomType
          | TypeErr String deriving (Show)

type CustomType = String

data Data = Product String [Data]
          | Sum String [Data]
          | Record Type String deriving (Show)

data Alias = Alias String Type deriving (Show)

data Definition = TermDef Term
                | AliasDef Alias
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
        <|> UsrType <$> (try adt_name)
        <|> TypeErr <$> (many $ oneOf $ iden_chars_rest)

iden_chars :: String
iden_chars = ['a'..'z'] ++ "_"

iden_chars_rest :: String
iden_chars_rest = iden_chars ++ ['0'..'9'] ++ ['A'..'Z']

identifier :: Parsec String () String
identifier = (:) <$> (oneOf iden_chars)
                 <*> (many $ oneOf $ iden_chars_rest)

operator_identifier :: Parsec String () String
operator_identifier = try $ many1 $ oneOf "!@#$%^&*-+<>?~:|"

adt_name :: Parsec String () String
adt_name = (:) <$> oneOf ['A'..'Z']
               <*> (many $ oneOf $ iden_chars_rest)

comment :: Parsec String () String
comment = (try $ string "//" *> (many anyChar) <* char '\n')
      <|> (try $ string "/*" *> (many anyChar) <* string "*/")

literal :: Parsec String () Literal
literal = (CharString <$> try (char '"' *> many charac <* char '"'))
      <|> (CharSingle <$> try (char '\'' *> anyChar <* char '\''))
      <|> (Float <$> try floating)
      <|> (Integral <$> try int)
  where charac :: Parsec String () Char
        charac = (noneOf "\\\"\0\n\r\v\t\b\f") -- TODO excapes?


term :: Parsec String () Term
term = Term <$> data_type
            <*> ((operator_identifier <|> identifier) <* spaces)
            <*> (space_series pattern)
            <*> (char '=' *> spaces *> term_set <* spaces)
  where term_set = block_expression <|> control_flow <|> application_expression

block_expression :: Parsec String () Expression
block_expression = (Block <$> (char '{' *> spaces *> (many expression) <* spaces <* char '}'))

application_expression :: Parsec String () Expression
application_expression = applChain <$> (application <* (char ';' <* spaces))

predicate :: Parsec String () Expression
predicate = applChain <$> application

application :: Parsec String () [Expression]
application = many1 (subexpr <* spaces)
  where subexpr :: Parsec String () Expression
        subexpr = (applChain <$> (char '(' *> spaces *> application <* spaces <* char ')'))
              <|> (BoundName <$> identifier)
              <|> (BoundOperator <$> operator_identifier)
              <|> (LiteralPattern <$> pattern)

applChain :: [Expression] -> Expression
applChain a = (descend . reverse) a
  where descend :: [Expression] -> Expression
        descend [x] = x
        descend (x:xs) = Application (descend xs) x

expression :: Parsec String () Expression
expression = control_flow
         <|> block_expression
         <|> (Closure <$> try term)
         <|> application_expression

control_flow :: Parsec String () Expression
control_flow = If <$> try (string "if" *> spaces *> predicate <* spaces)
                  <*> (spaces *> block_expression <* spaces)
                  <*> ((Else <$> (try (spaces *> string "else" *> spaces *> elsexpr <* spaces))) <|> (pure Nop))
           <|> Case <$> (try (string "case" *> spaces *> predicate <* spaces))
                    <*> (spaces *> match_block <* spaces)
  where elsexpr :: Parsec String () Expression
        elsexpr = control_flow <|> block_expression <|> application_expression

match_block :: Parsec String () [Match]
match_block = (char '{' *> spaces) *> (many1 match) <* (spaces <* char '}')

match :: Parsec String () Match
match = Match <$> (pattern <* spaces) <*> (string "->" *> spaces *> match_set <* spaces)
  where match_set :: Parsec String () Expression
        match_set = control_flow
                <|> block_expression
                <|> application_expression

pattern :: Parsec String () Pattern
pattern = (ReferenceShape <$> (try (identifier <* (spaces *> char '@' <* spaces)))
                          <*> grouping)
      <|> (Shape <$> (unbounded <|> grouping))
      <|> LitAnchor <$> literal
      <|> Anchor <$> identifier
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

data_definition :: Parsec String () Data
data_definition = (string "data") *> spaces *> adt
  where adt :: Parsec String () Data
        adt = try (Product <$> typename <*> (lbrack *> (series (string "|") adt) <* rbrack))
          <|> Sum <$> typename <*> ((lbrack *> many ((record <* spaces)) <* rbrack) <|> (pure []))
        record :: Parsec String () Data
        record = Record <$> (data_type <* spaces) <*> (identifier <* spaces <* char ';')
        typename = adt_name <* spaces
        lbrack = (char '{') *> spaces
        rbrack = spaces <* (char '}')

programFile :: Parsec String () Program
programFile = categorize <$> (Program <$> (many (try include)) <*> pure [] <*> pure [] <*> pure [])
                         <*> (many definitions)
  where categorize :: Program -> [Definition] -> Program
        categorize p [] = p
        categorize (Program mods dat als trms) [x] =
          case x of DataDef d -> Program mods (d:dat) als trms
                    AliasDef a -> Program mods dat (a:als) trms
                    TermDef t -> Program mods dat als (t:trms)
        categorize p (x:xs) = categorize (categorize p [x]) xs
        definitions :: Parsec String () Definition
        definitions = (DataDef <$> try (spaces *> data_definition <* spaces))
                  <|> (AliasDef <$> try (spaces *> alias_definition <* spaces))
                  <|> (TermDef <$> (spaces *> term <* spaces))

parseProgram :: String -> IO ()
parseProgram program =
  case parse programFile "(unknown)" program of
       Left e -> putStrLn "Parse Error"
              >> print e
       Right (Program m d a t) -> mapM_ print m >> (putStrLn "\n")
                               >> mapM_ print d >> (putStrLn "\n")
                               >> mapM_ print a >> (putStrLn "\n")
                               >> mapM_ print t

main :: IO ()
main = getArgs >>= \args ->
       case length args of
            0 -> putStrLn "provide source file"
            _ -> readFile (head args) >>= parseProgram
