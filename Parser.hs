module Main where

import System.IO
import System.Environment
import Control.Monad
import Data.Char (isLetter, isDigit)
import Text.Parsec
import Text.Parsec.Char
import Data.Functor.Foldable
import Numeric

data Program = Program [Module] [Alias] [Term] deriving (Show)
type Scope = [Expression]
type Binding = String

data Term = Term {
    typeCons :: Type,
    name :: Binding,
    agumentNames :: [Binding],
    evaluation :: Expression
} deriving (Show)

data Alias = Alias Type Type deriving (Show)

data Expression = Closure Term
                | Application Expression Expression
                | BoundName Binding
                | Block Scope
                | Nop deriving (Show)

data Type = TermType Type Type
          | I8 | I16 | I32 | I64
          | U8 | U16 | U32 | U64
          | F32 | F64
          | Chr
          | Bl
          | UsrType String
          | TypeErr String deriving (Show)

data Definition = TermDef Term
                | AliasDef Alias deriving (Show)

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
        <|> UsrType <$> (try identifier)
        <|> TypeErr <$> (many $ oneOf $ iden_chars_rest)

iden_chars :: String
iden_chars = ['a'..'z'] ++ ['A'..'Z'] ++ "_"

iden_chars_rest :: String
iden_chars_rest = iden_chars ++ ['0'..'9']

identifier :: Parsec String () String
identifier = (:) <$> (oneOf iden_chars)
                 <*> (many $ oneOf $ iden_chars_rest)

term :: Parsec String () Term
term = Term <$> data_type
            <*> (identifier <* spaces)
            <*> (space_series identifier)
            <*> (char '=' *> spaces *> expression)

expression :: Parsec String () Expression
expression = (Block <$> (char '{' *> spaces *> (many (expression <* spaces)) <* spaces <* char '}'))
         <|> (Closure <$> try term)
         <|> (applChain <$> (searchTerm <* spaces <* char ';'))
  where subexpression :: Parsec String () Expression
        subexpression = (applChain <$> (char '(' *> spaces *> searchTerm <* spaces <* char ')'))
                    <|> (BoundName <$> identifier)
        searchTerm :: Parsec String () [Expression]
        searchTerm = many1 (subexpression <* spaces)
        applChain :: [Expression] -> Expression
        applChain a = (descend . reverse) a
          where descend :: [Expression] -> Expression
                descend [x] = x
                descend (x:xs) = Application (descend xs) x

include :: Parsec String () Module
include = Include <$> (string "include" *> spaces *> filename <* spaces <* char ';' <* spaces)

filename :: Parsec String () String
filename = (++) <$> (many $ oneOf iden_chars_rest)
                <*> string suffix

type_alias :: Parsec String () Alias
type_alias = Alias <$> (string "type" *> spaces *> data_type <* spaces)
                  <*> (char '=' *> spaces *> data_type <* spaces <* char ';')

programFile :: Parsec String () Program
programFile = categorize <$> (Program <$> (many include) <*> pure [] <*> pure [])
                         <*> (many definitions)
  where categorize :: Program -> [Definition] -> Program
        categorize p [] = p
        categorize (Program mods als trms) [x] =
          case x of AliasDef a -> Program mods (a:als) trms
                    TermDef t -> Program mods als (t:trms)
        categorize p (x:xs) = categorize (categorize p [x]) xs
        definitions :: Parsec String () Definition
        definitions = (AliasDef <$> (spaces *> type_alias <* spaces))
                  <|> (TermDef <$> (spaces *> term <* spaces))

parseProgram :: String -> IO ()
parseProgram program =
  case parse programFile "(unknown)" program of
       Left e -> putStrLn "Parse Error"
              >> print e
       Right (Program m a t) -> mapM_ print m
                             >> mapM_ print a
                             >> mapM_ print t

main :: IO ()
main = getArgs >>= \args ->
       case length args of
            0 -> putStrLn "provide source file"
            _ -> readFile (head args) >>= parseProgram
