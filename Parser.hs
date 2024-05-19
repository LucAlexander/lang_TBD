module Main where

import System.IO
import System.Environment
import Control.Monad
import Data.Char (isLetter, isDigit, toUpper)
import qualified Data.Text as T
import Data.Text (Text, pack, unpack, splitOn)
import Text.Parsec
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Number
import Data.Functor.Foldable
import Numeric
import Text.Pretty.Simple

data Program = Program [Module] Namespace deriving (Show)
type Scope = [Expression]
type Binding = String

data Namespace = Namespace String [Namespace] [TypeClass] [Data] [Alias] [Term] deriving (Show)

data Lambda = Lambda [Pattern] Expression deriving (Show)

data Term = Term {
    typeCons :: Type,
    termName :: Binding,
    guardedExprs :: [Lambda]
} |  DependentTerm [(CustomType, CustomType)] Term deriving (Show)

data TypeClass = TypeClass [CustomType] CustomType [(Type, Binding)]
               | Implementation CustomType CustomType Generics [Term] deriving (Show)

type Generics = [CustomType]

data Expression = Closure Term
                | Application Expression Expression
                | Anonymous Lambda
                | BoundName Binding
                | BoundOperator Binding
                | StructAccess Binding
                | NamespaceAccess [CustomType] Binding
                | Return Expression
                | Block Scope
                | If Expression Expression Expression
                | Else Expression
                | Case Expression [Match]
                | LiteralPattern Pattern
                | Nop deriving (Show)

data Match = Match Pattern Expression deriving (Show)

data Pattern = Shape [Pattern]
             | ReferenceShape Binding Pattern
             | ExternTypeAnchor [CustomType] CustomType
             | TypeAnchor CustomType
             | LitAnchor Literal
             | ArrayPattern [Pattern]
             | Anchor Binding deriving (Show)

data Type = TermType Type Type
          | I8 | I16 | I32 | I64
          | U8 | U16 | U32 | U64
          | F32 | F64
          | Chr
          | Bl
          | Array Type
          | UsrType CustomType [Type]
          | ExternType [CustomType] Type
          | TypeErr String deriving (Show)

type CustomType = String

data Data = Product CustomType Generics [Data]
          | Sum CustomType Generics [Data]
          | Record Type Binding deriving (Show)

data Alias = Alias String Type deriving (Show)

data Definition = TermDef Term
                | AliasDef Alias
                | TypeClassDef TypeClass
                | NamespaceDef Namespace
                | DataDef Data deriving (Show)

data Literal = Integral Int
             | Float Double
             | CharString String
             | ArrayLiteral [Expression]
             | CharSingle Char deriving (Show)

data Module = Include Filename deriving (Show)

type Filename = String

type Interpreter = Program -> IO ()
type Generator a = a -> String

suffix :: String
suffix = ".ctn"

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
type_atom = primitive
        <|> array
        <|> extern
        <|> custom
        <?> "valid type atom"
  where extern :: Parsec String () Type
        extern = try (ExternType <$> many1 (try (adt_name <* string "::")) <*> custom)
        primitive :: Parsec String () Type
        primitive = Chr <$ (try $ string "char")
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
                <?> "valid primitive"
        array :: Parsec String () Type
        array = try (Array <$> ((char '[' *> spaces) *> data_type <* (spaces <* char ']')))
        custom :: Parsec String () Type
        custom = (UsrType <$> try (adt_name) <*> (spaces *> space_series generic))
        generic :: Parsec String () Type
        generic = primitive
              <|> array
              <|> (UsrType <$> try (adt_name) <*> pure [])
              <|> ((char '(' *> spaces) *> type_atom <* (spaces <* char ')'))
              <?> "generic"

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
      <|> (ArrayLiteral <$> (char '[' *> spaces *> (series (string ",") array_comps) <* char ']'))
      <?> "valid literal"
  where charac :: Parsec String () Char
        charac = (noneOf "\\\"\0\n\r\v\t\b\f") -- TODO excapes?
        array_comps :: Parsec String () Expression
        array_comps = try applicable_control_flow
                  <|> block_expression
                  <|> application 

term :: Parsec String () Term
term = try (DependentTerm <$> (try (depend_group <* (spaces <* string "=>") <* spaces)) <*> independent)
   <|> independent
  where depend_group :: Parsec String () [(CustomType, CustomType)]
        depend_group = (char '(' *> spaces *> depends <* (spaces <* char ')')) <|> depends
        depends :: Parsec String () [(CustomType, CustomType)]
        depends = series (string ",") ((,) <$> (adt_name <* spaces) <*> (adt_name))
        independent :: Parsec String () Term
        independent = Term <$> data_type
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
              <|> try namespace_access
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

namespace_access :: Parsec String () Expression
namespace_access = NamespaceAccess <$> many1 (adt_name <* string "::")
                                   <*> (try identifier <|> try adt_name <|> operator_identifier)

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
                          <*> ((Shape <$> grouping) <|> array_pattern))
      <|> (Shape <$> (unbounded <|> grouping))
      <|> array_pattern
      <|> LitAnchor <$> literal
      <|> Anchor <$> identifier
      <?> "valid pattern"
  where grouping :: Parsec String () [Pattern]
        grouping = (char '(' *> spaces) *> (((:) <$> pattern <*> pure []) <|> unbounded) <* (spaces <* char ')')
        unbounded :: Parsec String () [Pattern]
        unbounded = (:) <$> typeMatch
                        <*> (many (pattern <* spaces))
        typeMatch :: Parsec String () Pattern
        typeMatch = (ExternTypeAnchor <$> many1 (try (adt_name <* string "::")) <*> (adt_name <* spaces))
                <|> TypeAnchor <$> (adt_name <* spaces)
        array_pattern :: Parsec String () Pattern
        array_pattern = ArrayPattern <$> (try $ (char '[' *> spaces) *> (series (string ":") pattern) <* (spaces <* char ']'))

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

empty_namespace :: Parsec String () String -> Parsec String () Namespace
empty_namespace name = Namespace <$> name
                                 <*> pure []
                                 <*> pure []
                                 <*> pure []
                                 <*> pure []
                                 <*> pure []

custom_namespace_definitions :: Parsec String () Namespace
custom_namespace_definitions =
    string "namespace" *> spaces *>
    (categorize_namespace <$> (empty_namespace $ try adt_name)
                          <*> definition_block)
  where definition_block :: Parsec String () [Definition]
        definition_block = (spaces *> char '{' *> spaces *> (many global_definitions) <* spaces <* char '}')

parse_namespace :: Parsec String () Namespace
parse_namespace = categorize_namespace <$> (empty_namespace $ pure "global")
                                       <*> (many global_definitions)

categorize_namespace :: Namespace -> [Definition] -> Namespace
categorize_namespace p [] = p
categorize_namespace (Namespace name nms tcs dat als trms) [x] =
  case x of NamespaceDef n -> Namespace name (n:nms) tcs dat als trms
            TypeClassDef c -> Namespace name nms (c:tcs) dat als trms
            DataDef d -> Namespace name nms tcs (d:dat) als trms
            AliasDef a -> Namespace name nms tcs dat (a:als) trms
            TermDef t -> Namespace name nms tcs dat als (t:trms)
categorize_namespace p (x:xs) = categorize_namespace (categorize_namespace p [x]) xs

global_definitions :: Parsec String () Definition
global_definitions = (DataDef <$> try (spaces *> data_definition <* spaces))
                 <|> (NamespaceDef <$> try (spaces *> custom_namespace_definitions))
                 <|> (TypeClassDef <$> try (spaces *> typeclass_definition))
                 <|> (AliasDef <$> try (spaces *> alias_definition <* spaces))
                 <|> (TermDef <$> (spaces *> term <* spaces))
                 <?> "valid definition"

programFile :: Parsec String () Program
programFile = (Program <$> (many $ try include)
                       <*> parse_namespace)

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

-- semantic analysis

check_dup_decls :: Namespace -> Either String Namespace
check_dup_decls glob@(Namespace namsp subspaces typeclasses data_structs aliases terms) =
    case Right [] >>= (check_dupl check_term terms)
                  >>= (check_dupl check_data data_structs)
                  >>= (check_dupl check_class typeclasses)
                  >>= (check_dupl check_alias aliases) of
         Right _ -> Right glob
         Left e -> Left e
  where contains :: (Show a) => String -> [String] -> a -> ([String] -> Either String [String]) -> Either String [String]
        contains item tabl full dip = 
            if item `elem` tabl
            then Left $ concat ["duplicate ", item, " definition: ", show full]
            else dip $ item:tabl

        check_dupl :: (a -> [String] -> Either String [String]) -> [a] -> [String] -> Either String [String]
        check_dupl seq items start = foldl (>>=) (Right start) $ map seq items

        check_term :: Term -> [String] -> Either String [String]
        check_term full@(Term tcons tname texprs) tabl = contains tname tabl full $ dip_term $ map (\(Lambda _ ex) -> ex) texprs
          where dip_term :: [Expression] -> [String] -> Either String [String]
                dip_term [] newtable = Right newtable
                dip_term (t:exprs) newtable =
                    (case check_dupl check_term (aggr_terms t) newtable of
                         Left err -> Left err
                         Right _ -> Right newtable) >>= dip_term exprs

                aggr_terms :: Expression -> [Term]
                aggr_terms (Closure t) = [t]
                aggr_terms (Application a b) = (aggr_terms a) ++ (aggr_terms b)
                aggr_terms (Anonymous (Lambda _ e)) = aggr_terms e
                aggr_terms (Return expr) = aggr_terms expr
                aggr_terms (Block scope) = concat $ map aggr_terms $ scope 
                aggr_terms (If pred cond alt) = (aggr_terms pred) ++ (aggr_terms cond) ++ (aggr_terms alt)
                aggr_terms (Else alt) = aggr_terms alt
                aggr_terms (Case expr cases) = (aggr_terms expr) ++ (concat $ map (\(Match _ e) -> aggr_terms e) cases)
                aggr_terms _ = []

        check_data :: Data -> [String] -> Either String [String]
        check_data full@(Product pname _ membs) tabl = contains pname tabl full $ check_dupl check_data membs
        check_data full@(Sum sname _ membs) tabl = contains sname tabl full $ check_dupl check_data membs
        check_data _ tabl = Right tabl

        check_class :: TypeClass -> [String] -> Either String [String]
        check_class full@(TypeClass _ tcname _) tabl = contains tcname tabl full $ Right
        check_class _ tabl = Right tabl

        check_alias :: Alias -> [String] -> Either String [String]
        check_alias full@(Alias aname _) tabl = contains aname tabl full $ Right

replace_namespace_type_alias :: Namespace -> Namespace
replace_namespace_type_alias (Namespace namsp subspaces typeclasses data_structs aliases terms) =
    Namespace namsp (map replace_namespace_type_alias subspaces)
                    (map replace_typeclass_type_alias typeclasses)
                    (map replace_data_type_alias data_structs)
                    aliases
                    (map replace_term_type_alias terms)
  where look :: (String -> Maybe Type)
        look = alias_lookup aliases
          where alias_lookup :: [Alias] -> String -> Maybe Type
                alias_lookup [] _ = Nothing
                alias_lookup ((Alias s t):as) key = if s==key then Just t else alias_lookup as key

        replace_typeclass_type_alias :: TypeClass -> TypeClass
        replace_typeclass_type_alias (TypeClass deps name membs) = 
            TypeClass deps name $ map convert membs
          where convert :: (Type, Binding) -> (Type, Binding)
                convert (t, b) = (replace_usr_type_alias t, b)
        replace_typeclass_type_aliases (Implementation name t gens impls) = 
            Implementation name t gens (map replace_term_type_alias impls)

        replace_data_type_alias :: Data -> Data
        replace_data_type_alias (Product t g membs) =
            Product t g $ map replace_data_type_alias membs
        replace_data_type_alias (Sum t g membs) =
            Sum t g $ map replace_data_type_alias membs
        replace_data_type_alias (Record t var_binding) =
            Record (replace_usr_type_alias t) var_binding

        replace_term_type_alias :: Term -> Term
        replace_term_type_alias (Term tcons tname exprs) =
            Term (replace_usr_type_alias tcons) tname (map replace_lam_type_alias exprs)
        replace_term_type_alias (DependentTerm deps trm) =
            DependentTerm deps (replace_term_type_alias trm)

        replace_lam_type_alias :: Lambda -> Lambda
        replace_lam_type_alias (Lambda args expr) =
            Lambda args $ replace_expr_type_alias expr

        replace_expr_type_alias :: Expression -> Expression
        replace_expr_type_alias (Closure trm) = Closure $ replace_term_type_alias trm
        replace_expr_type_alias (Anonymous lam) = Anonymous $ replace_lam_type_alias lam
        replace_expr_type_alias (Return expr) = Return $ replace_expr_type_alias expr
        replace_expr_type_alias (Block scope) = Block $ map replace_expr_type_alias scope
        replace_expr_type_alias (If pred cond alt) = If (replace_expr_type_alias pred) (replace_expr_type_alias cond) (replace_expr_type_alias alt)
        replace_expr_type_alias (Else alt) = Else $ replace_expr_type_alias alt
        replace_expr_type_alias (Case expr cases) = Case (replace_expr_type_alias expr) (map convert_match cases)
          where convert_match :: Match -> Match
                convert_match (Match pat expr) = Match pat (replace_expr_type_alias expr)
        replace_expr_type_alias other = other

        -- for now we're only looking for empty generics, since TODO type aliases are not parametric
        replace_usr_type_alias :: Type -> Type
        replace_usr_type_alias original@(UsrType name []) =
            case look name of
                 Nothing -> original
                 Just newType -> newType
        replace_usr_type_alias (TermType a b) =
            TermType (replace_usr_type_alias a) (replace_usr_type_alias b)
        replace_usr_type_alias (Array t) =
            Array $ replace_usr_type_alias t
        replace_usr_type_alias other = other

term_partial_structs :: Term -> Maybe Data
term_partial_structs fun@(Term type_sig name exprs) =
    case (decons type_sig) of
         [a, b] -> Nothing
         [a] -> Nothing
         [] -> Nothing
         record@(x:xs) -> Just $ Sum (name++"_partial") []
                        $ (Record type_sig "term") : (map to_record (zip record [0..]))
  where decons :: Type -> [Type]
        decons (TermType a b) = a:decons b
        decons x = [x]
        to_record :: (Type, Int) -> Data 
        to_record (t, index) = Record t $ "arg"++(show index)
term_partial_structs fun@(DependentTerm _ trm) = term_partial_structs trm

preprocess :: Program -> Either String Program
preprocess (Program mods (Namespace scope_name namespaces tclasses structs aliases terms)) =
           case check_dup_decls $ Namespace scope_name namespaces tclasses (structs++add_partial_structs terms) aliases terms of
                Right uniq -> Right $ Program mods $ replace_namespace_type_alias $ uniq
                Left e -> Left e
  where add_partial_structs :: [Term] -> [Data]
        add_partial_structs (t:ts) =
            case term_partial_structs t of
                  Just struct -> struct:add_partial_structs ts
                  Nothing -> add_partial_structs ts
        add_partial_structs [] = []

-- backends

c_backend :: Interpreter
c_backend program@(Program m (Namespace name n c d a t)) = putStrLn $ concat $ map c_data_generator d

inter_comma :: [String] -> String
inter_comma [] = []
inter_comma [x] = x
inter_comma (l:ls) = l ++ ',' : inter_comma ls

c_data_generator :: Generator Data
c_data_generator struct@(Product name _ members) =
    concat ["struct ",name,"{enum{"
           ,inter_comma $ map enumeration members
           ,"}tag;union{"
           ,concat $ map c_data_generator members
           ,"}unwrap;}", name, ";"]
  where enumeration :: Data -> String
        enumeration (Product n _ _) = n
        enumeration (Sum n _ _) = n
        enumeration r = concat ["encountered ", show r, "in enumeration for product type"]
c_data_generator struct@(Sum name _ members) =
    concat ["struct ",name,"{"
           ,concat $ map c_data_generator members
           ,"}", name, ";"]
c_data_generator rec@(Record m_type m_name) = c_format_variable_name (" "++m_name) ";" m_type 

c_format_variable_name :: String -> String -> Type -> String
c_format_variable_name m_name separator m_type =
    case m_type of
         TermType _ _ -> function_pointer $ flatten_term_type m_type
         Array subtype -> array_type $ flatten_compound_type subtype 
         UsrType usr_name generic_names -> user_type usr_name $ concat $ map (c_format_variable_name "" "") $ generic_names
         ExternType scopes realtype -> concat ["CTNEXT_", concat $ scopes, "N", c_format_variable_name m_name separator realtype]
         TypeErr _ -> "type parsing error"
         _ -> concat [c_member_type_generator m_type, m_name, separator]
  where function_pointer :: [Type] -> String
        function_pointer flat = concat [c_format_variable_name "" "" $ last flat
                                       ,"(*",m_name,")("
                                       ,init $ concat $ map (c_format_variable_name "" ",") $ init flat
                                       ,")" ,separator]
        flatten_term_type :: Type -> [Type]
        flatten_term_type (TermType a b) = a:flatten_term_type b
        flatten_term_type t = [t]
        array_type :: String -> String
        array_type subtype = concat ["CTNSTARR_", subtype, m_name, separator]
        flatten_compound_type :: Type -> String
        flatten_compound_type (TermType a b) = concat ["P",flatten_compound_type a, "F",flatten_compound_type b,"P"]
        flatten_compound_type other = c_format_variable_name "" "" other
        user_type :: String -> String -> String
        user_type usrname genrname = concat [usrname, genrname, m_name, separator]

c_member_type_generator :: Generator Type
c_member_type_generator t =
    case t of
         Chr -> "char"
         Bl -> "uint8_t"
         F64 -> "double"
         F32 -> "float"
         I8 -> "int8_t"
         I16 -> "int16_t"
         I32 -> "int32_t"
         I64 -> "int64_t"
         U8 -> "uint8_t"
         U16 -> "uint16_t"
         U32 -> "uint32_t"
         U64 -> "uint64_t"
         _ -> "unknown type"

c_term_generator :: Generator Term
c_term_generator (Term t name guards) = "term unimplemented"
c_term_generator (DependentTerm _ t) = c_term_generator t

ast_backend :: Interpreter
ast_backend (Program m (Namespace _ n c d a t)) = mapM_ pPrint m >> (putStrLn "\n")
                                               >> mapM_ pPrint n >> (putStrLn "\n")
                                               >> mapM_ pPrint c >> (putStrLn "\n")
                                               >> mapM_ pPrint d >> (putStrLn "\n")
                                               >> mapM_ pPrint a >> (putStrLn "\n")
                                               >> mapM_ pPrint t >> (putStrLn "\n")

compiler :: Interpreter -> String -> IO ()
compiler compile program =
  case parse programFile "(unknown)" (remove_line_comments $ remove_block_comments program) of
       Left e -> putStrLn "Parse Error"
              >> print e
       Right p -> case preprocess p of
                       Left e -> putStrLn "Semantic Analysis Error"
                              >> print e
                       Right ast -> compile ast

main :: IO ()
main = getArgs >>= \args ->
       case length args of
            0 -> putStrLn "provide source file"
            _ -> readFile (head args) >>= (compiler c_backend)
