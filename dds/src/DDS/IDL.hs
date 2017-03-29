module DDS.IDL where

import DDS.TopicXML
import Control.Monad (void, liftM)
import Data.List (intercalate, intersperse)
import Data.Char (isSpace)
import Data.Maybe
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

parseIDL :: String -> Either String MetaData
parseIDL idl = case parse specification "(anonymous)" idl of
  Left err -> Left $ show err
  Right defs -> Right $ MD defs

manySemi = flip endBy semi

specification = sc *> definitions <* eof

definitions = liftM catMaybes $ many definition_or_keylist

definition_or_keylist = hashline <|> definition

definition = do
  d <- def_module <|> def_enum <|> def_type <|> def_struct <|> def_union <|> def_const <?> "definition"
  semi
  return $ Just d

def_module = do
  reserved "module"
  name <- identifier
  defs <- braces $ definitions
  return $ DM name defs

def_enum = do
  reserved "enum"
  name <- identifier
  ns <- braces $ (sepBy1 identifier comma)
  return $ DE name (map (uncurry E) (zip ns [0..]))

def_struct = do
  reserved "struct"
  name <- identifier
  ms <- braces $ manySemi member
  return $ DS name $ concat ms

def_union = do
  reserved "union"
  name <- identifier
  reserved "switch"
  disctype <- parens basetype
  cs <- braces $ manySemi ucase
  return $ DU name disctype cs

def_const = do -- should not ignore type and support things other than ints
  reserved "const"
  void basetype
  name <- identifier
  symbol "="
  v <- idl_integer
  return $ DC name $ fromIntegral v

member = do
  ms <- declarator_list
  return $ map (uncurry M) ms

ucase = do
  ls <- endBy1 ulabel colon
  (name, t) <- declarator
  return $ UC name t ls

ulabel = a <|> b
  where
    a = reserved "case" >> (ax <|> ay)
    ax = idl_integer >>= (return . UL . show)
    ay = identifier >>= (return . UL)
    b = reserved "default" >> return ULDef

def_type = do
  reserved "typedef"
  (name, t) <- declarator
  return $ DT name t

declarator = do
  t <- basetype
  name <- identifier
  ds <- many (brackets integer)
  return (name, foldr TArr t $ map fromIntegral ds)  

declarator_list = do
  t <- basetype
  sepBy1 (declarator_list_elem t) comma

declarator_list_elem t = do
  name <- identifier
  ds <- many (brackets integer)
  return (name, foldr TArr t $ map fromIntegral ds)  

inttype = do
  u <- option False (reserved "unsigned" >> return True)
  s <- (reserved "short" >> return 0) <|>
       try (reserved "long" >> reserved "long" >> return 2) <|>
       (reserved "long" >> return 1) <?>
       "integer type"
  return $ case (s,u) of
    (0, False) -> TP TS
    (0, True)  -> TP TUS
    (1, False) -> TP TL
    (1, True)  -> TP TUL
    (2, False) -> TP TLL
    (2, True)  -> TP TULL

stringtype = do
  reserved "string"
  b <- option Nothing (angles integer >>= return . Just . fromIntegral)
  return $ TStr b

seqtype = do
  reserved "sequence"
  symbol "<"
  t <- basetype
  b <- option Nothing (comma >> integer >>= return . Just . fromIntegral)
  symbol ">"
  return $ TSeq b t

sname = do
  a <- option "" (scope >> return "::")
  bs <- sepBy1 identifier scope
  return $ concat (a : intersperse "::" bs)

basetype =
  inttype <|>
  (reserved "char"    >> (return $ TP TC)) <|>
  (reserved "boolean" >> (return $ TP TB)) <|>
  (reserved "octet"   >> (return $ TP TO)) <|>
  (reserved "float"   >> (return $ TP TF)) <|>
  (reserved "double"  >> (return $ TP TD)) <|>
  seqtype    <|>
  stringtype <|>
  (def_struct >>= return . TDef) <|>
  (def_enum   >>= return . TDef) <|>
  (sname      >>= return . TR) <?>
  "type"

hashline = symbol "#" >> (pragma <|> linenumber)

pragma = symbol "pragma" >> (keylist <|> prefix)

keylist = do
  symbol "keylist"
  name <- snameE
  keys <- many fieldE
  void eol <|> eof
  sc
  return $ Just (DK name keys)

prefix = do
  symbol "prefix"
  name <- snameE
  prefix <- identifierE
  void eol <|> eof
  sc
  return $ Just (DP name prefix)

linenumber = do
  integer
  L.skipLineComment ""
  sc
  return Nothing

fieldE =
  sepBy1 identifierE dotE >>= return . intercalate "."

snameE = do
  a <- option "" (scope >> return "::")
  bs <- sepBy1 identifierE scopeE
  return $ concat (a:bs)

sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = L.space (void spaceChar) lineComment blockComment
  where lineComment  = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

scE :: Parser () -- like sc, but not consuming EOL
scE = L.space (void spaceCharE) lineComment blockComment
  where lineComment  = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"
        spaceCharE   = satisfy isSpaceE <?> "white space"
        isSpaceE x   = isSpace x && x /= '\n' && x /= '\r'

lexeme = L.lexeme sc
symbol = L.symbol sc

parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
angles = between (symbol "<") (symbol ">")
brackets = between (symbol "[") (symbol "]")

integer = lexeme L.integer

idl_integer = do
  try (char '0' >> char' 'x' >> L.hexadecimal)
  <|> try (char '0' >> L.octal)
  <|> integer

semi = symbol ";"
comma = symbol ","
colon = symbol ":"
dot = symbol "."
dotE = L.symbol scE "."
scope = symbol "::"
scopeE = L.symbol scE "::"

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String] -- list of reserved words
rws = [ "boolean", "octet", "short", "long", "unsigned", "string", "float",
        "double", "enum", "sequence", "struct", "union", "switch", "case",
        "default", "typedef", "const", "module" ]

identifierX :: Parser () -> Parser String
identifierX sc = L.lexeme sc (p >>= check)
  where p       = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
        check x = if x `elem` rws
                  then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                  else return x

identifier = identifierX sc
identifierE = identifierX scE
