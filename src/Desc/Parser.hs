{-# LANGUAGE TemplateHaskell #-}
module Desc.Parser (parseDesc) where

import           Control.Lens
import           Data.Maybe           (fromMaybe)
import           System.Directory     (doesFileExist)
import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.String   (Parser)
import           Text.Parsec.Token

data Desc = Desc {
      _descName   :: String,          -- Имя
      _descInput  :: String,          -- Каталог входных файлов
      _descEDS    :: String,          -- Имя EDS файла
      _descFilter :: Maybe String,    -- Необязательное имя к FLT файлу
      _descRegistry :: Maybe String,  -- Необязательное имя к REG файлу
      _descOutput :: String           -- Каталог для выходного кода значение по умолчанию "output"
    } deriving (Show)
makeLenses ''Desc

edsDef :: LanguageDef st
edsDef = LanguageDef
         { commentStart   = ""
         , commentEnd     = ""
         , commentLine    = ";"
         , nestedComments = True
         , identStart     = alphaNum
         , identLetter    = alphaNum
         , opStart        = oneOf ":"
         , opLetter       = oneOf ":"
         , reservedOpNames= []
         , reservedNames  = []
         , caseSensitive  = True
         }


lexer :: TokenParser st
lexer = makeTokenParser edsDef

descParser :: String -> Parser Desc
descParser input = do
    whiteSpace lexer
    pairs <- many keyValuePair
    let name    = lookup "Name" pairs
        eds     = lookup "EDS" pairs
        ffilter = lookup "Filter" pairs
        register= lookup "Register" pairs
        output  = fromMaybe "output" (lookup "Output" pairs)
    case (name, eds) of
        (Just n, Just e)   -> return $ Desc n input e ffilter register output
        (Just _, Nothing)  -> fail "Отсутсвует обязательное поле: Name"
        (Nothing, Just _)  -> fail "Отсутсвует обязательное поле: EDS"
        (Nothing, Nothing) -> fail "Отсутсвуют обязательные поля: Name, EDS"

keyValuePair :: Parser (String, String)
keyValuePair = do
    key <- identifier lexer
    whiteSpace lexer
    _ <- char '='
    whiteSpace lexer
    value <- valueParser
    return (key, value)

-- Parser for values (strings without newline or comment)
valueParser :: Parser String
valueParser = manyTill anyChar endOfLine

parseDesc :: FilePath -> IO (Either ParseError Desc)
parseDesc path =
    do
      ex <- doesFileExist path
      if ex
      then
          do
            content <- readFile path
            return (parse (descParser path) path content)
      else
          fail $ "Файл " ++ path ++ " не существует"