module Main where

import           Control.Monad.State
import           Control.Monad.Except

import           Desc.Parser
import           Generator

type Ctx a = StateT (GeneratorCtx ()) (ExceptT GeneratorError IO) a

run :: Ctx ()
run =
    do
      pure ()

main :: IO ()
main = do
  maybeDesc <- parseDesc "input/test.desc"
  case maybeDesc of
    Left err -> putStrLn $ "Ошибка: " ++ (show err)
    Right desc ->
      do
        let
          gen = initGeneratorCtx desc
        result <- runExceptT $ evalStateT run gen
        case result of
          Left err -> putStrLn $ "Ошибка: " ++ (show err)
          Right _  -> putStrLn "Успех"