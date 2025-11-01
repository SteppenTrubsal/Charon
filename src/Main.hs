module Main where

import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Except

import           Desc.Parser

-- type Ctx a = StateT (GeneratorCtx ()) (ExceptT GeneratorError IO) a

-- run :: Ctx ()
-- run =
--     do
--       ()

main :: IO ()
main = do
  maybeDesc <- parseDesc "input/test.desc"
  case maybeDesc of
    Left err -> putStrLn $ "Ошибка: " ++ (show err)
    Right desc -> putStrLn "Успех"
      -- do
      --   let
      --     gen = initGeneratorCtx desc
      --   result <- runExceptT $ evalStateT run gen
      --   case result of
      --     Left err -> putStrLn $ "Ошибка: " ++ (show err)
      --     Right _  -> putStrLn "Успех"
