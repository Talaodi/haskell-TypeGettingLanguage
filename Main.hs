module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Tuple
import qualified System.Environment as SE

type TypeName = String

type Id = String

data Program
  = Get Id
  | Define Id TypeName
  | Block [Program]
  deriving (Show)

data Env
  = None
  | Env {envIds :: [(Id, TypeName)], envNext :: Env}
  deriving (Show)

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap f (Parser g) = Parser $ \input -> do
    (input', x) <- g input
    return (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser f) <*> (Parser x) = Parser $ \input -> do
    (input', g) <- f input
    (input'', a) <- x input'
    return (input'', g a)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser f) <|> (Parser g) = Parser $ \input -> f input <|> g input

charParser :: Char -> Parser Char
charParser c = Parser f
  where
    f (x : xs)
      | x == c = Just (xs, x)
    f _ = Nothing

stringParser :: String -> Parser String
stringParser = sequenceA . map charParser

alphaParser :: Parser Char
alphaParser = foldl (<|>) empty (map charParser $ ['a' .. 'z'] ++ ['A' .. 'Z'])

digitParser :: Parser Char
digitParser = foldl (<|>) empty (map charParser ['0' .. '9'])

idCharParser :: Parser Char
idCharParser = alphaParser <|> digitParser

whiteSpaceParser :: Parser Char
whiteSpaceParser = foldl (<|>) empty (map charParser " \n\t")

whiteParser :: Parser String
whiteParser = many whiteSpaceParser

whiteParser' :: Parser String
whiteParser' = some whiteSpaceParser

idParser :: Parser String
idParser = (:) <$> alphaParser <*> many idCharParser

typeParser :: Parser String
typeParser = stringParser "int" <|> stringParser "char" <|> stringParser "bool"

getParser :: Parser Program
getParser = Get <$> (whiteParser *> idParser <* whiteParser <* charParser ';' <* whiteParser)

defineParser :: Parser Program
defineParser = (\t _ i -> Define i t) <$> (whiteParser *> typeParser) <*> whiteParser' <*> idParser <* whiteParser <* charParser ';' <* whiteParser

blockParser :: Parser Program
blockParser = Block <$> (whiteParser *> charParser '{' *> many programParser <* whiteParser <* charParser '}' <* whiteParser)

programParser :: Parser Program
programParser = blockParser <|> getParser <|> defineParser 

printType :: Id -> Maybe TypeName -> Maybe (IO ())
printType _ Nothing = Nothing
printType id (Just typeName) = Just $ putStrLn $ id ++ " : " ++ typeName

lookupEnv :: Env -> Id -> Maybe TypeName
lookupEnv None _ = Nothing
lookupEnv (Env ids next) id =
  case lookup id ids of
    Nothing -> lookupEnv next id
    typeName -> typeName

printTabs :: Integer -> IO ()
printTabs n = putStr $ foldl (\xs _ -> '\t' : xs) "" [1 .. n]

doStatement :: Env -> [IO ()] -> Integer -> Program -> Maybe (Env, [IO ()])
doStatement env xs tabs (Get id) = do
  newIO <- (printType id $ lookupEnv env id)
  return (env, xs ++ [printTabs tabs, newIO])
doStatement env@(Env ids next) xs _ (Define id typeName) =
  case lookup id ids of
    Nothing -> Just (Env ((id, typeName) : ids) next, xs)
    _ -> Nothing
doStatement None xs _ (Define id typeName) = Just (Env [(id, typeName)] None, xs)
doStatement env xs tabs (Block stmts) = foldM (\(es, is) -> doStatement es is $ tabs + 1) (Env [] env, []) stmts >>= (Just . (,) env . add . snd)
  where
    add is = printTabs tabs : putStrLn "{" : is ++ [printTabs tabs, putStrLn "}"]

runCode :: String -> IO ()
runCode s =
  case runParser programParser s >>= (doStatement None [] 0 . snd) of
    Nothing -> putStrLn "Failed."
    (Just (_, xs)) -> mapM_ id xs

main :: IO ()
main = do
  files <- SE.getArgs
  if null files then
    putStrLn "No files!"
  else do
    code <- readFile $ head files
    runCode code