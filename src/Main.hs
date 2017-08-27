{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Data.Bool
import           Data.Functor
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text.Lens
import           Options.Applicative
import           System.Exit
import           System.IO
import           Text.Megaparsec


newtype Options = Options
  { _end :: [String]
  } deriving (Show)

makeFieldsNoPrefix ''Options

optionsP :: Parser Options
optionsP = Options
  <$> ( some ( strOption
        ( long "end"
       <> short 'e'
       <> metavar "END"
       <> help ("Read until END in operation mode 2 (default: " <> show defEnd <> ")") ) )
    <|> pure defEnd )

defEnd :: [String]
defEnd = ["-1", "0"]


data Operation
  = ReadNLines Int
  | ReadUntilSpecialCharacter [Text]
  | ReadUntilEof
  deriving (Show, Eq)

makeClassyPrisms ''Operation

type MParser = Parsec Dec Text

exprP :: MParser Bool
exprP = do
  binop <- string "AND" $> (&&) <|> string "OR" $> (||)
  char ' '
  l <- boolP
  char ' '
  r <- boolP
  pure $ binop l r

  where
    boolP :: MParser Bool
    boolP = toEnum . read . pure <$> oneOf ("01" :: String)


data Teletype param
  = GetOperation (Operation -> param)
  | GetExpr (Bool -> param)
  | GetExprOrLine (Text -> param) (Bool -> param)
  | GetExprOrEof param (Bool -> param)
  | PrintLine String param
  | ExitWithError String
  | Exit
  deriving (Functor)

makeFree ''Teletype

type TeletypeM = Free Teletype


runTeletypeIO :: TeletypeM a -> IO a
runTeletypeIO = iterM run where
  run :: Teletype (IO a) -> IO a
  run (GetOperation f) = do
    options <- execParser opts
    line <- TIO.getLine
    operation <- case line of
      "1" -> ReadNLines . read . T.unpack <$> TIO.getLine
      "2" -> pure $ ReadUntilSpecialCharacter $ options ^.. end.traverse.packed
      "3" -> pure ReadUntilEof
      _ -> die "Invalid mode"
    f operation

  run (GetExpr f) = parseExpr <$> TIO.getLine >>= either (const (die "Invalid expression")) f

  run (GetExprOrLine ferror f) = TIO.getLine >>= \line ->
    either (const (ferror line)) f $ parseExpr line

  run (GetExprOrEof end f) = isEOF >>= bool (run (GetExpr f)) end

  run (PrintLine s rest) = TIO.putStrLn (T.pack s) >> rest
  run (ExitWithError msg) = die msg
  run Exit = exitSuccess

  parseExpr = parse exprP "<stdin>"

  opts = info (optionsP <**> helper)
    ( fullDesc
   <> progDesc "Evaluate logical expressions"
   <> header "hello - not a simple \"print Hello World\" problem" )


program :: TeletypeM ()
program = do
  operation <- getOperation
  case operation of
    ReadNLines n -> readNLines n
    ReadUntilSpecialCharacter chars -> readUntilSpecialCharacter chars
    ReadUntilEof -> readUntilEof
  exit

readNLines :: Int -> TeletypeM ()
readNLines n = replicateM_ n $ getExpr >>= printBool

readUntilSpecialCharacter :: [Text] -> TeletypeM ()
readUntilSpecialCharacter chars = forever $ getExprOrLine >>= \case
  Left t -> if elem t chars
            then exit
            else exitWithError $ "Unexpected input: " <> show t
  Right b -> printBool b

readUntilEof :: TeletypeM ()
readUntilEof = forever $ getExprOrEof >>= maybe exit printBool

printBool :: Bool -> TeletypeM ()
printBool = printLine . show . fromEnum


main :: IO ()
main = runTeletypeIO program
