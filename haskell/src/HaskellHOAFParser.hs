-- | A Parser for Hanoi Omega-Automata Format (HOAF) based on jhoafparser.
module HaskellHOAFParser where
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ( void, void )
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Grammar from https://adl.github.io/hoaf/#header
-- header ::= format-version header-item*
-- format-version ::= "HOA:" IDENTIFIER
-- header-item ::= "States:" INT
--              | "Start:" state-conj
--              | "AP:" INT STRING*
--              | "Alias:" ANAME label-expr
--              | "Acceptance:" INT acceptance-cond
--              | "acc-name:" IDENTIFIER (BOOLEAN|INT|IDENTIFIER)*
--              | "tool:" STRING STRING?
--              | "name:" STRING
--              | "properties:" IDENTIFIER*
--              | HEADERNAME (BOOLEAN|INT|STRING|IDENTIFIER)*
data HOA = HOA
  { version :: Text
  , tool :: Maybe (Text, Maybe Text)
  , name :: Maybe Text
  , startStates :: [Int]
  , accName :: Maybe [Text]
  , acceptance :: Maybe (Int, Text)
  , properties :: [Text]
  , atomicPropositions :: [Text]
  , states :: [State]
  } deriving (Show, Eq)

data State = State
  { stateId :: Int
  , transitions :: [Transition]
  } deriving (Show, Eq)

data Transition = Transition
  { condition :: Text
  , target :: Int
  , accSet :: Maybe [Int]
  } deriving (Show, Eq)

data HeaderItem
  = States Int
  | Start [Int]
  | AP Int [Text]
  | Alias Text Text
  | Acceptance Int Text
  | AccName [Text]
  | Tool Text (Maybe Text)
  | Name Text
  | Properties [Text]
  | CustomHeader Text [Text]
  deriving (Show, Eq)

type Parser = Parsec Void Text

-- Helpers for parsing
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

quotedString :: Parser Text
quotedString = T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

-- HOA file parsers
hoaParser :: Parser HOA
hoaParser = do
  void $ symbol "HOA:"
  ver <- lexeme (takeWhile1P Nothing (/= '\n'))
  void newline
  headerItems <- many headerItemParser
  states <- stateBlockParser
  let toolItem = lookupTool headerItems
      nameItem = lookupName headerItems
      startStates = concatMap (\x -> case x of Start s -> s; _ -> []) headerItems
      accName = lookupAccName headerItems
      acceptance = lookupAcceptance headerItems
      properties = concatMap (\x -> case x of Properties p -> p; _ -> []) headerItems
      atomicProps = concatMap (\x -> case x of AP _ ps -> ps; _ -> []) headerItems
  return HOA
    { version = ver
    , tool = toolItem
    , name = nameItem
    , startStates = startStates
    , accName = accName
    , acceptance = acceptance
    , properties = properties
    , atomicPropositions = atomicProps
    , states = states
    }

headerItemParser :: Parser HeaderItem
headerItemParser =
  choice
    [ States <$> (symbol "States:" *> integer <* newline)
    , Start . (:[]) <$> (symbol "Start:" *> integer <* newline)
    , AP <$> (symbol "AP:" *> integer) <*> many quotedString <* newline
    , Alias <$> (symbol "Alias:" *> quotedString) <*> quotedString <* newline
    , Acceptance <$> (symbol "Acceptance:" *> integer) <*> lexeme (takeWhile1P Nothing (/= '\n'))
    , AccName <$> (symbol "acc-name:" *> many (lexeme (takeWhile1P Nothing (/= '\n'))))
    , Tool <$> (symbol "tool:" *> quotedString) <*> optional quotedString <* newline
    , Name <$> (symbol "name:" *> quotedString <* newline)
    , Properties <$> (symbol "properties:" *> many (lexeme (takeWhile1P Nothing (/= '\n'))))
    , CustomHeader <$> lexeme (takeWhile1P Nothing (/= ':')) <*> many (lexeme (takeWhile1P Nothing (/= '\n')))
    ]

lookupTool :: [HeaderItem] -> Maybe (Text, Maybe Text)
lookupTool = foldr f Nothing
  where
    f (Tool t1 t2) _ = Just (t1, t2)
    f _ acc = acc

lookupName :: [HeaderItem] -> Maybe Text
lookupName = foldr f Nothing
  where
    f (Name n) _ = Just n
    f _ acc = acc

lookupAccName :: [HeaderItem] -> Maybe [Text]
lookupAccName = foldr f Nothing
  where
    f (AccName a) _ = Just a
    f _ acc = acc

lookupAcceptance :: [HeaderItem] -> Maybe (Int, Text)
lookupAcceptance = foldr f Nothing
  where
    f (Acceptance n t) _ = Just (n, t)
    f _ acc = acc

stateBlockParser :: Parser [State]
stateBlockParser = do
  void $ symbol "--BODY--"
  void newline
  states <- many stateParser
  void $ symbol "--END--"
  return states

stateParser :: Parser State
stateParser = do
  void $ symbol "State:"
  sid <- integer
  trans <- many transitionParser
  return State {stateId = sid, transitions = trans}

transitionParser :: Parser Transition
transitionParser = do
  cond <- between (symbol "[") (symbol "]") (takeWhile1P Nothing (/= ']'))
  target <- integer
  accSet <- optional (symbol "{" *> many integer <* symbol "}")
  void newline
  return Transition {condition = cond, target = target, accSet = accSet}

-- Utility function to parse an HOA file
parseHOAFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) HOA)
parseHOAFile filePath = do
  content <- TIO.readFile filePath
  return $ runParser hoaParser filePath content
