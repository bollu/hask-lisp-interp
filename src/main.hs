{-# LANGUAGE RecordWildCards #-}
-- import Control.Monad.State

import Data.Monoid
import Data.Maybe()
import Text.Read()
import Control.Monad.State()
import System.Exit

type Point = Int
newtype Region = Region (Point, Point) deriving (Eq, Show)

instance Monoid Region where
    mempty = Region (0, 0)
    mappend (Region (begin, _)) (Region(_, end)) = Region (begin, end)

--Sourced
data Sourced a = Sourced {
    val :: a,
    region :: Region
}

instance Monoid a =>  Monoid (Sourced a) where
  mempty = Sourced {val = mempty, region=mempty}
  mappend source1 source2 = Sourced { val=val source1 <> val source2, region=region source1 <> region source2}

appendSourced :: Sourced [a] -> Sourced a -> Sourced [a]
appendSourced list atom = Sourced { val= val list ++ [val atom], region=region list <> region atom }

instance Show a => Show (Sourced a) where
    show Sourced {val=val, region=Region (begin, end)} = "(" ++ show begin ++ ":" ++ show end  ++ ")" ++ show val

instance Functor Sourced where
    fmap f sourced = Sourced { val=f $ val sourced, region=region sourced}

--Token
data Token = TokenOpenBracket | TokenCloseBracket | TokenIdentifier String | TokenString String | TokenBool Bool

instance Show Token where
    show TokenOpenBracket = "("
    show TokenCloseBracket = ")"
    show (TokenIdentifier s) = "id-" ++ s
    show (TokenString s) = "\"" ++ s ++ "\""
    show (TokenBool b) = "bool-" ++ show b

--Cursor
class StreamError e where
  emptyError :: e

data Cursor a e = Cursor {
  stream :: [a],
  point :: Point
}

cursorEmpty :: Cursor a e -> Bool
cursorEmpty Cursor{..} = null stream

cursorTake :: StreamError e => Int -> Cursor a e -> Either e (Sourced [a], Cursor a e)
cursorTake move Cursor{..} =
  if length stream < move
  then Left  emptyError
  else Right (sourcedValue, cursor')
  where
    sourcedValue = Sourced { val=value, region=Region(point, point') }
    value = take move stream
    point' = point + move
    cursor' = Cursor { stream=drop move stream, point=point' }

cursorAdvance :: StreamError e => Cursor a e -> Either e (Sourced a, Cursor a e)
cursorAdvance cursor = do
  (sourcedListVal, cursor') <- cursorTake 1 cursor
  let value = head $ val sourcedListVal
  return (Sourced {val=value, region=region sourcedListVal}, cursor')

cursorPeek :: StreamError e => Int -> Cursor a e -> Either e a
cursorPeek index Cursor{..} =
  if length stream <= index
     then Left emptyError
     else Right $ stream !! index


cursorTakeWhileAccum :: StreamError e => Sourced [a]  -> (a -> Bool) -> Cursor a e -> Either e (Sourced [a], Cursor a e)
cursorTakeWhileAccum accum taker cursor =
  if cursorEmpty cursor
    then
      return (accum, cursor)
    else
      do
        peek <- cursorPeek 0 cursor
        if taker peek
          then do
            (current, cursor') <- cursorAdvance cursor
            cursorTakeWhileAccum (accum `appendSourced` current) taker cursor' 
          else Right (accum, cursor)

cursorTakeWhile :: StreamError e =>  (a -> Bool) -> Cursor a e -> Either e (Sourced [a], Cursor a e)
cursorTakeWhile taker cursor = cursorTakeWhileAccum emptyaccum taker cursor where
  emptyaccum = Sourced {val=[], region=Region(point cursor, point cursor)}

--Tokenizer
data TokenizerError = UnexpectedEOF | UnclosedString (Sourced String) deriving (Show)
instance StreamError TokenizerError where
  emptyError = UnexpectedEOF 


tokenizeOpenBracket :: [Sourced Token] -> Cursor Char TokenizerError -> Either TokenizerError [Sourced Token]
tokenizeOpenBracket accum cursor = do
  (sourcedStr, cursor') <- cursorAdvance cursor
  let sourcedTok = fmap (const TokenOpenBracket) sourcedStr
  tokenizeAccum (accum ++ [sourcedTok]) cursor'

tokenizeCloseBracket :: [Sourced Token] -> Cursor Char TokenizerError -> Either TokenizerError [Sourced Token]
tokenizeCloseBracket accum cursor = do
  (sourcedStr, cursor') <- cursorAdvance cursor
  let sourcedTok = fmap (const TokenCloseBracket) sourcedStr
  tokenizeAccum (accum ++ [sourcedTok]) cursor'

tokenizeIdentifier :: [Sourced Token] -> Cursor Char TokenizerError -> Either TokenizerError [Sourced Token]
tokenizeIdentifier accum cursor =
        do
          (sourcedStr, cursor') <- cursorTakeWhile (`notElem` "() \n\t") cursor
          let sourcedTok = --check for keywords 
                case val sourcedStr of
                    "true" -> fmap (const (TokenBool True)) sourcedStr
                    "false" -> fmap (const (TokenBool False)) sourcedStr
                    _ -> fmap TokenIdentifier sourcedStr
      
          tokenizeAccum (accum ++ [sourcedTok]) cursor'

tokenizeString :: [Sourced Token] -> Cursor Char TokenizerError -> Either TokenizerError [Sourced Token]
tokenizeString accum cursor =
        do
          (_, cursorNext) <- cursorAdvance cursor --consume the "
          (sourcedStr, cursor') <- cursorTakeWhile (/= '\"') cursorNext
          
          if cursorEmpty cursor' 
            then
              Left (UnclosedString sourcedStr)    
            else do  
              (_, cursorAbsorbed) <- cursorAdvance cursor' --absorb the '\"'
              let sourcedTok = fmap TokenString sourcedStr
              tokenizeAccum (accum ++ [sourcedTok]) cursorAbsorbed


tokenizeAccum :: [Sourced Token] -> Cursor Char TokenizerError -> Either TokenizerError [Sourced Token]
tokenizeAccum accum cursor =
  do
    (_, cursor') <- cursorTakeWhile (`elem` " \n\t") cursor --cleanup whtespace
    
    if cursorEmpty cursor'
       then return accum
       else do
         peek <- cursorPeek 0 cursor'
         case peek of
           '(' -> tokenizeOpenBracket accum cursor'
           ')' -> tokenizeCloseBracket accum cursor'
           '\"' -> tokenizeString accum cursor'
           _ -> tokenizeIdentifier accum cursor'


tokenize :: String -> Either TokenizerError [Sourced Token]
tokenize src = tokenizeAccum [] cursor where
  cursor = Cursor {stream=src, point=0}

--Parser
data ParseError = UnbalancedParantheses (Sourced Token) | UnexpectedToken (Sourced Token) | UnexpectedEndOfTokens deriving(Show)

instance StreamError ParseError where
  emptyError = UnexpectedEndOfTokens

data AST = ASTList [Sourced AST] | AtomId String | AtomInt Int | AtomFloat Double

instance Show AST where
  show (ASTList l) = "( " ++ foldl  (\a b -> a ++ " " ++ b) "" (map show l) ++ " )" 
  show (AtomId str) = "atomid-" ++  str
  show (AtomInt int) = show int
  show (AtomFloat float) = show float

type ParseCursor = Cursor (Sourced Token) ParseError

parseListAccum :: [Sourced AST] -> ParseCursor -> Either ParseError ([Sourced AST], ParseCursor)
parseListAccum accum cursor = do
  Sourced{val=peek} <- cursorPeek 0 cursor

  case peek of
    TokenCloseBracket -> return (accum, cursor)
    _ -> do
      (ast, cursor') <- parseSingle cursor
      parseListAccum (accum ++ [ast]) cursor'

parseList :: ParseCursor -> Either ParseError (Sourced AST, ParseCursor)
parseList cursor = do
  (Sourced{val=tokenBegin}, cursorBegin) <- cursorAdvance cursor
  (sourcedList, cursorAtCloseBracket) <- parseListAccum [] cursorBegin
  (Sourced{val=tokenEnd}, cursorEnd) <- cursorAdvance cursorAtCloseBracket

  let totalRegion = region tokenBegin <> region tokenEnd
  
  return (Sourced{val=ASTList sourcedList, region=totalRegion}, cursorEnd)
  --parseAccum (accum ++ list) cursorEnd

parseIdentifier :: Cursor (Sourced Token) ParseError -> Either ParseError (Sourced AST, ParseCursor)
parseIdentifier cursor = do
  (Sourced{val=idSourcedToken}, cursor') <- cursorAdvance cursor
  
  case val idSourcedToken of
    TokenIdentifier idStr -> return (atomId, cursor')
      where atomId = Sourced {val = AtomId idStr, region=region idSourcedToken}
    _ -> Left (UnexpectedToken idSourcedToken) 

parseSingle ::  Cursor (Sourced Token) ParseError -> Either ParseError (Sourced AST, ParseCursor)
parseSingle cursor =
  if cursorEmpty cursor
     then Left UnexpectedEndOfTokens
     else do
       sourcedPeek @ Sourced{val=peek} <- cursorPeek 0 cursor

       case peek of
         TokenOpenBracket -> parseList cursor
         TokenIdentifier _ -> parseIdentifier cursor  
         TokenCloseBracket -> Left (UnbalancedParantheses sourcedPeek)
         
         _ -> undefined
       

parseAccum :: [Sourced AST] -> ParseCursor -> Either ParseError [Sourced AST]
parseAccum accum cursor =
  if cursorEmpty cursor
     then return accum
  else do
     (ast, cursor') <- parseSingle cursor
     parseAccum (accum ++ [ast]) cursor'

parse :: [Sourced Token]  -> Either ParseError [Sourced AST]
parse tokens = parseAccum [] cursor where
  cursor = Cursor { stream=tokens, point=0}

main :: IO ()
main = do
    input <- getLine
    let tokensResult = tokenize input

    case tokensResult of
      Right tokens -> (print tokens) >> (putStrLn ("parse:\n" ++ (show $ parse tokens)))
      Left tokenizationError -> putStrLn ("tokenization error:\n" ++ (show tokenizationError)) >> exitFailure

    
   --print tokensResult
    --let parseResult =
    --      case tokensResult of
    --        Right tokens -> parse tokens
    --        Left _ -> Right []
    
    -- print parseResult
