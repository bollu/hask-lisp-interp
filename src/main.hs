{-# LANGUAGE RecordWildCards #-}
-- import Control.Monad.State
import Data.Monoid
import Data.Maybe()
import Text.Read()
import Control.Monad.State()

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

instance Show a => Show (Sourced a) where
    show Sourced {val=val, region=Region (begin, end)} = "(" ++ show begin ++ ":" ++ show end  ++ ")" ++ show val

instance Functor Sourced where
    fmap f sourced = Sourced { val=f $ val sourced, region=region sourced}

--Token
data Token = TokenOpenBracket | TokenCloseBracket | TokenIdentifier String | TokenString String

instance Show Token where
    show TokenOpenBracket = "("
    show TokenCloseBracket = ")"
    show (TokenIdentifier s) = "tok-" ++ s
    show (TokenString s) = "\"" ++ s ++ "\""


--Cursor
class StreamError e where
  emptyError :: e

data Cursor a e = Cursor {
  stream :: [a],
  point :: Point
 }

cursorEmpty :: Cursor a e -> Bool
cursorEmpty Cursor{..} = null stream

cursorAdvance :: StreamError e => Int -> Cursor a e -> Either e (Sourced [a], Cursor a e)
cursorAdvance move Cursor{..} =
  if length stream < move
  then Left  emptyError
  else Right (sourcedValue, cursor')
  where
    sourcedValue = Sourced { val=value, region=Region(point, point') }
    value = take move stream
    point' = point + move
    cursor' = Cursor { stream=drop move stream, point=point' }
                                  
cursorPeek :: StreamError e => Int -> Cursor a e -> Either e a
cursorPeek index Cursor{..} =
  if length stream < index
     then Left emptyError
     else Right $ stream !! index

data TakeWhileType = QuitOnEnd | ErrorOnEnd

cursorTakeWhileAccum :: StreamError e => Sourced [a] -> TakeWhileType -> (a -> Bool) -> Cursor a e -> Either e (Sourced [a], Cursor a e)
cursorTakeWhileAccum accum takingtype taker cursor =
  if cursorEmpty cursor
    then
      case takingtype of
      QuitOnEnd -> return (accum, cursor)
      ErrorOnEnd -> Left emptyError
    else
      do
        peek <- cursorPeek 0 cursor
        if taker peek
          then do
            (current, cursor') <- cursorAdvance 1 cursor
            cursorTakeWhileAccum (accum <> current) takingtype taker cursor' 
          else Right (accum, cursor)

cursorTakeWhile :: StreamError e => TakeWhileType ->  (a -> Bool) -> Cursor a e -> Either e (Sourced [a], Cursor a e)
cursorTakeWhile takingtype taker cursor = cursorTakeWhileAccum emptyaccum takingtype  taker cursor where
  emptyaccum = Sourced {val=[], region=Region(point cursor, point cursor)}

--Tokenizer
data TokenizerError = UnexpectedEOF | UnclosedString (Sourced String) deriving (Show)
instance StreamError TokenizerError where
  emptyError = UnexpectedEOF 


tokenizeOpenBracket :: [Sourced Token] -> Cursor Char TokenizerError -> Either TokenizerError [Sourced Token]
tokenizeOpenBracket accum cursor = do
  (sourcedStr, cursor') <- cursorAdvance 1 cursor
  let sourcedTok = fmap (const TokenOpenBracket) sourcedStr
  tokenizeAccum (accum ++ [sourcedTok]) cursor'

tokenizeCloseBracket :: [Sourced Token] -> Cursor Char TokenizerError -> Either TokenizerError [Sourced Token]
tokenizeCloseBracket accum cursor = do
  (sourcedStr, cursor') <- cursorAdvance 1 cursor
  let sourcedTok = fmap (const TokenCloseBracket) sourcedStr
  tokenizeAccum (accum ++ [sourcedTok]) cursor'

tokenizeIdentifier :: [Sourced Token] -> Cursor Char TokenizerError -> Either TokenizerError [Sourced Token]
tokenizeIdentifier accum cursor =
        do
          (sourcedStr, cursor') <- cursorTakeWhile QuitOnEnd (`notElem` "() \n\t") cursor
          let sourcedTok = fmap TokenIdentifier sourcedStr
          tokenizeAccum (accum ++ [sourcedTok]) cursor'

tokenizeString :: [Sourced Token] -> Cursor Char TokenizerError -> Either TokenizerError [Sourced Token]
tokenizeString accum cursor =
        do
          (_, cursorNext) <- cursorAdvance 1 cursor--consume the "
          (sourcedStr, cursor') <- cursorTakeWhile QuitOnEnd (/= '\"') cursorNext
          
          if cursorEmpty cursor' 
            then
              Left (UnclosedString sourcedStr)    
            else do  
              (_, cursorAbsorbed) <- cursorAdvance 1 cursor' --absorb the '\"'
              let sourcedTok = fmap TokenString sourcedStr
              tokenizeAccum (accum ++ [sourcedTok]) cursorAbsorbed


tokenizeAccum :: [Sourced Token] -> Cursor Char TokenizerError -> Either TokenizerError [Sourced Token]
tokenizeAccum accum cursor =
  if cursorEmpty cursor
     then Right accum
     else do
       (_, cursor') <- cursorTakeWhile QuitOnEnd (`elem` " \n\t") cursor
       peek <- cursorPeek 0 cursor'
       case peek of
         '(' -> tokenizeOpenBracket accum cursor'
         ')' -> tokenizeCloseBracket accum cursor'
         '\"' -> tokenizeString accum cursor'
         _ -> tokenizeIdentifier accum cursor'


tokenize :: String -> Either TokenizerError [Sourced Token]
tokenize src = tokenizeAccum [] cursor where
  cursor = Cursor { stream=src, point=0}

--Parser
--data ParseError = UnbalancedParantheses
--data AST = List [AST] | AtomId String | AtomInt Int | AtomFloat Double

--parse :: [Sourced Token]  -> [Sourced AST]
main :: IO ()
main = do
    input <- getLine
    print $ tokenize input
    
