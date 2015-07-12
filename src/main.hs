{-# LANGUAGE RecordWildCards #-}
-- import Control.Monad.State
import Data.Monoid
import Data.Maybe
import Text.Read

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

instance Show a => Show (Sourced a) where
    show Sourced {val=val, region=Region (begin, end)} = "(" ++ (show begin) ++ ":" ++ (show end)  ++ ")" ++ (show val)

instance Functor Sourced where
    fmap f sourced = Sourced { val=f $ val sourced, region= region sourced}

--Token
data Token = TokenOpenBracket | TokenCloseBracket | TokenIdentifier String

instance Show Token where
    show TokenOpenBracket = "("
    show TokenCloseBracket = ")"
    show (TokenIdentifier s) = "tok-" ++ s


--Cursor
data Cursor a = Cursor {
    stream :: [a],
    point :: Point
}

cursorTakewhile :: (a -> Point -> Bool) -> Cursor a -> (Sourced [a], Cursor a)
cursorTakewhile taker Cursor { stream=stream, point=begin } = (sourcedVal, cursor') where
    sourcedVal = Sourced { val=headstream, region=Region(begin, end) }
    cursor' = Cursor {
        stream = tailstream,
        point = end
    }
    headstream = map fst $ takeWhile (uncurry taker) indexed_stream
    tailstream = map fst $ dropWhile (uncurry taker) indexed_stream
    indexed_stream = (zip stream [0..])
    end = begin + length headstream


--Tokenizer Code
type TokenizerCursor = Cursor Char

tokens_glyphs :: String
tokens_glyphs = "()"

tokens_whitespace:: String
tokens_whitespace = " \n\t"

takeOpenBracket :: [Sourced Token] -> TokenizerCursor -> [Sourced Token]
takeOpenBracket accum cursor = tokenizeAccum (accum ++ [token]) cursor' where 
    token = fmap (const TokenOpenBracket) sourcedStr
    (sourcedStr, cursor') = cursorTakewhile (\_ index -> index == 0) cursor 

takeCloseBracket :: [Sourced Token] -> TokenizerCursor -> [Sourced Token]
takeCloseBracket accum cursor = tokenizeAccum (accum ++ [token]) cursor' where 
    token = fmap (const TokenCloseBracket) sourcedStr
    (sourcedStr, cursor') = cursorTakewhile (\_ index -> index == 0) cursor 

takeIdentifier :: [Sourced Token] -> TokenizerCursor -> [Sourced Token]
takeIdentifier accum cursor = tokenizeAccum (accum ++ [token]) cursor' where 
    token = fmap TokenIdentifier sourcedStr
    (sourcedStr, cursor') = cursorTakewhile (\c _ -> not $ c `elem` tokens_whitespace ++ tokens_glyphs) cursor 

tokenizeAccum :: [Sourced Token] -> TokenizerCursor -> [Sourced Token]
tokenizeAccum accum cursor @ Cursor { stream=stream, point=point} = 
    case cleanedStream of
        "" -> accum
        '(':_ -> takeOpenBracket accum cleanedCursor  
        ')':_ -> takeCloseBracket accum cleanedCursor
        _ -> takeIdentifier accum cleanedCursor
    where
        whitespace = takeWhile (\c -> c `elem` tokens_whitespace) stream
        cleanedStream = dropWhile (\c -> c `elem` tokens_whitespace) stream
        cleanedCursor = Cursor { stream=cleanedStream, point=point + length whitespace}

tokenize :: String -> [Sourced Token]
tokenize s = tokenizeAccum [] Cursor { stream=s, point=0 }

--AST
data AST = AtomInt Int | AtomFloat Double | AtomId String | MoleculeList [AST]

instance Show AST where
    show (AtomInt i) = "i-" ++ show i
    show (AtomFloat f) = "f-" ++ show f
    show (AtomId ident) = "id-" ++ show ident
    show (MoleculeList list) = show list

--Parser
type ParserCursor = Cursor (Sourced Token)
data ParseError = UnmatchedBrackets deriving (Show)
type ParseResult r = Either (Sourced ParseError) r

parseList :: [Sourced AST] -> ParserCursor -> ParseResult [Sourced AST]
parseList accum Cursor {..} = undefined

parseIdentifier :: [Sourced AST] -> ParserCursor -> ParseResult [Sourced AST]
parseIdentifier accum Cursor {..} = parseAccum (accum ++ [sourcedAtom]) cursor' where
    sourcedAtom = Sourced { val=atom, region=atomRegion } 
    atom = 
	if (isJust maybeInt)
            then AtomInt (fromJust maybeInt)
	    else if (isJust maybeFloat)
                then AtomFloat (fromJust maybeFloat)
		else AtomId str
    cursor' = Cursor {
    stream = tail stream,
    point = point + 1
    }
    
    maybeInt = readMaybe str :: Maybe Int
    maybeFloat = readMaybe str :: Maybe Double
    
    str = case val (head stream) of 
              TokenIdentifier ident -> ident
              _ -> undefined
    atomRegion = region (head stream)

parseAccum :: [Sourced AST] -> ParserCursor -> ParseResult [Sourced AST]
parseAccum accum Cursor {stream=[], ..} = Right accum
parseAccum accum cursor@Cursor {..} = 
    case peek of
        TokenOpenBracket -> parseList accum cursor 
        (TokenIdentifier _)-> parseIdentifier accum cursor
        TokenCloseBracket -> Left $ Sourced {val=UnmatchedBrackets, region=peekRegion} 
    where
    peek = val (head stream)
    peekRegion = region (head stream)


parse :: [Sourced Token] -> ParseResult [Sourced AST]
parse tokens = parseAccum [] Cursor {stream=tokens, point=0}

main :: IO ()
main = do
    input <- getLine
    let tokens = tokenize input
    let parsed = parse tokens
    --print $ tokenize input
    
    case parsed of
      (Right parseTree) -> print parseTree
      (Left parseError) -> print $  "parse error:\n" ++ (show parseError) 

