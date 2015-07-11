-- import Control.Monad.State
import Data.Monoid

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
    show (TokenIdentifier s) = "id-" ++ s


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
    (sourcedStr, cursor') = cursorTakewhile (\c _ -> not $ c `elem` " \n\t()") cursor 

tokenizeAccum :: [Sourced Token] -> TokenizerCursor -> [Sourced Token]
tokenizeAccum accum cursor @ Cursor { stream=stream, point=point} = 
    case cleanedStream of
        "" -> accum
        '(':_ -> takeOpenBracket accum cleanedCursor  
        ')':_ -> takeCloseBracket accum cleanedCursor
        _ -> takeIdentifier accum cleanedCursor
    where
        cleanedStream = dropWhile (\c -> c `elem` " \n\t") stream
        cleanedCursor = Cursor { stream=cleanedStream, point=point }

tokenize :: String -> [Sourced Token]
tokenize s = tokenizeAccum [] Cursor { stream=s, point=0 }
--tokenize :: String -> [Sourced Token]
--tokenize s = [Sourced {val=TokenIdentifier s, region=mempty}]

main :: IO ()
main = do
    input <- getLine
    print $ tokenize input

