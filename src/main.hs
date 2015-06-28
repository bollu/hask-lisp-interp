import Control.Monad
import Control.Applicative
import Control.Monad.State
import Data.List
import Control.Monad.Trans.Except

-- basic cursor type
data Cursor i = Cursor {
    input :: [i]
}


_takeCursorWhile :: (i -> Bool) -> Cursor i -> ([i], Cursor i)
_takeCursorWhile acceptor cursor = (output, cursor') where
    output = takeWhile acceptor (input cursor)
    cursor' = Cursor {
        input = dropWhile acceptor (input cursor)
    }  

takeCursorWhile :: (i -> Bool) -> State (Cursor i) [i]
takeCursorWhile acceptor = state $ \cursor -> (_takeCursorWhile acceptor cursor)


peekCursor :: Int -> State (Cursor i) i
peekCursor 0 = state $ \cursor -> (head (input cursor), cursor)
peekCursor n = state $  \cursor -> ((input cursor) !! n, cursor)

advanceCursor :: State (Cursor i) i
advanceCursor = state $ 
    \cursor -> let cursor' = Cursor (tail (input cursor)) in 
                (head (input cursor), cursor') 

advanceCursor_ :: State (Cursor i) ()
advanceCursor_ = state $ 
    \cursor -> let cursor' = Cursor (tail (input cursor)) in 
                ((), cursor') 


isCursorEmpty :: State (Cursor i) Bool   
isCursorEmpty = state $ \cursor -> (length (input cursor) == 0, cursor)

-- tokenizer cursor, tokenization

type TokenizerCursor = Cursor Char

data Token = 
    TokenIdentifier String | 
    TokenNumber String |
    TokenOpenBracket |
    TokenCloseBracket |
    TokenLambda
    deriving (Eq)

instance Show Token where
    show (TokenIdentifier str) = "id-" ++ str
    show (TokenNumber str) = "n-" ++ str
    show (TokenOpenBracket) = "tok-("
    show (TokenCloseBracket) = "tok-)"
    show (TokenLambda) = "tok-lambda"

isWhitespace :: Char -> Bool
isWhitespace ch = elem ch [' ', '\n', '\t']

isDigit :: Char -> Bool
isDigit ch = elem ch ['0'..'9']

isLetter :: Char -> Bool
isLetter ch = elem ch ['a'..'z']

isGlyph :: Char -> Bool
isGlyph ch = not (isDigit ch || isLetter ch || isWhitespace ch)

takeNumber :: State TokenizerCursor Token
takeNumber = do 
    numstr <- takeCursorWhile isDigit
    return $ TokenNumber numstr

keywordMap = [("lambda", TokenLambda)]

takeIdentifier :: State TokenizerCursor Token
takeIdentifier = do
    let isIdentifier = (\c -> isLetter c || isDigit c)
    idstr <- takeCursorWhile isIdentifier

    let matching_keyword = filter (\(keyword_str, _) -> keyword_str == idstr) keywordMap

    if length matching_keyword == 0
        then return $ TokenIdentifier idstr 
        else return $ snd $ head matching_keyword 
takeWhitespace :: State TokenizerCursor ()
takeWhitespace = do 
    _ <- takeCursorWhile isWhitespace
    return ()



glyphMap = [("(", TokenOpenBracket),
            (")", TokenCloseBracket)]

strToGlyph :: String -> (String, Token) 
strToGlyph str =
    if length glyphs == 0 
    then (defaultGlyph, TokenIdentifier defaultGlyph)
    else (head glyphs)
    where
    glyphs =  filter (\(glyph, token) -> glyph == take (length glyph) str) glyphMap 
    defaultGlyph = takeWhile isGlyph str 

_takeGlyph :: TokenizerCursor -> (Token, TokenizerCursor)
_takeGlyph cursor = (token, cursor') where
    (glyphStr, token) = strToGlyph (input cursor)
    glyphLen = length glyphStr 
    cursor' = Cursor {
        input = drop glyphLen (input cursor)
    }

takeGlyph :: State TokenizerCursor Token
takeGlyph = state _takeGlyph


sTokenize :: State TokenizerCursor [Token]
sTokenize = do
    head <- peekCursor 0

    token <- if (isDigit head)
        then takeNumber
        else if (isGlyph head)
            then takeGlyph
            else takeIdentifier

    takeWhitespace
    empty <- isCursorEmpty

    rest <- if empty
                then (return [])
                else sTokenize

    return ([token] ++ rest)
    
tokenize :: String -> [Token]
tokenize input = fst $ runState sTokenize (Cursor input)


--PARSER

data AST = Atom Token | Expr [AST] deriving(Show) 
type ParserCursor = Cursor Token


collectExpr :: [AST] -> State ParserCursor [AST]
collectExpr asts = do
    peek_token <- peekCursor 0

    if peek_token == TokenCloseBracket
        then do
            advanceCursor_ --take up the CloseBracket
            return asts
        else do
            peek_ast <- sParse
            rest_ast <- collectExpr $ asts ++ [peek_ast]

            return rest_ast

parseExpr :: State ParserCursor AST
parseExpr =  do
    advanceCursor_ -- take up the branket 
    inner <- collectExpr []

    return $ Expr inner

parseAtom :: State ParserCursor AST
parseAtom = do
    token <- advanceCursor
    if token == TokenCloseBracket
        then error "unable to parse atom"
        else return $ Atom token 

sParse :: State ParserCursor AST
sParse = do
    head <- peekCursor 0
    
    value <- if head == TokenOpenBracket
        then parseExpr
        else parseAtom
    
    return value 

parse :: [Token] -> AST
parse tokens = fst $ runState sParse (Cursor tokens)


stringifyAST :: Int -> AST -> String
stringifyAST indent ast = 
 case ast of
    Atom (TokenIdentifier id) -> id
    Atom (TokenNumber n) -> n
    Atom (other @ _ ) -> (show other)

    Expr inner -> "\n" ++ indent_guide ++ "(" ++ (inner_str inner) ++ ")"
 where
    indent_guide = replicate (indent * 4) ' '
    inner_str = \inner -> unwords $ (stringifyAST (indent + 1) <$> inner)

--main = putStrLn $ (stringifyAST 0) $ parse $ tokenize "(\\ (* 5 6) (+ 2 22))"
main = print $ tokenize "(lambda (* 5 6) (+ 2 22))"
