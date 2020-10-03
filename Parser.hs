module Parser where

import Prelude hiding (return, (>>=), (>>))
import Data.Char


------------------------------------------------------------------------------------------

-- | A Parser is a function from a string (input) to a result (a Maybe "state
--   pair). If successful the value contains the parse result and the remainder input to
--   parse. A parser is parameterized by its result type.
type Parser a = String -> Maybe (a, String)

-- | Runs a parser on an input and returns true if and only if the parser succeeds on the
--   entire string
parse :: Parser a -> String -> a
parse p input = 
  case p input of 
    Just (result, "") -> result
    Just (_, remainder) -> error ("Unexpected input: " ++ remainder)
    Nothing -> error "Parse error"

------------------------------------------------------------------------------------------

-- | A parser that gets the next character (or fails if there is no more input)
get :: Parser Char
get "" = Nothing
get (c:cs) = Just (c, cs)

-- | A parser that always succeeds without consuming input
return :: a -> Parser a
return result s = Just (result, s)

-- | A parser that always fails without consuming input
pfail :: Parser a
pfail _ = Nothing

-- | A parser combinator for alternatives
infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a
(p1 <|> p2) s = 
  case p1 s of
    Just (result, s') -> Just (result, s')
    Nothing -> p2 s
    
-- | The "bind" operator: transforms a parser using a function that uses the parse result
--   to create a new parser.
infixl 1 >>=
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
(p >>= f) s = 
  case p s of
    Nothing -> Nothing
    Just (result, s') -> f result s'

------------------------------------------------------------------------------------------

-- | Use a function to transform a parse result.
infixl 1 >>=:
(>>=:) :: Parser a -> (a -> b) -> Parser b
p >>=: f = p >>= return . f

-- | Parse, but ignore the result and produce a different parser
infixl 1 >>
(>>) :: Parser a -> Parser b -> Parser b
p1 >> p2 = p1 >>= \_ -> p2

-- | A parser combinator for sequencing two parsers
(<+>) :: Parser a -> Parser b -> Parser (a, b)
p1 <+> p2 = 
  p1 >>= \result1 ->
    p2 >>= \result2 ->
      return (result1, result2)

-- | A parser combinator for concatenating two parsers
(<:>) :: Parser a -> Parser [a] -> Parser [a]
p1 <:> p2 = 
  p1 >>= \result1 ->
    p2 >>= \result2 ->
      return (result1 : result2)

-- | A parser combinator for concatenating two parsers
(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
p1 <++> p2 = 
  p1 >>= \result1 ->
    p2 >>= \result2 ->
      return (result1 ++ result2)

-- | A parser that succeeds if the result of another parser p satisfies a predicate
(<=>) :: Parser a -> (a -> Bool) -> Parser a
p <=> cond = 
  p >>= \result -> if cond result then return result else pfail

-- | A parser combinator that discards the left result
(<-+>) :: Parser a -> Parser b -> Parser b
p1 <-+> p2 = p1 >> p2

-- | A parser combinator that discards the right result
(<+->) :: Parser a -> Parser b -> Parser a
p1 <+-> p2 = 
  p1 >>= \result1 ->
    p2 >> return result1
    
-- | A parser combinator that parses zero or more instances of p
many :: Parser a -> Parser [a]
many p = (p <:> many p) <|> return []

-- | A parser combinator that parses one or more instances of p
some :: Parser a -> Parser [a]
some p = p <:> many p

-- | Constructs a parser that matches a specific character
getCharThat :: (Char -> Bool) -> Parser Char
getCharThat cond = get <=> cond

-- | A parser that matches a digit
digit :: Parser Char
digit = getCharThat isDigit

-- | A parser that matches one or more digits
digits :: Parser String
digits = some digit

-- | A parser that matches a letter
letter :: Parser Char
letter = getCharThat isLetter

-- | A parser that matches one or more letters
letters :: Parser String
letters = some letter

-- | A parser that matches a space
space :: Parser Char
space = getCharThat isSpace

-- | A parser that matches one or more spaces
spaces :: Parser String
spaces = some space

-- | A parser combinator that skips leading whitespace and matches p
skipws :: Parser a -> Parser a
skipws p = many space <-+> p

-- | A parser that matches a number (ignoring leading whitespace)
number :: Parser String
number = skipws digits

-- | Constructs a parser that matches a given character (ignoring leading whitespace)
sym :: Char -> Parser Char
sym c = skipws (getCharThat (== c))
