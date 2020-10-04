module Parser (
    Parser 
  , parse
  , get, pfail
  , (<|>), some, many
  , (>>=:), (<+>), (<:>), (<++>), (<=>), (<-+>), (<+->)
  , getCharThat, digit, digits, letter, letters, space, spaces
  , skipws, number, sym
) where

import Control.Monad
import Control.Applicative
import Data.Char


------------------------------------------------------------------------------------------

-- | A Parser is a function from a string (input) to a result (a Maybe "state
--   pair). If successful the value contains the parse result and the remainder input to
--   parse. A parser is parameterized by its result type.
newtype Parser a = ParsingFunction (String -> Maybe (a, String))

-- | Runs a parser on an input and returns true if and only if the parser succeeds on the
--   entire string
parse :: Parser a -> String -> a
parse (ParsingFunction p) input = 
  case p input of 
    Just (result, "") -> result
    Just (_, remainder) -> error ("Unexpected input: " ++ remainder)
    Nothing -> error "Parse error"

------------------------------------------------------------------------------------------

instance Functor Parser where 
    fmap f p = p >>=: f

instance Applicative Parser where
    pure = return
    p1 <*> p2 = do f <- p1
                   result <- p2
                   return (f result)

instance Monad Parser where
    -- | A parser that always succeeds without consuming input 
    return value = ParsingFunction (\s -> Just (value, s))

    -- | The "bind" operator: transforms a parser using a function that uses 
    --   the parse result to create a new parser.
    (ParsingFunction p) >>= f = ParsingFunction bindFunc
      where bindFunc s = 
              case p s of
                Nothing -> Nothing
                Just (result, s') -> let (ParsingFunction p') = f result in p' s'

instance Alternative Parser where
  empty = pfail

  -- | A parser combinator for alternatives
  (ParsingFunction p1) <|> (ParsingFunction p2) = ParsingFunction alternativeFunc 
    where alternativeFunc s = 
            case p1 s of
              Just (result, s') -> Just (result, s')
              Nothing -> p2 s

  -- | A parser combinator that parses zero or more instances of p
  many p = p <:> many p <|> return []
  
  -- | A parser combinator that parses one or more instances of p
  some p = p <:> many p

------------------------------------------------------------------------------------------

-- | A parser that gets the next character (or fails if there is no more input)
get :: Parser Char
get = ParsingFunction getFunc
  where getFunc "" = Nothing
        getFunc (c:cs) = Just (c, cs)

-- | A parser that always fails without consuming input
pfail :: Parser a
pfail = ParsingFunction pfailFunc
  where pfailFunc _ = Nothing

------------------------------------------------------------------------------------------

-- | Use a function to transform a parse result.
infixl 1 >>=:
(>>=:) :: Parser a -> (a -> b) -> Parser b
p >>=: f = p >>= return . f

-- | A parser combinator for sequencing two parsers
(<+>) :: Parser a -> Parser b -> Parser (a, b)
p1 <+> p2 = do result1 <- p1
               result2 <- p2
               return (result1, result2)

-- | A parser combinator for concatenating two parsers
(<:>) :: Parser a -> Parser [a] -> Parser [a]
p1 <:> p2 =  do result1 <- p1
                result2 <- p2
                return (result1 : result2)

-- | A parser combinator for concatenating two parsers
(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
p1 <++> p2 =  do result1 <- p1
                 result2 <- p2
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
p1 <+-> p2 =  do result1 <- p1
                 _ <- p2
                 return result1
    
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
