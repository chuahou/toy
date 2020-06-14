-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou
-- https://www.codewars.com/kata/5470c635304c127cad000f0d/train/haskell
--
-- Additional reference:
--  G. Jones, Functional Programming. Lecture Notes, 2019.
--  https://www.youtube.com/watch?v=N9RUqGYuGfw

module RegExpParser ( RegExp(..) , parseRegExp ) where

import Control.Monad ( join )
import Control.Applicative ( Alternative, empty, (<|>), many )
import Data.Tuple ( swap )

data RegExp = Normal Char       -- A character that is not in "()*|."
            | Any               -- Any character (.)
            | ZeroOrMore RegExp -- 0+ occurences of the same regexp (*)
            | Or RegExp RegExp  -- A choice between 2 regexps (|)
            | Str [RegExp]      -- A sequence of regexps
    deriving (Show, Eq)

----- Parser definitions -----

-- Parser type and its instances as a Monad
newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    -- parse and then apply f to the returned value
    fmap f (Parser p) = Parser (fmap f' . p)
        where f' (x, xs) = (f x, xs)

instance Applicative Parser where
    -- return y and consume no input
    pure y = Parser (\xs -> Just (y, xs))

    -- parse f, then parse p, then apply the return of f to the return of p
    Parser f <*> Parser p = Parser (join . fmap applyf . f)
        -- applyf takes the output of f's parse, then uses p's parse and applies
        -- f's parse to p's parse
        where applyf (f', ys) = fmap (swap . fmap f' . swap) . p $ ys

instance Monad Parser where
    -- Parse p, then pass new value to f to generate the next parser
    Parser p >>= f = Parser (join . fmap f' . p)
        where f' (y, ys) = parse (f y) ys

instance Alternative Parser where
    -- Fail parsing
    empty = Parser (\_ -> Nothing)

    -- Return p's parse, if fail, then return q's parse
    Parser p <|> Parser q = Parser (\xs -> p xs <|> q xs)

-- Parser that returns Nothing
nothing :: Parser a
nothing = Parser (\x -> Nothing)

-- Additional constraint on parser
(<?>) :: (a -> Bool) -> Parser a -> Parser a
pred <?> Parser p = Parser p >>= f
    where f y | pred y    = return y
              | otherwise = nothing

-- Match any single character
char :: Parser Char
char = Parser parse
    where parse (x:xs) = Just (x, xs)
          parse [] = Nothing

-- Match a specific character
mchar :: Char -> Parser Char
mchar c = (== c) <?> char

----- Regex Parsers -----

-- Special characters
specialChars :: [Char]
specialChars = "()*|."

-- Match any normal character (not in specialChars)
normal :: Parser RegExp
normal = (`notElem` specialChars) <?> char >>= return . Normal

-- Match an Any character (.)
anychar :: Parser RegExp
anychar = mchar '.' >> return Any

-- Match a sequence of RegExps
str :: Parser RegExp
str = many sequenceable >>= return . Str

-- Sequenceable parsers
sequenceable :: Parser RegExp
sequenceable = normal <|> anychar

-- Overall parser
regex :: Parser RegExp
regex = undefined

----- Codewars -----

parseRegExp :: String -> Maybe RegExp
parseRegExp = fmap fst . parse regex
