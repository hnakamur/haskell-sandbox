{-# LANGUAGE FlexibleContexts #-}

module SandBox.Text.CSS.Parsec.Combinator
    ( countRange
    , countMax
    , countMin
    , manyUpTo
    ) where

import Text.Parsec ((<|>), Stream, ParsecT, many, try)

countRange :: Stream s m t =>
              Int -> Int -> ParsecT s u m a -> ParsecT s u m [a]
countRange min max p
    | min <= 0  = countMax max p
    | otherwise = try p >>= \x ->
                    (try (countRange (min-1) (max-1) p) >>= \xs ->
                      return (x:xs))

countMax :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
countMax max p
    | max <= 0  = return []
    | otherwise = (try p >>= \x ->
                    try (countMax (max-1) p) >>= \xs -> return (x:xs)
                  )
                  <|> return []

countMin :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
countMin min p
    | min <= 0  = many p
    | otherwise = try p >>= \x ->
                    (try (countMin (min-1) p) >>= \xs -> return (x:xs))

manyUpTo :: Stream s m t =>
            ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m [a]
manyUpTo p end = scan
  where
    scan =   do{ e <- end; return [e] }
         <|> do{ x <- p; xs <- scan; return (x:xs) }
