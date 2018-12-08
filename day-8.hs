-- https://adventofcode.com/2018/day/8 in Haskell
-- (rolling our own parser combinators, à la http://dev.stephendiehl.com/fun/002_parsers.html)
-- Try it here: https://tio.run/#haskell

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad

newtype Parser t a = Parser { parse :: [t] -> [(a, [t])] } deriving (Functor)

instance Applicative (Parser t) where
  pure a = Parser (\s -> [(a, s)])
  (<*>)  = ap

instance Monad (Parser t) where
  p >>= f = Parser (\s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s)

item :: Parser t t
item = Parser $ \case []     -> []
                      (t:ts) -> [(t,ts)]

runParser :: Parser t a -> [t] -> a
runParser m s =
  case parse m s of
    [(a, [])]   -> a
    [(_, rest)] -> error "Parser did not consume entire stream."
    _           -> error "Couldn't parse."

-----------------------------------------------------------------------

data Node =
  Node { children :: [Node]
       , metadata :: [Int] }

parseNode :: Parser Int Node
parseNode = do
  c <- item
  m <- item
  children <- replicateM c parseNode
  metadata <- replicateM m item
  pure $ Node children metadata

sumMetadata :: Node -> Int
sumMetadata (Node c m) = sum m + sum (map sumMetadata c)

value :: Node -> Int
value (Node [] m) = sum m
value (Node cs m) = sum [value $ cs!!(i-1) | i <- m, i >= 1, i <= length cs]

main = do
  stream <- map read . words <$> getLine
  let node = runParser parseNode stream
  print (sumMetadata node)
  print (value node)
