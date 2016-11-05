{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Anagrams where

import           Course.Applicative
import           Course.Core
import           Course.FileIO
import           Course.Functor
import           Course.List
import           Course.Monad
import qualified Data.List          as L

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
-- anagrams "acost" "words.txt"
anagrams :: Chars -> Filename -> IO (List Chars)
anagrams input file = do
  fileText <- readLatin file
  let fileWords = filter ((== length input) . length) $ lines (listh fileText)
  let base = toLower <$> sort input
  let anas = filter (\w -> sort w `equalIgnoringCase` base) fileWords
  return anas

sort :: Chars -> Chars
sort = listh . L.sort . hlist

-- Compare two strings for equality, ignoring case
equalIgnoringCase :: Chars -> Chars -> Bool
equalIgnoringCase = on (==) (toLower <$>)
