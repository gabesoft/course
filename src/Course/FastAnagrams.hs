{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import           Control.DeepSeq
import           Course.Applicative
import           Course.Core
import           Course.Functor
import           Course.List
import           Course.Monad
import qualified Data.Set           as S
import           System.IO          hiding (readFile)

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
-- on a Mac - run this with:
-- > fastAnagrams "Tony" "/usr/share/dict/words"
fastAnagrams :: Chars -> Filename -> IO (List Chars)
fastAnagrams name f = process . listh <$> readLatin f
  where
    process =
        flip (filter . flip S.member) (permutations name) .
        S.fromList . hlist . lines

readLatin :: Chars -> IO [Char]
readLatin file = withFile (hlist file) ReadMode $ \h ->
  do hSetEncoding h latin1
     contents <- hGetContents h
     return $!! contents

newtype NoCaseString = NoCaseString { ncString :: Chars }

instance Eq NoCaseString where
    (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
    show = show . ncString
