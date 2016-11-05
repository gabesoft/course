{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FileIO where

import           Control.DeepSeq
import           Course.Applicative
import           Course.Core
import           Course.Functor
import           Course.List
import           Course.Monad
import           System.IO          hiding (readFile, FilePath)

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ...
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

To run from ./share:
share/ $ runhaskell -i../src ../src/Course/FileIO.hs files.txt
-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  error "todo: Course.FileIO#main"

readUtf8 :: Chars -> IO [Char]
readUtf8 file = withFile (hlist file) ReadMode $ \h ->
  do hSetEncoding h utf8
     contents <- hGetContents h
     return $!! contents

readLatin :: Chars -> IO [Char]
readLatin file = withFile (hlist file) ReadMode $ \h ->
  do hSetEncoding h latin1
     contents <- hGetContents h
     return $!! contents

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run =
  error "todo: Course.FileIO#run"

getFiles :: List FilePath -> IO (List (FilePath, Chars))
getFiles =
  error "todo: Course.FileIO#getFiles"

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile =
  error "todo: Course.FileIO#getFile"

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
  error "todo: Course.FileIO#printFiles"

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile =
  error "todo: Course.FileIO#printFile"

