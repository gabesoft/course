{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Optional where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import Data.Monoid
import qualified Prelude as P

--  class Optional<A> {
--    Optional(A a) {} // Full
--    Optional() {} // Empty
--  }
data Optional a
    = Full a
    | Empty
    deriving (Eq,Show)

mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional _ Empty = Empty
mapOptional f (Full a) = Full (f a)

bindOptional :: (a -> Optional b) -> Optional a -> Optional b
bindOptional _ Empty = Empty
bindOptional f (Full a) = f a

fromFull :: Optional a -> a
fromFull (Full a) = a
fromFull _ = error "Optional#fromFull: full value expected"

hasValue :: Optional a -> Bool
hasValue Empty = False
hasValue _ = True

(??) :: Optional a -> a -> a
Empty ?? d = d
Full a ?? _ = a

(<+>) :: Optional a -> Optional a -> Optional a
Empty <+> o = o
k <+> _ = k

applyOptional :: Optional (a -> b) -> Optional a -> Optional b
applyOptional f a =
    bindOptional
        (\f' ->
              mapOptional
                  (\a' ->
                        f' a')
                  a)
        f

twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f = applyOptional . mapOptional f

contains
    :: Eq a
    => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

instance P.Functor Optional where
    fmap = M.liftM

instance Monoid a => Monoid (Optional a) where
  mempty = Empty
  mappend (Full a) (Full b) = Full (a <> b)
  mappend (Full a) _ = Full a
  mappend _ (Full a) = Full a
  mappend _ _ = Empty

instance A.Applicative Optional where
    (<*>) = M.ap
    pure = Full

instance P.Monad Optional where
    (>>=) = flip bindOptional
    return = Full