{-# LANGUAGE RankNTypes #-}
module Lens2 where

--import VanLaarhovenLenses(Lens')
import MoreLens as M
import Data.Functor.Identity
import Data.Traversable
import Control.Applicative
-- type Lens' s a = forall f . Functor f => (a -> f a) -> s -> f s

_1 :: Lens' (a,b) a
_1 f (x,y) = fmap (\a -> (a,y)) (f x)

-- x = view _1 (1,2)        .. 1
-- x = set _1 3 (1,2)
-- x = over _1 (+ 5) (1,2)  .. (6.2)

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)

fmapDefault :: Traversable t => (a -> b) -> t a -> t b
fmapDefault f x = runIdentity $ traverse (Identity . f) x

data User = User String [Post] deriving Show
data Post = Post String deriving Show

posts :: Traversal' User [Post]
posts f (User n p) = fmap (\p' -> User n p') (f p)

title :: Traversal' Post String
title f (Post t) = fmap Post (f t)

users :: [User]
users = [User "john" [Post "hello", Post "world"], User "bob" [Post "foobar"]]
