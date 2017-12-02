{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module ExtendedFunctorFamily where

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'

class Bifunctor p where
  bimap :: (a -> b) -> (x -> y) -> p a x -> p b y
-- bimap :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
-- p = (,)
-- a bifunctor maps every pair of objects, one from category C, and one from category D, to an object in category E.

class Monoid m where
  mappend :: m -> m -> m
  mempty  :: m

-- mu :: (m,m) -> m
-- (m,m) ~ (m x m)

class Profunctor p => Monoidal p where
  par    :: p a b -> p c d -> p (a,c)  ( b, d)
-- par      (a -> b) -> (c -> d) -> ((a, c) -> (b,d))
-- p = (->), .....
  empty  :: p () ()

class Profunctor p => General p t where
 par1 :: p a b -> p c d -> p (t a c) (t b d)


instance General (->) (,) where
  par1 = bimap

instance (Monoidal p, Profunctor p) => General p (,) where
  par1 = par


instance Monoidal (->) where
  par f g (x,y) = (f x, g y)
  empty         = id


instance Profunctor (->) where
  dimap f g h = g . h . f


instance Bifunctor (,) where
  bimap f g (x,y) = (f x , g y)


