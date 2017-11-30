{-# LANGUAGE AllowAmbiguousTypes #-}
module ExtendedFunctorFamily where

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Bifunctor p where
  bimap :: (a -> b) -> (x -> y) -> p a b -> p x y

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

class Profunctor f where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
