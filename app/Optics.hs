{-# LANGUAGE RankNTypes #-}
module Optics where

-- | A Lens type which takes compound data structure of type S and focusses onto a small component A. Given a new component of type B updates the old data structure of type B to return a new structure of type T.

--         view
--         ___
--        |   |
--        v   |
data Lens a b s t = Lens{ view   :: s     -> a
                        , update :: (b,s) -> t
                        }
--         b,s  t
--          |   ^
--          |   |
--           ---
--          update

-- | A Prism type which takes a compound data structure of type S and downcasts it to a possible type A and yields the original S if not possible. Updates the data structure by upcasting the type A to the compound type S

--         match :: if successful then a else Left error
--          ___
--         |   |
--         v   |
data Prism a b s t = Prism { match :: s -> Either t a
                           , build :: b -> t
                           }
--           b   t
--           |   ^
--           |   |
--            ---
--           build

-- | An Adapter is a specialization of Lens and Prism, where the component being viewed is the entire structure.
--            from
--            ___
--           |   |
--           v   |
data Adapter a b s t = Adapter { from :: s -> a
                               , to   :: b -> t
                               }
--             b   t
--             |   ^
--             |   |
--              ---
--              to

data Traversal a b s t = Traversal {traverse :: forall f . Applicative f =>
                                    s -> (a -> f b) -> f t}
--                                  ^    ----------
--                                  |        ^
--                                  |        |
--                                  |   function on 'a'
--                                  |
--                           container of 'a's

-- 1. Profunctors
class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'

-- 2. function arrows: a -> b  are instances of profunctors
instance Profunctor (->) where
  dimap f g h = g . h . f

-- 3. infact a -> f b are instance of profunctors
data UpStar f a b = UpStar {unUpStar :: a -> f b}

instance Functor f => Profunctor(UpStar f) where
  dimap f g (UpStar h) = UpStar ((fmap g) . h . f)
{-Explanation
f :: a' -> a
h :: a  -> f b
g :: b  -> b'

h . f :: a' -> f b

g :: b -> b'
fmap g :: f b -> f b'

So fmap g . h . f :: a' -> f b'
-}

------------------------------------------------------
-- 4. Introduce Cartesian and co cartesian profunctors
-- Cartesian works on product types whereas cocartesian works on sum types
class Profunctor p => Cartesian p where
  first  :: p a b -> p (a,c) (b,c)
  second :: p a b -> p (c,a) (c,b)
-- Cartesian is also known as Strong

instance Cartesian (->) where
  first  h (x,y) = (h x,y)
  second h (x,y) = (x,h y)
{-
h :: p a b
(x,y) :: (a, c)
-}

class Profunctor p => Cocartesian p where
  left  :: p a b -> p (Either a c) (Either b c)
  right :: p a b -> p (Either c a) (Either c b)
-- Cocartesian is also known as Choice

instance Cocartesian (->) where
  left  h (Left x)  = Left (h x)
  left  h (Right y) = Right y
  right h (Left x)  = Left x
  right h (Right y) = Right (h y)

class Profunctor p => Monoidal p where
  par    :: p a b -> p c d -> p ( a, c) ( b, d)
  empty  :: p () ()

instance Monoidal (->) where
  par f g (x,y) = (f x, g y)
  empty         = id

type Optic p a b s t = p a b -> p s t

type AdapterP a b s t = forall p . Profunctor p => Optic p a b s t

type LensP a b s t = forall p . Cartesian p => Optic p a b s t

type PrismP a b s t = forall p . Cocartesian p => Optic p a b s t

type TraversalP a b s t = forall p . (Cartesian p, Cocartesian p, Monoidal p) => Optic p a b s t

