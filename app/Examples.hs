{-# LANGUAGE RankNTypes #-}
module Examples where

import Optics

------------ LENS EXAMPLES---------------------------
-- view :: given (a,c) focuses on a
-- update :: given (b , (a,c)) updates to (b,c)
pi1 :: Lens a b (a,c) (b,c)
pi1 = Lens view update where
  view  (x,y)        = x
  update (x', (x,y)) = (x',y)

sign :: Lens Bool Bool Integer Integer
sign = Lens view update where
  view x = (x >= 0)
  update (b,x) = if b then abs x else -(abs x)
------------------------------------------------------


----------------PRISM EXAMPLES-----------------------

the :: Prism a b (Maybe a) (Maybe b)
the = Prism match build where
  match (Just x) = Right x
  match Nothing  = Left Nothing
  build x        = Just x

whole :: Prism Integer Integer Double Double
whole = Prism match build where
  match x
   | f == 0    = Right n
   | otherwise = Left x
   where (n,f) = properFraction x
  build = fromIntegral

-----------------------------------------------------

------------Bad Composition------------------
pi11 :: Lens a b ( ( a, c), d) ( ( b, c), d)
pi11 = Lens view update where
  Lens v u         =  pi1
  view             =  v . v
  update (x',xyz)  =  u (xy', xyz) where
                             xy   = v xyz
                             xy'  = u (x',xy)
---------------------------------------------

flatten :: Adapter ( a, b, c) ( a', b', c') ( ( a, b), c) ( ( a', b'), c')
flatten = Adapter from to where
  from ((x,y),z)  = (x,y,z)
  to (x,y,z)      = ((x,y),z)

------------Profunctor optics example--------------------

-- Example from http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf

prismC2P :: Prism a b s t -> PrismP a b s t
prismC2P (Prism m b) = dimap m (either id b) . right

lensC2P :: Lens a b s t -> LensP a b s t
lensC2P (Lens v u) = dimap (fork v id) u . first where
  fork f g x = (f x, g x)

data Tree a = Empty | Node (Tree a) a (Tree a)

type Number = String
type ID = String
type Name = String
data Contact = Phone Number | Skype ID
data Entry = Entry Name Contact
type Book = Tree Entry

phone :: PrismP Number Number Contact Contact
phone = prismC2P (Prism m Phone) where
  m (Phone n) = Right n
  m(Skype s) =Left(Skype s)

contact :: LensP Contact Contact Entry Entry
contact = lensC2P (Lens v u) where
  v (Entry n c) = c
  u (c',Entry n c) = Entry n c'
