module Optics (Lens) where

-- | A Lens type which takes compound data structure of type S and focusses onto a small component A. Given a new component of type B updates the old data structure of type B to return a new structure of type T.


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


data Prism a b s t = Prism { match :: s -> Either t a
                           , build :: b -> t
                           }
