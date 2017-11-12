module Optics (Lens, Prism) where

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

