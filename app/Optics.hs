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

