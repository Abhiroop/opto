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

