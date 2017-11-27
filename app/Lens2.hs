{-# LANGUAGE RankNTypes #-}
module Lens2 where

import VanLaarhovenLenses

-- type Lens' s a = forall f . Functor f => (a -> f a) -> s -> f s

_1 :: Lens' (a,b) a
_1 f (x,y) = fmap (\a -> (a,y)) (f x)

-- x = view _1 (1,2)        .. 1
-- x = set _1 3 (1,2)
-- x = over _1 (+ 5) (1,2)  .. (6.2)
