{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module MoreLens where
import VanLaarhovenLenses (set)
import Control.Lens.TH
data Person = P { _name   :: String
                , _salary :: Int
                , _addr   :: Address}

data Address = A { _road :: String
                 , _city :: String
                 , _postcode :: String}

type Lens' s a = forall f . Functor f => (a -> f a) -> s -> f s

{-
elt_fn :: String -> f String
P n s :: Person
-}

$(makeLenses ''Person)
$(makeLenses ''Address)

-- name :: Lens' Person String
-- name elt_fn (P n s a) = fmap (\n' -> P n' s a) (elt_fn n) 

setPostcode :: String -> Person -> Person
setPostcode pc p = set (addr.postcode) pc p

data Temp = T { _fahrenheit :: Float}
