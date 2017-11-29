{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module MoreLens where
import VanLaarhovenLenses (set)
import Control.Lens.TH
import Control.Applicative hiding (Const,getConst)
import Data.Coerce (coerce)
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

$(makeLenses ''Temp)

{-
centi_fn :: Float -> f Float

-}
centigrade :: Lens' Temp Float
centigrade centi_fn (T faren)
   = fmap (\centi' -> T (cToF centi')) (centi_fn (fToC faren))

cToF :: Float -> Float
cToF = undefined

fToC :: Float -> Float
fToC = undefined

type Traversal' s a = forall f . Applicative f => (a -> f a) -> (s -> f s)

view :: Monoid a => Traversal' s a -> s -> a
view ln s = getConst  $ ln Const s

newtype Const v a = Const v

getConst :: Const v a -> v
getConst (Const x) = x

instance Functor (Const v) where
  fmap f (Const x) = Const x         -- how does this typecheck?

instance Monoid m => Applicative (Const m) where
    pure _ = Const mempty
    (<*>) (Const (atob)) (Const a)  = Const $ mappend atob a--Const $ atob a



addr_string :: Traversal' Address String
addr_string elt_fn (A r c p)
  = pure (\r' c' -> A r' c' p) <*> (elt_fn r) <*> (elt_fn c)

