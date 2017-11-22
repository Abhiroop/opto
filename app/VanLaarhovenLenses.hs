{-# LANGUAGE RankNTypes #-}
module VanLaarhovenLenses where

{-
Lens' s a
s :: type of the container
a :: type of the focus

Eg : Lens DateTime Mins
     Lens DateTime Hours

Lenses compose:

composeL :: Lens' s1 s2
         -> Lens' s2 a
         -> Lens' s1 a

-}

data Person = P { name   :: String
                , addr   :: Address
                , salary :: Int
                }

data Address = A { road :: String
                 , city :: String
                 , postcode :: String}

setName :: String -> Person -> Person
setName n P { name = n'
            , addr = a
            , salary = s}
  = P { name = n
      , addr = a
      , salary = s}

setPostcode :: String -> Person -> Person
setPostcode pc P { name = n
                 , addr = A { road = r
                            , city = c
                            , postcode = p}
                 , salary = s}
  = P { name = n
      , addr = A { road = r
                 , city = c
                 , postcode = pc}
      , salary = s}

setName' :: String -> Person -> Person
setName' n p = p { name = n }

setPostcode' :: String -> Person -> Person
setPostcode' pc p
  = p { addr = (addr p) { postcode = pc } }

data LensR s a
  = L { viewR :: s -> a
      , setR  :: a -> s -> s
      , mod   :: (a -> a) -> s -> s
      , modF  :: forall f . Functor f => (a -> f a) -> s -> f s}

type Lens' s a = forall f . Functor f => (a -> f a) -> s -> f s

-- Lens' s a  ~ LensR s a

-- Using a Lens to set
{-
ln :: Lens' s a
x  :: a
s  :: s
We want this:

set :: forall s a . Lens' s a -> a -> s -> s

We need s

but we have "f s"

so f = identity
-}

newtype Identity a = Identity a

runIdentity :: Identity a -> a
runIdentity (Identity x) = x

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)


set :: forall s a . Lens' s a -> a -> s -> s
set ln x s = runIdentity $ ln f s
  where f :: a -> Identity a
        f = Identity

over' :: forall s a . Lens' s a -> (a -> a) -> s -> s
over' ln f s = runIdentity $ ln foo s
  where foo :: a -> Identity a
        foo = undefined

{-
Now f :: a -> a
 Identity :: a -> Identity a

Identity . f :: a -> Identity a

and foo requires that same type
so we can replace foo with Identity
Hence,
-}

over :: forall s a . Lens' s a -> (a -> a) -> s -> s
over ln f s = runIdentity $ ln (Identity . f) s

-- over is much more efficient. CRUX

newtype Const v a = Const v

getConst :: Const v a -> v
getConst (Const x) = x

instance Functor (Const v) where
  fmap f (Const x) = Const x         -- how does this typecheck?

{-
Here the functor f is Const a

Remember
type Lens' s a = forall f . Functor f => (a -> f a) -> s -> f s

Now for view
(a -> f a) becomes a -> Const a a
s is s

so when v = a
:t Const :: a -> Const a a
            a ->     f   a

ln Const s :: f s ~ Const a s


and now getConst :: Const a a -> a

getConst $ ln f s :: discards s and gives a 
-}

view :: forall s a . Lens' s a -> s -> a
view ln s = getConst  $ ln Const s
