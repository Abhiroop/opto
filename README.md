# opto

How to build:

```
stack build
```

Example usage:

```
import VanLaarhovenLenses

data Person = P {name :: String, salary :: Int}

nameL :: Lens' Person String
nameL elt_fn (P n s)
  = fmap (\n' -> P n' s) (elt_fn n)

let p = P {name = "Fred", salary = 500}


> view nameL p
> "Fred"
```
Some more examples of Profunctor Optics in action at `Examples.hs`
