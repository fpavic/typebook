class Functor f => Applicative' f where
    pure' :: a -> f a
    (<<*>>) :: f (a -> b) -> f a -> f b

{-
The identity law:
pure id <*> v = v

Homomorphism:
pure f <*> pure x = pure $ f x
Collapsing multiple adjacent pure occurences into one.

Interchange:
u <*> pure y = pure ($ y) <*> u
Moving occurences of pure to the left.

Composition:
u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
f (b -> c) <*> (f (a -> b) <*> f a) = f (.) <*> f (b -> c) <*> f (a -> b) <*> f a
Reassociating <*>.
-}

{-
One might imagine a variant of the interchange law that says something about applying
a pure function to an effectful argument.
pure f <*> x = pure (flip ($)) <*> x <*> pure f

Proof:

(pure (flip ($)) <*> x) <*> pure f =
    pure ($ f) <*> (pure (flip ($)) <*> x) =
    pure (.) <*> pure ($ f) <*> pure (flip ($)) <*> x =
    pure ((.) ($ f)) <*> pure (flip ($)) <*> x =
    pure ((.) ($ f) (flip ($))) <*> x
    pure (($ f) . (flip ($))) <*> x
    pure f <*> x
-}

instance Applicative' Maybe where
    pure' x = Just x
    Just f <<*>> Just x = Just $ f x
    _ <<*>> _ = Nothing

{- 
Two views on lists.
  a) Ordered collection of elements
  b) Context representing multiple results of nondeterministic computation.
-}

newtype ZipList a = ZipList { getZipList :: [a]} deriving (Show, Eq)

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList $ fmap f xs

instance Applicative' ZipList where
    pure' g = ZipList $ repeat g

    ZipList gs <<*>> ZipList xs = ZipList $ zipWith ($) gs xs

instance Applicative' [] where
    pure' x = [x]
    gs <<*>> xs = [g x| g <- gs, x <- xs]

sequenceAL :: Applicative' f => [f a] -> f [a]
sequenceAL = foldr (\ x -> (<<*>>) (pure' (:) <<*>> x)) (pure' [])

-- sequenceAL [] = pure' []
-- sequenceAL (x:xs) = pure' (:) <<*>> x <<*>> sequenceAL  xs

class Functor f => Monoidal f where
    unit' :: f a 
    (**) :: f a -> f b -> f (a, b)

{-
Monoidal laws:

left identity:
unit ** v = v

right identity
u ** unit = u

associativity
u ** (v ** w) = (u ** v) ** w

pure a = fmap (const a) unit
gs <*> xs = fmap (uncurry $) (gs ** xs)

unit = pure ()
fx ** f y = pure (,) <*> fx <*> f y 
-}