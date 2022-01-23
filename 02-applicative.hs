class Functor f => Applicative' f where
    pure :: a -> f a
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