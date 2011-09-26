
# Data.Rated

Rated data types. Handy for measuring the "quality" of arbitrary types. A bit
more useful than a tuple because it includes these handy type class instances:

* Applicative
* Alternative
* Functor
* Monad
* Ord
* Eq

## Constructors

```haskell

data Rated a = Rate Int a
             | Junk
             deriving (Show)

```

## Examples

```haskell

nice :: Rated String
nice = Rate 5 "an exquisite string"

ugly :: Rated String
ugly = Rate 1 "an atrocious string"

junk :: Rated String
junk = Junk

best :: Rated String
best = ugly <|> junk <|> nice
-- > Rate 5 "an exquisite string"

upperNice :: Rated String
upperNice = fmap (map toUpper) nice
-- > Rate 5 "AN EXQUISITE STRING"

betterNice :: Rated String
betterNice = changeRating (+5) nice
-- > Rate 10 "an exquisite string"

```

The `Applicative` and `Monad` instances are pretty interesting. They allow you
to rate individual parts of a type and return a single rated type with the
individual ratings summed.

For example:

```haskell

data Position = Pos Float Float Float
              deriving (Show)

ratedPosition :: Rated Position
ratedPosition = do
  x <- Rate 2 10.0
  y <- Rate 2 20.0
  z <- Rate 1 30.4
  return $ Pos x y z
-- > Rate 5 (Pos 10.0 20.0 30.4)

invalidPosition = do
  x <- Rate 5 10.0
  y <- Junk
  z <- Rate 1 20.0
  return $ Pos x y z
-- > Junk

applPos = Pos <$> Rate 2 10.0
              <*> Rate 2 20.0
              <*> Rate 1 30.4
- > Rate 5 (Pos 10.0 20.0 30.4)

```

Nothing too fancy
