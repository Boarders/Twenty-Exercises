{-# LANGUAGE LambdaCase #-}
module TwentyExercises where

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry f = \case
    [] -> []
    x:xs -> f x : furry f xs

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f = \case
    Nothing -> Nothing
    Just x  -> Just $ f x

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f g = f . g

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f = \case
    EitherLeft (Right t) -> EitherLeft (Right t)
    EitherLeft (Left a ) -> EitherLeft (Left $ f a)

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f =  \case
    EitherRight (Left t)  -> EitherRight (Left t)
    EitherRight (Right a) -> EitherRight (Right $ f a)

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a

  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f)

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana f = mconcat . (furry f)
  unicorn x = [x]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana f = \case
    Nothing -> Nothing
    Just a  -> f a 
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana fatb ta = \t -> fatb (ta t) t
  unicorn = const


-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana faet = \case
    EitherLeft (Right t) -> EitherLeft (Right t)
    EitherLeft (Left a)  -> faet a
  unicorn = EitherLeft . Left

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana faet = \case
    EitherRight (Left t)  -> EitherRight (Left t)
    EitherRight (Right a) -> faet a
  unicorn = EitherRight . Right

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple ma mab
  = (\a -> ((\ab -> unicorn $ ab a) `banana` mab)) `banana` ma


-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy as f = case as of
  []   -> unicorn []
  a:as ->
    let
      mb  = f a
      mbs = moppy as f
    in
      (\b -> (\bs -> unicorn $ (b:bs)) `banana` mbs) `banana` mb
      
-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage = (flip moppy) id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 fabc ma mb = apple mb (furry' fabc ma)

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 fabcd ma mb mc = apple mc (banana2 fabcd ma mb)

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 fabcde ma mb mc md = apple md (banana3 fabcde ma mb mc)

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry f st  =
    let
      g = state st
    in
      State $ \s0 ->
          let
            (s1, a) = g s0
          in
            (s1, f a)

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana fasb sa =
    let
      ga = state sa
    in
      State $ \s0 ->
        let
          (s1, a) = ga s0
          gb = state $ fasb a
        in
          gb s1
  unicorn a = State $ \s -> (s, a)
