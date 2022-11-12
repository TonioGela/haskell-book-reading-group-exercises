module Ch05.Polymorphism where

-- a -> a -> a
fn1 :: a -> a -> a
fn1 a b = a
fn1 a b = b

fn2 :: a -> b -> b
fn2 a b = b
-- the behavior do not change when the types of a or b change because the fn2 is parametric polymorphic
