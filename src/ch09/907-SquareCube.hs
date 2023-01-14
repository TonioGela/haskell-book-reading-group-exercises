module Ch09.SquareCube where

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

t = [(x, y) | x <- mySqr, y <- myCube]
t' = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
t'' = length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]