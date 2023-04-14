-- a = (+1) $ (read "[1]" :: [Int])
a = fmap (+1) $ (read "[1]" :: [Int])

-- b = (++ "lol") (Just ["Hi,", "Hello"])
b = (fmap.fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- c = (*2) (\x -> x - 2)
c = (*2) . (\x -> x - 2)

-- d =
--    ((return '1' ++) . show)
--    (\x -> [x, 1..3])
d = (return '1' ++) . (show . (\x -> [x, 1..3]))

-- e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123" ++ ) show ioi
--     in (*3) changed
