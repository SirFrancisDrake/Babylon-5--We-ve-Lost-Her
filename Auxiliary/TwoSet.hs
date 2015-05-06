
find :: (Eq a) => a -> [(a,a)] -> Bool
find x ls = not . null $
  filter (\(a,b) -> a == x || b == x) ls
