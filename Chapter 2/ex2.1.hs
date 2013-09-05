suffixes [] = [[]]
suffixes xss@(x:xs) = xss : suffixes xs

-- suffixes xs = xs : suffixes (tail xs)
-- suffixes = tails
