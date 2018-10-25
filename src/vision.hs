a = [0.0, 0.5]
f a = map (/2) ( zipWith((+))a $ (0.0) : a ) ++ []
