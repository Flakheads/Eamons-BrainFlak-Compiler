import Expression

alphabet :: String
alphabet = ['a'..'z']

base26 :: (Integral a) => a -> String
base26 x
 | x < 26 = (alphabet !! (fromIntegral x)):[]
 | otherwise = (alphabet !! (fromIntegral (mod x 26))):(base26$ (quot x 26)-1)


