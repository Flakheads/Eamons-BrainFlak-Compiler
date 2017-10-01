import Expression
-- Temporary --
import Algebrizer
---------------

alphabet :: String
alphabet = ['a'..'z']

base26 :: (Integral a) => a -> String
base26 x
 | x < 26 = (alphabet !! (fromIntegral x)):[]
 | otherwise = (alphabet !! (fromIntegral (mod x 26))):(base26$ (quot x 26)-1)

makeArguments :: (Integral a) => a -> String -> String
makeArguments x name = "("++concat[name ++ '_':base26 a++":"|a<-[0..x-1]]++"__" ++ name ++ ")" 

destack :: [Integer] -> String -> String
destack polynomial name = concat [show coefficient ++ "*" ++ name ++ "_" ++ base26 index ++ "+" |(index,coefficient) <- zip [0..] polynomial,coefficient /= 0]

dealgebrize :: (([Expression], [Expression], [Expression]), (Integer, Integer, Integer), Bool) -> String -> String

dealgebrize ((left,right,third),(d1,d2,d3),True) name = name ++ " :: ([Integer],[Integer],[Integer]) -> (Integer,Integer) -> ([Integer],[Integer],[Integer])\n" ++ name ++ "(" ++ (makeArguments d1 "left") ++ "," ++ (makeArguments d2 "right") ++ "," ++ (makeArguments d3 "third") ++ ") (_lh,_rh) = (" ++ concat["(" ++ destack (onStack e) "left" ++ show (rawvalue e) ++ "):"|e<-left] ++ "__left,[]:__right,[]:__third)" ++ "\n"
