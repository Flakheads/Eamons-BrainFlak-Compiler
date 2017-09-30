module Expression (Expression(..), Height(..), plus, neg) where

-- Heights represent a way of fetching height values --
{-
heights is the number of heights taken from the particular stack,
difference represents the value
-}
data Height = Height {
 heights :: Integer,
 difference :: Integer
} deriving (Show)

-- Expressions are values used by the algebrizer to generate algebraic code --
data Expression = Expression {
 rawvalue :: Integer,
 onStack :: [Integer],
 offStack :: [Integer],
 sstack :: [Integer],
 rheight :: (Height, Height),
 lheight :: (Height, Height)
} deriving (Show)

plus :: Expression -> Expression -> Expression
plus a b = Expression {
  rawvalue = rawvalue a + rawvalue b,
  onStack = stackAdd (onStack a) (onStack b),
  offStack = stackAdd (offStack a) (offStack b),
  sstack = stackAdd (sstack a) (sstack b),
  rheight = tupAdd (rheight a) (rheight b),
  lheight = tupAdd (lheight a) (lheight b)
 }

neg :: Expression -> Expression
neg a = Expression {
 rawvalue = -(rawvalue a),
 onStack = map (\x->(-x)) $ onStack a,
 offStack = map (\x->(-x)) $ offStack a,
 sstack = map (\x->(-x)) $ sstack a,
 rheight = (\(a,b)->(b,a)) $ rheight a,
 lheight = (\(a,b)->(b,a)) $ lheight a
}

stackAdd :: (Num a) => [a] -> [a] -> [a]
stackAdd [] x = x
stackAdd x [] = x
stackAdd (a:ax) (b:bx) = (a + b) : stackAdd ax bx

tupAdd :: (Height,Height) -> (Height,Height) -> (Height,Height)
tupAdd (a,b) (c,d) = (
 Height {
  heights = (heights a + heights b),
  difference = (difference a + difference b)
 },
 Height {
  heights = (heights c + heights d),
  difference = (difference c + difference d)
 })
