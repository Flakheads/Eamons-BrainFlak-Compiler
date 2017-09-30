import Expression

unheight = Height {heights = 0, difference = 0}
unit = Expression {
 rawvalue = 1,
 onStack = [],
 offStack = [],
 sstack = [],
 lheight = (unheight,unheight),
 rheight = (unheight,unheight)
}

singleHeight :: Integer -> Integer -> Bool -> Expression
singleHeight diff val False = Expression {
 rawvalue = val,
 onStack = [],
 offStack = [],
 sstack = [],
 lheight = (Height {heights = 1, difference = diff},unheight),
 rheight = (unheight,unheight)
}
singleHeight diff val True = Expression {
 rawvalue = val,
 onStack = [],
 offStack = [],
 sstack = [],
 lheight = (unheight,unheight),
 rheight = (Height {heights = 1, difference = diff},unheight)
}

empty = Expression {
 rawvalue = 0,
 onStack = [],
 offStack = [],
 sstack = [],
 lheight = (unheight,unheight),
 rheight = (unheight,unheight)
}

newScope sdepth = Expression {
 rawvalue = 0,
 onStack = [],
 offStack = [],
 sstack = ([1..sdepth]>>[0]++[1]),
 lheight = (unheight,unheight),
 rheight = (unheight,unheight)
}

semiAlgebrize :: String -> ([Expression], [Expression], [Expression]) -> (Integer, Integer, Integer) -> Bool -> (([Expression], [Expression], [Expression]), (Integer, Integer, Integer), Bool)
semiAlgebrize [] a b s = (a, b, s)

semiAlgebrize x@('{':'}':_) ([], offStack, sstack) (onDepth, offDepth, sdepth) s = semiAlgebrize x ([Expression {
 rawvalue = 0,
 onStack = [1..onDepth]>>[0]++[1],
 offStack = [],
 sstack = [],
 lheight = (unheight,unheight),
 rheight = (unheight,unheight)
}], offStack, sstack) (onDepth+1, offDepth, sdepth) s

semiAlgebrize x@(a:b:_) (onStack, offStack, []) (onDepth, offDepth, sdepth) s
 | elem (a:b:[]) ["()","[]","{}"] || a == '>' = semiAlgebrize x (onStack, offStack, [newScope sdepth]) (onDepth, offDepth, sdepth + 1) s

semiAlgebrize x@(a:_) (onStack, offStack, []) (onDepth, offDepth, sdepth) s
 | elem a ")]" = semiAlgebrize x (onStack, offStack, [newScope sdepth, newScope sdepth]) (onDepth, offDepth, sdepth + 2) s

semiAlgebrize x@(a:_) (onStack, offStack, [top]) (onDepth, offDepth, sdepth) s
 | elem a ")]" = semiAlgebrize x (onStack, offStack, [top, newScope sdepth]) (onDepth, offDepth, sdepth + 1) s

semiAlgebrize ('(':')':x) (onStack, offStack, (top:rest)) depths s = semiAlgebrize x (onStack, offStack, (plus unit top:rest)) depths s
semiAlgebrize ('[':']':x) (onStack, offStack, (top:rest)) depths@(onDepth, _, _) s = semiAlgebrize x (onStack, offStack, (plus (singleHeight onDepth (toInteger $ length onStack) s) top:rest)) depths s
semiAlgebrize ('{':'}':x) ((poppand:onStack), offStack, (top:rest)) depths s = semiAlgebrize x (onStack, offStack, (plus poppand top):rest) depths s

semiAlgebrize ('<':'>':x) (onStack, offStack, sstack) depths s = semiAlgebrize x (offStack, onStack, sstack) depths (not s)

semiAlgebrize ('(':x) (onStack, offStack, scope) depths s = semiAlgebrize x (onStack, offStack, empty:scope) depths s
semiAlgebrize ('[':x) (onStack, offStack, scope) depths s = semiAlgebrize x (onStack, offStack, empty:scope) depths s
semiAlgebrize ('<':x) (onStack, offStack, scope) depths s = semiAlgebrize x (onStack, offStack, empty:scope) depths s

semiAlgebrize (')':x) (onStack, offStack, (top:next:scope)) depths s = semiAlgebrize x (top:onStack, offStack, (plus top next):scope) depths s
semiAlgebrize ('>':x) (onStack, offStack, (top:scope)) depths s = semiAlgebrize x (onStack, offStack, scope) depths s
semiAlgebrize (']':x) (onStack, offStack, (top:next:scope)) depths s = semiAlgebrize x (onStack, offStack, (plus (neg top) next):scope) depths s

semiAlgebrize x a b c = error$ show(x, a, b, c)
