

structure Point where
 x : Float
 y : Float
deriving Repr

structure Segment where
 s : Point
 e : Point
deriving Repr

def Segment.length (s : Segment) :=
  Float.sqrt $ (s.e.x - s.s.x) ^ 2 + (s.e.y - s.s.y) ^ 2

def a := Segment.mk (Point.mk 2 3) (Point.mk 3 4)

#eval a.length
