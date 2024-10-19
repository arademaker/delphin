namespace InsertionSort

def insert [Ord α] (x : α) : List α → List α
  | [] => [x]
  | y :: ys => if Ord.compare x y == .lt || Ord.compare x y == .eq 
                then x :: y :: ys 
                else y :: insert x ys

def insertionSort [Ord α] : List α → List α
  | [] => []
  | x :: xs => insert x (insertionSort xs)

end InsertionSort
