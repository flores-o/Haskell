data NestedList a = Elem a | List [NestedList a]

--flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
flattenList :: NestedList a -> [a]
flattenList (List []) = []
flattenList (Elem a)  = [a]
flattenList (List (x:xs)) = flattenList x ++ flattenList (List xs)