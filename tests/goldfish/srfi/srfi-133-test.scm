(check (vector-cumulate + 0 #()) => #())
(check (vector-cumulate + 0 #(1 2 3)) => #(1 3 6))
(check (vector-cumulate - 0 #(1 2 3)) => #(-1 -3 -6))
