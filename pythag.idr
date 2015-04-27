-- Shamelessly stolen from Edwin Brady
module Main

pythag : Int -> List (Int, Int, Int)
pythag max = [(x, y, z) | z <- [1..max], y <- [1..z], x <- [1..y],
                          x * x + y *y == z * z]

-- If you use higer numbers here, you'll quite certainly
-- run into GAP's recursion depth trap.
main : IO ()
main = do
  print (pythag 10)
  putStrLn ""
