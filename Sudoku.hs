module Sudoku where

import Test.QuickCheck
import Data.Char (digitToInt, isDigit)
import GHC.RTS.Flags (ParFlags(parGcThreads))
import Test.QuickCheck.Property (Property(MkProperty))
------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row]
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku      = Sudoku $ replicate 9 $ replicate 9 Nothing

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = length (rs) == 9 &&
             all ((== 9) . length) rs &&
             all validCell (concat rs)
  where
    rs = rows s
    validCell Nothing = True
    validCell (Just n) = n >= 1 && n <= 9

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled s          = not $ or [ or [c == Nothing | c <- r] | r <- rows s]

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = putStr $ unlines [[show' c | c <- r] | r <- rows s]

show' :: Cell -> Char
show' Nothing       = '.'
show' (Just n)      = head (show n)

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do
  f <- readFile path
  let l = lines f
  let s = Sudoku $ map stringToRow l
  let s' | isSudoku s = s
         | otherwise = error "Program error: Not a Sudoku!"
  return s'

stringToRow :: String -> Row
stringToRow [] = []
stringToRow (x:xs) = charToCell x : stringToRow xs

charToCell :: Char -> Cell
charToCell c | c == '.'   = Nothing
             | isDigit c  = (Just (digitToInt c))
             | otherwise = Nothing
------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = frequency [ (9, return Nothing), (1, fmap Just (elements [1..9])) ]
-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
    s <- sequence [vectorOf 9 cell | _ <- [1..9]]
    return $ Sudoku s

-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Block

-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock []           = True
isOkayBlock (Nothing:cs) = isOkayBlock cs
isOkayBlock (c:cs)       = (length duplicates) == 0 && isOkayBlock cs
  where duplicates = filter (== c) cs

-- * D2

makeBlocks :: [[Cell]] -> [Block]
makeBlocks [r1, r2, r3] = map (concatMap f) $ chunks $ zip3 r1 r2 r3
  where f (c1, c2, c3) = [c1, c2, c3]

chunks :: [a] -> [[a]]
chunks [] = []
chunks xs = take 3 xs : chunks (drop 3 xs)

boxes :: Sudoku -> [Block]
boxes = concatMap makeBlocks . chunks . rows

columns :: Sudoku -> [Block]
columns s = [map (!! c) (rows s) | c <- [0..8]]

blocks :: Sudoku -> [Block]
blocks s = rows s ++ columns s ++ boxes s

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths s = length (b) == 27 && and [length (b !! x) == 9 | x <- [0..26]]
 where b = blocks s

-- * D3

isOkay :: Sudoku -> Bool
isOkay s = and [isOkayBlock (b !! x) | x <- [0..26]]
  where b = blocks s


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int, Int)

-- * E1
blanks :: Sudoku -> [Pos]
blanks (Sudoku rows) =
  map (\(_, pos) -> pos) $
  filter (\(b, _) -> b) $
  concat $
  mapWithIndex (\j row ->
  mapWithIndex (\i cell -> (cell == Nothing, (j, i))) row) rows

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0..]

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = blanks allBlankSudoku == [(x, y) | x <- [0..8], y <- [0..8]]


-- * E2
(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = take i xs ++ [y] ++ drop (i+1) xs

prop_bangBangEquals_correct :: [Cell] -> (Int, Cell) -> Bool
prop_bangBangEquals_correct xs (i, y)
  | i < 0 || i >= length xs || null xs = True
  | otherwise = length xs' == length xs && xs' !! i == y
  where
    xs' = xs !!= (i, y)

-- * E3
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku rows) (r, c) newValue =
  Sudoku (rows !!= (r, updatedRow))
  where
    updatedRow = (rows !! r) !!= (c, newValue)

prop_update_updated :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update_updated (Sudoku rows) (r,c) newValue
  | r < 0 || r >= 9 || c < 0 || c >= 9 = True
  | otherwise = updatedRows !! r !! c == newValue
    where Sudoku updatedRows = update (Sudoku rows) (r,c) newValue


------------------------------------------------------------------------------

-- * F1
solve :: Sudoku -> Maybe Sudoku
solve s
  | not (isSudoku s) = Nothing
  | otherwise = listToMaybe (solve' s (blanks s))


solve' :: Sudoku -> [Pos] -> [Sudoku]
solve' s _     | not (isOkay s) = []
solve' s []    = [s | isSudoku s && isOkay s]
solve' s (x:xs) = concatMap (\n -> solve' (update s x (Just n)) xs) [1..9]

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

-- * F2
printSolution :: Maybe Sudoku -> IO ()
printSolution Nothing = putStrLn "No solution found."
printSolution (Just sol) = printSudoku sol

readAndSolve :: FilePath -> IO ()
readAndSolve path = do
  sudoku <- readSudoku path
  let solved = solve sudoku
  printSolution solved

-- * F3
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf (Sudoku solved) (Sudoku original)
  | isFilled (Sudoku solved) =  all (\(r1, r2) -> all (\(c1, c2) -> c2 == Nothing || c1 == c2)
                                      (zip r1 r2)) (zip solved original)
                                && isOkayBlockAll (blocks (Sudoku solved))
                                
  | otherwise = False

isOkayBlockAll :: [Block] -> Bool
isOkayBlockAll = foldr ((&&) . isOkayBlock) True

-- * F4
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sudoku =
  case solve sudoku of
    Nothing -> property True
    Just solved -> property $ isSolutionOf solved sudoku