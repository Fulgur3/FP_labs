type Position = (Int, Int)
type Board = [[Int]]

-- Розмір шахівниці
boardSize :: Int
boardSize = 8

-- Початкова позиція коня
startPosition :: Position
startPosition = (0, 0)

-- Перевірка, чи координати знаходяться на шахівниці
isValid :: Position -> Bool
isValid (x, y) = x >= 0 && y >= 0 && x < boardSize && y < boardSize

-- Перевірка, чи клітина не відвідана
isNotVisited :: Board -> Position -> Bool
isNotVisited board (x, y) = board !! x !! y == 0

-- Знайти можливі ходи коня з поточної позиції
possibleMoves :: Position -> [Position]
possibleMoves (x, y) =
  filter (\(a, b) -> isValid (a, b) && isNotVisited board (a, b)) moves
  where
    moves = [ (x + 2, y + 1), (x + 1, y + 2), (x - 1, y + 2), (x - 2, y + 1)
            , (x - 2, y - 1), (x - 1, y - 2), (x + 1, y - 2), (x + 2, y - 1)
            ]

-- Рекурсивна функція для пошуку шляху коня
knightTour :: Board -> Position -> Int -> Maybe Board
knightTour board pos@(x, y) count
  | count == boardSize * boardSize = Just board
  | otherwise =
    case filter (\move -> knightTour (updateBoard board move count) move (count + 1) /= Nothing) (possibleMoves pos) of
      [] -> Nothing
      (nextMove:_) -> knightTour (updateBoard board nextMove count) nextMove (count + 1)

-- Оновлення шахівниці після ходу коня
updateBoard :: Board -> Position -> Int -> Board
updateBoard board (x, y) count =
  take x board ++ [take y (board !! x) ++ [count] ++ drop (y + 1) (board !! x)] ++ drop (x + 1) board

-- Функція для ініціалізації та виклику пошуку шляху коня
findKnightTour :: Maybe Board
findKnightTour = knightTour (replicate boardSize (replicate boardSize 0)) startPosition 1

-- Вивід результату
main :: IO ()
main =
  case findKnightTour of
    Just solution -> mapM_ print solution
    Nothing -> putStrLn "Розв'язок не знайдено."