{- 
Module    : Solver
Description: Algorytm rozwiązujący zagadkę 

  Moduł zawiera funkcję do rozwiązywania zagadki 
  opartej na metodzie przeszukiwania z nawrotami.
-}

module Solver

where

import Pyramid
import Array


-- | Rozwiązanie zagadki piramid.
-- Funkcja rozwiązuje zagadkę piramid za pomocą algorytmu przeszukiwania z nawrotami. 
--  W kolejnych iteracjach rozbudowywane jest częsciowe rozwiązanie do momentu osiągnięcia pełnej planszy.  
--  Przy tworzeniu kolejnych rozwiązań częściowych sprawdzana jest ich poprawność pod względem reguł gry.  
--  Jeśli rozwiązanie częściowe jest niepoprawne, jest ono odrzucane i testujemy kolejną możliwość.
--  Przeszukiwanie zaczyna się w polu (1, 1) tablicy.
solve ::
      Piramids -> -- ^ Piramidarray ze wskazówkami
      Int -> -- ^ Rozmiar tablicy zagadki
      Array -> -- ^ Tablica zagadki
      (Int, Int) -> -- ^ Indeks tablicy
      IO (Maybe Array)
solve pyramid n array (col, row)
  | col == n + 1 =
    do piramidsFailed <- check_violations pyramid n array (col - 1, row)
       if (piramidsFailed) then return Nothing else solve pyramid n array (1, row + 1)
  | row == n + 1 = pretty_print n array >> return (Just array)
  | otherwise =
    do if (row == n && col > 1) then
         do piramidsFailed <- check_violations pyramid n array (col - 1, row)
            if (piramidsFailed) then return Nothing else solution
         else solution
  where 
        solution = 
          do 
            v <- read_array array (col, row) n
            case v of
              0 -> count_avail_values pyramid n array (col, row) >>=
                          backtrack pyramid n array (col, row)
              _ -> solve pyramid n array (col + 1, row)
        backtrack pyramid n array (col, row) [] = return Nothing
        backtrack pyramid n array (col, row) (v : vs)
          = do write_array array (col, row) n v
               r <- solve pyramid n array (col + 1, row)
               if (r == Nothing) then
                 do write_array array (col, row) n 0
                    backtrack pyramid n array (col, row) vs
                 else return r

