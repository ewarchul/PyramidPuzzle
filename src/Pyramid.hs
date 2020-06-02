{-# LANGUAGE FlexibleContexts #-}
{- 
Module    : Pyramid
Description: Model piramidy w zagadce. 

    Moduł zawiera funkcje, które pozwalają operować na tablicy zagadki w kontekście
    umieszczanych na nich piramidach:
      - zliczanie wysokości piramid w wierszu
      - sprawdzanie ograniczeń wynikających ze wskazówek podanych przez użytkownika
      - etc.
-}
module Pyramid

where

import Array
import Data.Array.IO
import Data.List


data Piramids = 
  Piramids [Maybe Int] [Maybe Int] [Maybe Int] [Maybe Int] deriving (Show, Read)

-- | Poprawność komórki
-- Funkcja sprawdza poprawność sąsiedztwa komórki (i, j).
check_violations ::
              (MArray a1 a2 m, Ord a2, Num a2) =>
               Piramids -> -- ^ Piramida
               Int -> -- ^ Rozmiar tablicy zagadki
               a1 Int a2 -> -- ^ Tablica zagadki
               (Int, Int) -> -- ^ Element tablicy zagadki (i, j)
               m Bool                                                                
check_violations (Piramids above below left right) n array (0,row) = 
  do 
    return False 
check_violations (Piramids above below left right) n array (col,row) = 
  do 
    r <- count_rows_bad array row n
    c <- count_cols_bad array col n
    let cAbove = above !! (col-1) 
        cBelow = below !! (col-1)
        cLeft  = left  !! (row-1)
        cRight = right !! (row-1) 
        fail = not ((check_row cAbove c) && (check_row cBelow (reverse c)) 
                     && (check_row cLeft r) && (check_row cRight (reverse r)))  
    return (fail)

-- | Możliwe wysokości.
-- Funkcja wylicza możliwe wysokości dla
-- podanego elementu tablicy (i, j).
count_avail_values ::
                    MArray a1 Int m =>
                    Piramids -> -- ^ Piramidarray
                    Int -> -- ^ Rozmiar tablicy zagadki
                    a1 Int Int -> -- ^ Tablica zagadki
                    (Int, Int) -> -- ^ Element talbicy
                    m [Int] -- ^ Lista dozwolonych wartości
count_avail_values pyramid n array (col,row) = do   
                                     r <- count_rows_bad array row n
                                     c <- count_cols_bad array col n
                                     h <- count_all_heights pyramid n (col,row)                                      
                                     let array = [0..n] \\ (r `union` c `union` h)
                                     return array

-- | Wysokości.
-- Funkcja zliczarray wysokości wokół indeksu (i, j)
-- i zwracarray je jako listę.
count_all_heights ::
                    Monad m =>
                    Piramids -> -- ^ Piramida
                    Int -> -- ^ Rozmiar tablicy
                    (Int, Int) -> -- ^ Indeks tablicy
                    m [Int]
count_all_heights (Piramids above below left right) n (col,row) =
  do 
    return (
            get_row_height (above !! (col-1)) n row ++
            get_row_height (below !! (col-1)) n (n-row+1) ++
            get_row_height (left !! (row-1)) n col ++
            get_row_height (right !! (row-1)) n (n-col+1)
            )

pretty_print dim array  =
  mapM_ (\col -> print_col dim array col) [1..dim]
  where
        print_col dim array col =
          putStr "[" >> 
          mapM (\row -> read_array array (row, col) dim) [1..dim] >>=
          mapM_ (putStr . show) >>
          putStrLn "]"

