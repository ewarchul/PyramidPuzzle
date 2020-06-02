{- 
Module    : Array
Description: Model tablicy zagadki 

  Moduł zawiera model tablicy zagadki, na której umieszczane są piramidy. 
  Model ten oparty jest na mutowalnej tablicy MArray udostępnianej w ramach
  biblioteki Data.Array.IO.
-}

module Array

where

import Data.Array.IO as IOArray
import Data.List
import Control.Monad.State


type Array = 
  IOArray Int Int 

-- | Wrapper na funkcję readArray z biblioteki Data.Array.IO.
-- Funkcja pozwala na pobranie elementu z tablicy MArray 
-- indeksowaniu wierszy i kolumn od 1.
read_array :: 
          (MArray a e m, Ix i, Num i) =>
          a i e -> -- ^ Macierz 
          (i, i) -> -- ^ Indeks tablicy
          i -> -- ^ Rozmiar macierzy
          m e -- ^ Macierz 
read_array a (col,row) n = IOArray.readArray a (col+n*(row-1))


-- | Wrapper na funkcję writeArray z biblioteki Data.Array.IO.
-- Funkcja pozwala na wpisanie elementu do tablicy MArray przy
-- indeksowaniu wierszy i kolumn od 1.
write_array :: 
          (MArray a e m, Ix i, Num i) =>
          a i e -> -- ^ Macierz 
          (i, i) -> -- ^ Indeks tablicy
          i -> -- ^ Rozmiar macierzy
          e -> -- ^ Wpisywany element
          m () -- ^ Macierz 
write_array a (col,row) n v = IOArray.writeArray a (col+n*(row-1)) v

-- | Niepoprawne wysokości.
-- Funkcja zwraca listę wysokości naruszających wskazówki w wierszu
get_row_height :: 
                    (Eq a, Num a, Enum a) => 
                    Maybe a -> -- ^ Wartość wskazówki 
                    a -> -- ^ Rozmiar tablicy
                    a -> -- ^ numer wiersza
                    [a] -- ^ Lista wartości wyokości
get_row_height Nothing _ _ = []
get_row_height (Just 1) n index 
  | index == 1 = [1..n] \\ [n]
  | otherwise = [n]
get_row_height (Just constraint) n index 
  | constraint == n = ([1..n] \\ [index])
  | otherwise = [1..n] \\ [1..index+n-constraint]

-- | Niepoprawne indeksy wierszy.
-- Funkcja zwraca indeksy wierszy, które naruszają ograniczenia wynikające ze wskazówek
count_rows_bad ::
                (Num i, Enum i, MArray a1 a2 m, Ix i) =>
                a1 i a2 -> -- ^ Mutowalna tablica
                i -> -- ^ Numer wiersza
                i -> -- ^ Rozmiar talbicy
                m [a2] -- ^ Tablica niepoprawnych indeksów
count_rows_bad a row n = 
  sequence [read_array a (col',row) n | col' <- [1..n]]
-- | Niepoprawne indeksy kolumn.
-- Funkcja zwraca indeksy kolumn, które naruszają ograniczenia wynikające ze wskazówek
count_cols_bad ::
                (Num i, Enum i, MArray a1 a2 m, Ix i) =>
                a1 i a2 -> -- ^ Mutowalna tablica
                i -> -- ^ Numer kolumny
                i -> -- ^ Rozmiar talbicy
                m [a2] -- ^ Tablica niepoprawnych indeksów
count_cols_bad a col n =
  sequence [read_array a (col,row') n | row' <- [1..n]]

-- | Widzialne piramidy.
-- Funkcja wylicza ile piramid w wierszu tablicy widać.
count_visibility :: 
                  (Ord b, Num p, Num b) =>
                  [b] -> -- ^ Wiersz tablicy
                  p -- ^ Liczba widzialnych piramid.
count_visibility [] = 0
count_visibility (x:xs) 
  | x > 0 = 1 + count_visibility (map (\y -> y-x) xs)
  | otherwise = count_visibility xs 

-- | Puste miejsca.
-- Funkcja wylicza liczbę niezajętych pól w wierszu tablicy zagadki.
calc_empty_cells :: 
                  (Eq a, Num a) =>
                  [a] -> -- ^ Wiersz tablicy zagadki
                  Int -- ^ Liczba pustych miejsc (zer)
calc_empty_cells line = 
  length ([val | val <- line, val == 0]) 

-- | Poprawność wiersza.
-- Funkcja sprawdza czy wiersz posiada poprawne wartości w wierszu tablicy zagadki.
check_row ::
          (Ord a1, Num a1, Num a2, Eq a2) =>
          Maybe a2 -> -- ^ Ograniczenie wynikające ze wskazówki
          [a1] -> -- ^ Wiersz tablicy zagadki
          Bool -- ^ Wynik poprawności
check_row (Just constraint) line = 
    (calc_empty_cells line == 0 && count_visibility line == constraint) ||
    calc_empty_cells line > 0
check_row Nothing _ = True

