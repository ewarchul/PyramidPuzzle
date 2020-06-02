{-# LANGUAGE FlexibleContexts #-}
{- 
Module        : Interjs użytkownika
Description   : Obsługa standardowegowe wejścia.
-}
module Cli
  (
  get_dimension,
  get_hint,
  )
  where
  import Text.Read as TR
  import Control.Monad

  -- | Odpytanie o rozmiar tablicy zagadki 
  get_dimension :: IO Int
  get_dimension = do 
    putStrLn "Proszę podać rozmiar kwadratowej tablicy, na której będą umieszczane piramidy: \n"
    dim <- getLine
    case TR.readMaybe dim of
      Just x -> 
        return x
      Nothing -> 
        putStrLn "Rozmiar tablicy powinien być dodatnią liczbą całkowitą! [Exit: CTRL + C]" >> get_dimension

  -- | Odpytanie o wskazówkę 
  get_hint :: String -> IO [Maybe Int]
  get_hint side = do 
    putStrLn ("Proszę podać wskazówki dla krawędzi " ++ side ++ ".\n") 
    hint <- getLine
    case readMaybe hint :: Maybe [Int] of
      Just x ->
        return $ fmap (\e -> do 
                        if (e > 0) then Just e
                        else Nothing) x
      Nothing ->
        putStrLn "Błędnie podany format wskazówek! [Exit: CTRL + C]." >> get_hint side
  
