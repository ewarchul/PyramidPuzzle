module Main

where

import Array
import Pyramid
import Solver
import Cli
import Data.Array.IO as IOArray

main = do

    putStrLn "UWAGA!"
    putStrLn "Wskazówki powinny być podane w postaci listy, której elementy oddzielone są przecinkiem."
    putStrLn "Jeśli w danym wierszu lub kolumny NIE MA wskazówki -- wpisz: 0\n"

    dim <- get_dimension
    top <- get_hint "GÓRNEJ"
    bottom <- get_hint "DOLNEJ"
    left <- get_hint "LEWEJ"
    right <- get_hint "PRAWEJ"

    let piramids = 
            Piramids top bottom left right
    puzzle_array <- 
            IOArray.newArray (1, dim*dim) 0 :: IO (Array)

    putStrLn "Rozwiązanie zagadki:"
    solve piramids dim puzzle_array (1, 1)
    putStrLn ""
    return ()
 
 
    






















