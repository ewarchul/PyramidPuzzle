# Piramidy


## Jak rozwiązywać piramidy?

* na planszy ustaw piramidy o wysokościach od 1 do n (przykładzie od 1 do 4)
* w każdym wierszu i kolumnie ustaw cztery piramidy różnej wysokości
* liczby oznaczają ile piramid widać w rzędzie z tego miejsca (1 - widać jedną, najwyższą piramidę, 4 - widać cztery piramidy, czyli ustawione są "według wzrostu")
* wyższe piramidy zasłaniają niższe
* każda łamigłówka ma dokładnie jedno rozwiązanie!

Źródło: https://www.wydawnictwologi.pl/index.php?_route_=piramida

## Użycie programu 

pobierz repozytorium:

```
git clone https://github.com/warbarbye/SPOP.git

```
wejdź do katalogu z projektem:

```
cd SPOP
```

wykonaj następujące komendy:

```
stack build

stack exec PyramidPuzzle-exe
```


## Działanie programu

![App demo](app-demo.gif)

## Wymagania

- `stack 2.3.1`
- `ghc 8.8.3`

