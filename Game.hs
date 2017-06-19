module Game where

import DataTypes
import System.Random

-- funkcja wstawiajaca odpowiedni symbol na plansze
makeMove::Board->(Int, Int)->Field->Board
makeMove (Board a) (x, y) f = Board ((take (x-1) a)++[(take (y-1) (a!!(x-1)))++[f]++(drop y (a!!(x-1)))]++(drop x a))

-- funkcja sprawdzajaca czy dane pole na planszy jest puste
isFieldEmpty::Board->(Int, Int)->Bool
isFieldEmpty (Board l) (x,y) = ((l!!(x-1))!!(y-1)) == Empty

-- funkcja sprawdzajaca czy w danej linii (liscie pol) znajduje sie dokladnie 5 pol pod rzad
isFiveInLine::[Field]->Int->Field->Bool
isFiveInLine (h:t) 5 f = do
    if h == f
        then isFiveInLine t 6 f
        else True
isFiveInLine [] _ _ = False
isFiveInLine (h:t) i f
    | h == Empty   = isFiveInLine t 0 Empty
    | h == f       = isFiveInLine t (i+1) f
    | otherwise    = isFiveInLine t 1 h

-- funkcja wyciagajaca linie (liste pol) z planszy
extractLine::Board->[Field]->(Int,Int)->(Int,Int)->[Field]
extractLine (Board l) a (x, y) (vx, vy) = do
    if (x<=0 && vx==(-1)) || (x>=18 && vx==1) || (y<=0 && vy==(-1)) || (y>=18 && vy==1)
        then a
        else extractLine (Board l) (((l!!(x+vx))!!(y+vy)):a) (x+vx, y+vy) (vx,vy)

-- funkcja sprawdzajaca czy w linii zadanej pozycja poczatkowa i offsetem zostaje spelniony warunek konca gry
checkLine::Board->(Int,Int)->(Int,Int)->Bool
checkLine (Board l) (x,y) (vx,vy) = do
    let line = (extractLine (Board l) [] (x,y) (vx,vy))++[(l!!x)!!y]++(reverse (extractLine (Board l) [] (x,y) (-vx,-vy)))
    isFiveInLine line 0 Empty

-- funkcja sprawdzajaca czy wykonany ruch konczy gre
isGameFinished::Board->(Int,Int)->Bool
isGameFinished b (x,y) = 
    (checkLine b (x-1, y-1) (-1,-1)) || (checkLine b (x-1, y-1) (0,-1)) || (checkLine b (x-1, y-1) (-1,0)) || (checkLine b (x-1, y-1) (1,-1))

-- funkcja wczytujaca ze standardowego wejscia ruch gracza
readMove::Board->IO (Int, Int)
readMove b = do
    putStrLn ("Podaj wiersz (1-19): ")
    strX <- getLine
    putStrLn ("Podaj kolumne (1-19): ")
    strY <- getLine
    let x = (read strX::Int)
    let y = (read strY::Int)
    if x>=1 && x<=19 && y>=1 && y<=19 && isFieldEmpty b (x,y)
        then return (x,y)
        else do
            putStrLn ("Niepoprawny ruch, spróbuj jeszcze raz")
            readMove b

-- funkcja odpowiadajaca za wykonanie ruchu przez gracza
playerMove::Board->Field->IO Board
playerMove b f = do
    putStrLn ("Ruch gracza"++(show f))
    (x,y) <- readMove b
    let newBoard = (makeMove b (x,y) f)
    putStrLn (show newBoard)
    if isGameFinished newBoard (x,y)
        then return (Board [])
        else return newBoard

-- funkcja losujaca ruch AI
generateMove::Board->IO (Int,Int)
generateMove b = do
    x <- randomRIO (1,19::Int)
    y <- randomRIO (1,19::Int)
    if isFieldEmpty b (x,y)
        then return (x,y)
        else generateMove b

-- funkcja odpowiadajaca za wykonanie ruch przez AI
aiMove::Board->Field->IO Board
aiMove b f = do
    putStrLn ("Ruch gracza"++(show f))
    (x,y) <- generateMove b
    let newBoard = makeMove b (x,y) f
    putStrLn (show newBoard)
    if isGameFinished newBoard (x,y)
        then return (Board [])
        else return newBoard