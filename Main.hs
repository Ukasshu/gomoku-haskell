import Game
import DataTypes 

-- główna pętla gry przyjmująca jako argument plansze, dwie funkcje wykonujace ruch i licznik ruchów
mainLoop::Board->(Board->Field->IO Board)->(Board->Field->IO Board)->Int->IO String
mainLoop board firstFunction secondFunction ct = do
    boardAfterFirst <- firstFunction board X
    if boardAfterFirst==(Board []) -- gry warunek skonczonej gry wewnatrz funkcji wykonujacej ruch zostaje spelniony - zostaje zwrocona plansza bez pól
        then return "Wygrywa gracz pierwszy (X)"
        else do
            if (ct+1)==361 --remi dla 361 ruchu
                then return "REMIS"
                else do 
                    boardAfterSecond <- secondFunction boardAfterFirst O
                    if boardAfterSecond==(Board [])
                        then return "Wygrywa gracz drugi (O)"
                        else mainLoop boardAfterSecond firstFunction secondFunction (ct + 2)

-- główna funkcja gry
main::IO String
main = do
    putStrLn "GOMOKU\n\nGracz vs AI - 1\nAI vs Gracz - 2\nGracz vs Gracz - 3\nAI vs AI - 4\nWyjscie - inna liczba\n"
    a <- getLine
    let opt = read a::Int
    if opt==1
        then do
            putStrLn (show createBoard)
            mainLoop createBoard playerMove aiMove 0
        else if opt==2
            then do
                putStrLn (show createBoard)
                mainLoop createBoard aiMove playerMove 0
            else if opt==3
                then  do
                    putStrLn (show createBoard)
                    mainLoop createBoard playerMove playerMove 0
                else if opt==4
                    then  do
                        putStrLn (show createBoard)
                        mainLoop createBoard aiMove aiMove 0
                    else return "No to elo!"