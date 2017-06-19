module DataTypes where

import System.Random

-- typ danych odpowiadajacy polu na planszy
data Field  = X | O | Empty

instance Show Field where
    show X = " X"
    show O = " O"
    show Empty = " ."

instance Eq Field where
    X == X = True
    O == O = True
    Empty == Empty = True
    X == _ = False
    O == _ = False
    Empty == _ = False

-- funkcja wspomagajaca wyswietlanie planszy
display::[Field]->String
display [] = ""
display (h:t) = (show h)++(display t)

-- typ danych odpowiadajacy planszy
data Board = Board [[Field]]

instance Show Board where
    show (Board []) = " 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9"
    show (Board (h:t)) = (display h)++(show (19 - (length t)))++"\n"++(show (Board t))

instance Eq Board where
    (Board a) == (Board b) = a == b

-- funkcja tworzaca pusta plansze 19x19
createBoard::Board
createBoard = Board (take 19 (repeat (take 19 (repeat Empty))))
