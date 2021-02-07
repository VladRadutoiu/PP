{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A
{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell Char Position deriving(Eq)
instance Show Cell
    where show (Cell char pos) =   show char ++ " " ++ show pos
        

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level (A.Array(Int,Int) Char)
    deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}
lowerRightCorner :: (A.Array(Int,Int) Char) -> (Int, Int)
lowerRightCorner = snd . A.bounds
lowerLeftCorner :: (A.Array(Int,Int) Char) -> (Int, Int)
lowerLeftCorner = fst . A.bounds

instance Show Level 
    where show (Level arr) = "\n" ++ [if y == 1 + (snd (lowerRightCorner arr)) then endl  else arr A.! (x,y) | x<- [0..(fst (lowerRightCorner arr))], y<-[0..(snd (lowerRightCorner arr)) +1]] 

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel pos = Level(A.array((0,0),pos) [((a,b),emptySpace) | a<-[0..(fst pos)],b<-[0..(snd pos)] ])

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell (c,pos) (Level arr)= if pos <=lowerRightCorner arr && pos >=(0,0) then Level(arr A.//[(pos,c)]) else Level arr


{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
changeArray :: Level -> [(Char,Position)] ->Level
changeArray (Level l) list =   Level (l A.// [((snd x),(fst x))  | x <-rev])
    where rev = reverse list
        
createLevel :: Position -> [(Char, Position)] -> Level
createLevel pos list = (changeArray (emptyLevel pos) list)
               



{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}
checkEmpty:: Char ->Char-> Int
checkEmpty arr prechange = if arr == emptySpace && prechange /= startUp && prechange /= startDown && prechange /= startLeft && prechange /= startRight && prechange /= winRight && prechange /= winLeft && prechange /= winDown && prechange /= winUp then 1
    else 0

changePos :: Position-> Directions-> Position
changePos (x,y) d = if d == North then (a-1,b)
    else if d == South then (a+1,b)
        else if d == East then (a,b+1)
        else  (a,b-1)
        where a = x
              b = y
      
updateLevel:: Position->Position->Level->Level
updateLevel pos1 pos2 (Level l) = Level(l A.//[(pos2, (l A.! pos1))]) 

makeEmpty:: Position->Level->Level
makeEmpty (a,b) (Level l) = Level(l A.//[((a,b), emptySpace)])

makeChange :: Position-> Level -> Position -> Level
makeChange prechange (Level l) (a,b)  = if a < 0 then (Level l)
      else if a > x then (Level l)
      else if b < 0 then (Level l)
      else if b > y then (Level l)
      else if  (checkEmpty (l A.! (a,b)) ( l A.! prechange)) == 0
                then (Level l)
                else (makeEmpty prechange (updateLevel prechange (a,b) (Level l)))
        where y = snd (lowerRightCorner l)
              x = fst (lowerRightCorner l)

moveCell :: Position -> Directions -> Level -> Level
moveCell pos d (Level l) =  (makeChange pos (Level l) (changePos pos d)) 
       
{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
{-
    *** TODO ***
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final

-}

checkConnTop:: Char->Char->Bool
checkConnTop char1 char2 = if char1 == verPipe && ( char2 == topRight || char2 == topLeft || char2 == verPipe || char2 == startUp) then True
                            else if char1 == horPipe then False
                        else if char1 == topRight || char1 == topLeft then False
                        else if (char1 == botLeft || char1 == botRight) && (char2 == verPipe || char2 == topLeft || char2 == topRight || char2 == startUp) then True
                        else if(char1 == winLeft || char1 == winRight || char1 == winUp || char1 == startRight || char1 == startLeft || char1 == startUp) then False
                           else True

checkConnBot:: Char->Char->Bool
checkConnBot char1 char2 = if char1 == verPipe && ( char2 == botLeft || char2 == botRight || char2 == verPipe || char2 == startDown) then True
                            else if char1 == horPipe then False
                        else if (char1 == topRight || char1 == topLeft) && (char2 == verPipe || char2 == botLeft || char2 == botRight || char2 == startUp || char2 == winDown)then False
                        else if (char1 == botLeft || char1 == botRight)  then False
                        else if(char1 == winLeft || char1 == winRight || char1 == winDown || char1 == startRight || char1 == startLeft || char1 == startDown) then False
                           else True

checkConnLeft:: Char->Char->Bool
checkConnLeft char1 char2  = if (char1 == verPipe || char1 == topLeft || char1 == botLeft) then False
                            else if (char1 == horPipe || char1 == topRight || char2 == botRight)  && ( char2 == botLeft || char2 == startLeft || char2 == topLeft || char2 == horPipe || char2 == winLeft)then True
                        else if(char1 == winRight || char1 == startRight || char1 == winUp || char1 == winDown || char1 == startDown || char1 == startUp) then False
                            else True

checkConnRight:: Char->Char->Bool
checkConnRight char1 char2  = if (char1 == verPipe || char1 == topRight || char1 == botRight) then False
                            else if (char1 == horPipe || char1 == topLeft || char2 == botLeft)  && ( char2 == botRight || char2 == startRight || char2 == topRight || char2 == horPipe || char2 == winRight)then True
                        else if(char1 == winLeft || char1 == startLeft || char1 == winUp || char1 == winDown || char1 == startDown || char1 == startUp) then False
                           else True

connection:: Cell -> Cell -> Directions -> Bool
connection (Cell char1 pos1) (Cell char2 pos2) d = if d == North then (checkConnTop char1 char2)
    else if d == South then (checkConnBot char1 char2)
    else if d == East then (checkConnRight char1 char2)
    else (checkConnLeft char1 char2)


{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
checkForWin:: Level->Position->[Char]->Bool
checkForWin (Level l) (a,b) char= if a < 0 then False
      else if a > x then False
      else if b < 0 then False
      else if b > y then False
      else if (char == "vin de sus" || char == "vin de jos") && ((l A.! (a,b)) == horPipe || (l A.! (a,b)) ==winLeft ||(l A.! (a,b)) == winRight) then False
      else if (l A.! (a,b)) == emptySpace then False
      else if (char == "vin din stanga" || char == "vin din dreapta") && ((l A.! (a,b)) == verPipe || (l A.! (a,b)) == winUp || (l A.! (a,b)) == winDown) then False
        else if (l A.! (a,b)) == horPipe && char == "vin din dreapta" then (checkForWin (Level l) (a,b-1) "vin din dreapta")
        else if (l A.! (a,b)) == horPipe && char == "vin din stanga" then (checkForWin (Level l) (a,b+1) "vin din stanga")

        else if (l A.! (a,b)) == verPipe && char == "vin de sus" then (checkForWin (Level l) (a+1,b) "vin de sus")
        else if (l A.! (a,b)) == verPipe && char == "vin de jos" then (checkForWin (Level l) (a-1,b) "vin de jos")

        else if (l A.! (a,b)) == topLeft && char == "vin de jos" then (checkForWin (Level l) (a,b+1) "vin din stanga")
        else if (l A.! (a,b)) == topLeft && char == "vin din dreapta" then (checkForWin (Level l) (a+1,b) "vin de sus")

         else if (l A.! (a,b)) == topRight && char == "vin din stanga" then (checkForWin (Level l) (a+1,b) "vin de sus")
        else if (l A.! (a,b)) == topRight && char == "vin de jos" then (checkForWin (Level l) (a,b-1) "vin din dreapta")

         else if (l A.! (a,b)) == botRight && char == "vin de sus" then (checkForWin (Level l) (a,b-1) "vin din dreapta")
        else if (l A.! (a,b)) == botRight && char == "vin din stanga" then (checkForWin (Level l) (a-1,b) "vin de jos")

         else if (l A.! (a,b)) == botLeft && char == "vin de sus" then (checkForWin (Level l) (a,b+1) "vin din stanga")
        else if (l A.! (a,b)) == botLeft && char == "vin din dreapta" then (checkForWin (Level l) (a-1,b) "vin de jos")
              else if (l A.! (a,b)) == winDown && char == "vin de jos" then True
              else if (l A.! (a,b)) == winUp && char == "vin de sus" then True
              else if (l A.! (a,b)) == winLeft && char == "vin din stanga" then True
              else if (l A.! (a,b)) == winRight && char == "vin din dreapta" then True
            else False
             where x = fst (lowerRightCorner l)
                   y = snd (lowerRightCorner l)

checkWon:: Level->Position->Bool
checkWon (Level l) (a,b) = if (l A.! (a,b)) == startUp then (checkForWin (Level l) (a-1,b) "vin de jos")
    else if (l A.! (a,b)) == startDown then (checkForWin (Level l) (a+1,b) "vin de sus")
    else if (l A.! (a,b)) == startLeft then (checkForWin (Level l) (a,b-1) "vin din dreapta")
    else if (l A.! (a,b)) == startRight then (checkForWin (Level l) (a,b+1) "vin din stanga")
                else False
wonLevel :: Level -> Bool
wonLevel (Level l) = if(length [(x,y) | x<-[0..a],y<-[0..b],(checkWon (Level l) (x,y)) == True]) >=1 then True
    else False
 where a = fst (lowerRightCorner l)
       b = snd (lowerRightCorner l)


checkSuccesor::Position->Level->Directions->Bool
checkSuccesor (a,b) (Level l) d =
     if (l A.! (a,b)) == emptySpace || ((l A.! (a,b)) /= emptyCell && (l A.! (a,b)) /= horPipe && (l A.! (a,b)) /= verPipe && (l A.! (a,b)) /= botLeft && (l A.! (a,b)) /= botRight && (l A.! (a,b)) /= topLeft && (l A.! (a,b)) /= topRight) then False
    else if d== North && a-1 >=0 && (l A.! (a-1,b)) == emptySpace  then True 
    else if d== South && a+1 <= x &&(l A.! (a+1,b)) == emptySpace then True 
     else if d== East && b+1 <= y &&(l A.! (a,b +1)) == emptySpace then True 
    else if d== West && b-1 >= 0 &&(l A.! (a,b-1)) == emptySpace then True 
        else False
         where y = snd (lowerRightCorner l)
               x = fst (lowerRightCorner l)

copyArray::Level->(A.Array(Int,Int) Char)
copyArray (Level arr) = A.array((0,0),pos) [ ((a,b),(arr A.! (a,b))) | a<-[0..(fst pos)],b<-[0..(snd pos)] ]
    where pos = lowerRightCorner arr

returnNewArray::Level->Level->Position->Position->Level
returnNewArray (Level l) (Level l2) initi final = Level( l2 A.// [ if x == final then (x, (l A.! initi)) else (x, emptySpace) | x<-[initi,final]])

instance ProblemState Level (Position, Directions) where
    successors (Level l) = [if d == North then (((x,y),d),(returnNewArray (Level l) (Level(copyArray (Level l))) (x,y) (x-1,y))) else if d == South then (((x,y),d),(returnNewArray (Level l)(Level(copyArray (Level l))) (x,y) (x+1,y))) else if d == East then (((x,y),d),(returnNewArray (Level l) (Level(copyArray (Level l))) (x,y) (x,y+1))) else (((x,y),d),(returnNewArray (Level l)(Level(copyArray (Level l))) (x,y) (x,y-1))) | x<-[0..a],y<-[0..b],d<-[North,South,East,West], (checkSuccesor (x,y) (Level l) d) == True]
         where a = fst (lowerRightCorner l)
               b = snd (lowerRightCorner l)
    

    isGoal (Level l)= undefined

    reverseAction (((x,y),d),(Level l))= if d == North then (((x-1,y),South),(Level l))
        else if d == South then (((x+1,y),North),(Level l))
        else if d == East then (((x,y-1),West),(Level l))
        else if d == West then  (((x,y+1),East),(Level l))
            else (((x,y),d),(Level l))
            
checkIfEqual::Level->Level->Bool
checkIfEqual (Level l1) (Level l2) = if(length [ x | x<-[0..a],y<-[0..b],(l1 A.! (x,y)) == (l2 A.! (x,y))]) == (a+1)*(b+1) then True
                                    else False 
                                    where a = fst (lowerRightCorner l1)
                                          b = snd (lowerRightCorner l1)
checkIfListEqual::Level->[Level]->Bool
checkIfListEqual (Level l) list = if(length [x | x<-list,(checkIfEqual x (Level l)) == False]) == length list then True
                                  else False

concatLists::Level->[Level]->[Level]
concatLists (Level l) list = list ++ [(Level l)]
