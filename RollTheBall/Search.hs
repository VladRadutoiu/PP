{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import Data.Maybe 
import Data.Maybe (fromJust)
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node {problemstate:: s,
                        action:: Maybe a,
                        parent:: Maybe (Node s a),
                        depth :: Int,
                        children:: [(Node s a)]} deriving Show  


{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState (Node s a n int l) = s
nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node s a n int l)= n


nodeDepth :: Node s a -> Int
nodeDepth (Node s a n int l)= int

nodeAction :: Node s a -> Maybe a
nodeAction (Node s a n int l)=  a


nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node s a n int l)= l

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

checkifapa::(ProblemState s a, Eq s)=>[s]->s->Bool
checkifapa l s = if length[x | x<-l,x==s] >=1 then True
                else False

removeState::(ProblemState s a, Eq s)=> [(a,s)]->[s]->[(a,s)]
removeState l1 l2 = [x | x<-l1, y<-l2, (snd x)/= y]

removeV::(ProblemState s a, Eq s)=> [s]->[(a,s)]->[(a,s)]
removeV visited succ = [x | x<-succ,y<-visited, (snd x)/=y]




changeParent::(ProblemState s a, Eq s)=> (Node s a)->(Node s a)->(Node s a)
changeParent n (Node s2 a2 parinte2 int2 l2) = (Node s2 a2 (Just n) int2 l2)

addParents::(ProblemState s a, Eq s)=> (Node s a)->(Node s a)
addParents (Node s a parinte int l) =   (Node s a parinte int  (map (\ (Node s2 a2 parinte2 int2 l2) ->(addParents(Node s2 a2 parinte2 int2 l2))) (map (\ (Node s2 a2 parinte2 int2 l2) ->(changeParent (Node s a parinte int l)(Node s2 a2 parinte2 int2 l2))) l) ))
                                       

convertMaybe:: a->Maybe a
convertMaybe a= listToMaybe [a]

createChildren:: (ProblemState s a, Eq s)=>[(a,s)]->Int->[s]->[(Node s a)]
createChildren l depth stari= [(Node (snd x) (convertMaybe (fst(reverseAction x))) Nothing depth (createChildren (successors (snd x)) (depth + 1) (stari ++ [(snd x)]))) | x<-l ] 


createFirstNode :: (ProblemState s a, Eq s) => s -> Node s a
createFirstNode s = (Node s Nothing Nothing 0 (createChildren (successors s) 1 [s]))


createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace s = addParents(createFirstNode s)
{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}
checkParent::Ord s =>Maybe(Node s a)->Maybe(Node s a)->Bool
checkParent Nothing Nothing = True
checkParent (Just(Node s1 a1 parinte1 int1 l1)) (Just(Node s2 a2 parinte2 int2 l2)) = if s1 /=s2 then False
                                                                                        else True
checkParent (Just(Node s1 a1 parinte1 int1 l1)) Nothing = False
checkParent Nothing (Just(Node s2 a2 parinte2 int2 l2)) = False

concatFlux:: [([Node s a], [Node s a])] ->[([Node s a], [Node s a])]->[([Node s a], [Node s a])]
concatFlux l1 l2 = l1 ++ l2

check::Ord s =>  Node s a->Node s a->Bool
check (Node s1 a1 parinte1 int1 l1) (Node s2 a2 parinte2 int2 l2) = if s1 /= s2 then True
                                                                    else if (checkParent parinte1 parinte2) == False then True
                                                                        else False

removeVisited::Ord s =>  [Node s a]->[Node s a]->[Node s a]
removeVisited l1 l2 =  if length l > 0 then l 
                        else []
                    where l = [y | x<-l1,y<-l2, (check x y) == True]

bfs2 ::Ord s =>  [Node s a]->[Node s a]->[Node s a]-> [([Node s a],[Node s a])]
bfs2 vecini visited flux   = (concatFlux [(vecini,flux)] (bfs2 (nodeChildren  (head flux)) (visited ++ [(head flux)]) (((tail flux)++ (nodeChildren (head flux))) )) )
bfs2 vecini visited [] = []
                          
bfs :: Ord s => Node s a -> [([Node s a],[Node s a])]
bfs (Node s a parinte int l)= tail (concatFlux [([(Node s a parinte int l)],[(Node s a parinte int l)])] (bfs2 l [(Node s a parinte int l)] l))




{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

checkElem::Ord s =>[Node s a] -> [Node s a]->Bool
checkElem l1 l2= if null [x | x<-l1,y<-l2,(nodeState y) == (nodeState x) && (nodeState (fromJust (nodeParent x))) /= (nodeState (fromJust (nodeParent y)))] == True then False
    else True

getElem::Ord s =>[Node s a] -> [Node s a]->((Node s a),(Node s a))
getElem l1 l2 = head [(x,y) | x<-l1, y<-l2,(nodeState y) == (nodeState x) && (nodeState (fromJust (nodeParent x))) /= (nodeState (fromJust (nodeParent y))) ]

containsEquals::Ord s =>[([Node s a],[Node s a])]->[([Node s a],[Node s a])]->(Node s a, Node s a)
containsEquals l1 l2 =   if  (checkElem (fst (head l1)) (snd (head l2))) == True then (getElem (fst (head l1)) (snd (head l2)))
                        else if (checkElem (snd (head l1)) (fst (head l2))) == True then (getElem (snd (head l1)) (fst (head l2)))
                        else containsEquals (tail l1) (tail l2)

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS (Node s1 a1 parinte1 int1 l1) (Node s2 a2 parinte2 int2 l2)=  containsEquals (bfs (Node s1 a1 parinte1 int1 l1)) (bfs (Node s2 a2 parinte2 int2 l2))

        


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}
extractPathr :: Maybe(Node s a) -> [(Maybe a, s)]
extractPathr   (Just (Node s a parinte int l)) = (extractPathr parinte) ++ [(a,s)]
extractPathr   Nothing = []

                                        

extractPath :: Node s a -> [(Maybe a, s)]
extractPath (Node s a parinte int l) = extractPathr (Just (Node s a parinte int l))




{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}



solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve s1 s2= undefined
        
