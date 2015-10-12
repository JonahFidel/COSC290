--Jonah Fidel 
--9/24/15
--Discrete Structures 
--Lab #4 
--Prof. Mulry 

--1.) 
double x = 2*x 
add x y = x + y 

--Main> map double [2, 3, 4]
----[4,6,8]
--Main> map (add 2) [3, 4, 5, 6, 7]
--[5,6,7,8,9]
--Main> :type map add
--map add :: Num a => [a] -> [a -> a]

--2.) 
rel1 1 = [2]
rel1 2 = [3] 
rel1 3 = [1, 3]
rel1 4 = [1]

rel2 1 = [10, 11]
rel2 2 = [12, 10]
rel2 3 = []
rel2 10 = [2]
rel2 11 = [2]
rel2 12 = [1]

rel3 1 = [2]
rel3 2 = [3]
rel3 3 = [1, 4]
rel3 4 = [1]

flatten ([]) = []
flatten (start:rest) = start ++ flatten(rest)

comp(rel2, rel1) x = fixlist$ flatten (map rel2 (rel1 x))

inlist val [] = False 
inlist val (start:rest)  = if val == start 
					then True 
					else inlist val rest 

fixlist [] = []
fixlist [start] = [start]
fixlist (start:rest) = if inlist start rest 
					then fixlist rest 
					else [start] ++ fixlist (rest)

relcomp 0 rel = error "not possible"
relcomp 1 rel = rel  
relcomp 2 rel = comp(rel, rel) 
relcomp n rel = comp(rel, relcomp (n-1) rel) 

--Main> relcomp 2 rel3 1 
--[3]
--Main> relcomp 3 rel3 1 
--[1,4]
--Main> relcomp 4 rel3 1 
--[2,1]

reflexrel rel num = fixlist(rel num ++ [num])

transrel rel var = fixlist(((relcomp 4 rel) var) ++ ((relcomp 3 rel) var) ++ ((relcomp 2 rel) var) ++ (rel var))

transrel2 1 rel var = rel var
transrel2 n rel var = fixlist(((relcomp n rel) var) ++ ((transrel2 (n-1) rel) var))



