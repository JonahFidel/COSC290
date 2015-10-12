--Jonah Fidel 
--10/8/15
--Discrete Structures 
--Professor Mulry 
--Lab #6 

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

reflexrel rel num = fixlist(rel num ++ [num])

transrel rel var = fixlist(((relcomp 4 rel) var) ++ ((relcomp 3 rel) var) ++ ((relcomp 2 rel) var) ++ (rel var))

transrel2 1 rel var = rel var
transrel2 n rel var = fixlist(((relcomp n rel) var) ++ ((transrel2 (n-1) rel) var))

rlist = [1,2,3,4]
slist = [1,2,3,4]
plist = [1,2,3,4]
qlist = [1,2,3,4,5,6,7,8]

r rlist 1 = [2, 4]
r rlist 2 = [3]
r rlist 3 = [4]
r rlist 4 = []

s slist 1 = [2]
s slist 2 = [3, 4]
s slist 3 = []
s slist 4 = []

p plist 1 = [3]
p plist 2 = [3]
p plist 3 = [4]
p plist 4 = []

q qlist 1 = [3]
q qlist 2 = [4]
q qlist 3 = [5]
q qlist 4 = [6]
q qlist 5 = [7]
q qlist 6 = [7]
q qlist 7 = [8]
q qlist 8 = []

listlength ([]) = 0 
listlength (new:rest) = 1 + (listlength rest)

transrelnew r rlist = transrel2 (listlength rlist) (r rlist)


