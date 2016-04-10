import MusicResources

makeStatsList :: [(Char,[(Int,Char)])]

makeStatsList = flat(map (makeStatsperchar) chars)



makeStatsperchar :: Char ->[(Char,[(Int,Char)])] 
makeStatsperchar a = [(a,sort((flat (removeEmpty (map (makeStatsforeach a (merge training)) chars)))))]


makeStatsforeach a l b
		|((countBafterA a l b)== 0) = []
		|otherwise = [((countBafterA a l b),b)]

countBafterA a (x:y:xs) b = if ( (x==a) && (b==y) ) then 
				(1+ (countBafterA a (y:xs) b)) else
 				(countBafterA a (y:xs) b)

countBafterA _ [x] _ = 0


removeEmpty [] = []
removeEmpty (x:xs)	|(x==[]) = removeEmpty xs
			|otherwise = (x:removeEmpty xs)

flat []=[]
flat (x:xs)=x++(flat xs)

merge []=[]
merge (x:xs)=x++"A"++(merge xs)

maxList [(a,b), (c,d)] = if(a<=c) then (c,d) else (a,b)
maxList ((a,b):xs)
				|((num(maxList xs))<=a)  = (a,b)
				|otherwise = maxList(xs)

num (a,b) = a

remove _ [] = []
remove e (x:xs)
		|(e==x) = xs
		|otherwise = (x:(remove e xs))


sort [] = []
sort [x] = [x]
sort (x:xs) = ((maxList (x:xs)):sort ((remove (maxList (x:xs)) (x:xs))))






 





