module endterm
import StdEnv

/* 1.
	Create a class MyMultDiv which has the operations *~ , /~ and has the neutral element as
	myOne. 
	So given two elements apply multiplication and division:
	*~ -> multiplication
	/~ -> division
	After that create an instace for Char.
	Hint: Operations in Char can be done on their ineteger representation and then convert back to string
	Be careful when you multiply and divide to stay in the range of 255 (you can use modulo)
*/
/*class MyMultDiv a
where 
	(*~) :: a a -> a
	(/~) :: a a -> a
	myOne :: a 
	
instance MyMultDiv Char 
where 
	(*~) ch1 ch2 = toChar((toInt(ch1) * toInt(ch2)) rem 255)
	(/~) ch1 ch2 = toChar((toInt(ch1) / toInt(ch2)) rem 255)
	myOne = '\0' 
	
*/

class MyMultDiv a 
where
	(*~) :: a a -> a
	(/~) :: a a -> a
	myOne :: a 
	
instance MyMultDiv Char
where 
	(*~) a b = toChar(((toInt a ) * (toInt b)) rem 255)
	(/~) a b = toChar(((toInt a ) / (toInt b)) rem 255)
	myOne = '\0'
	

	
//instance MyMultDiv Char

//Start = toInt('a') * toInt('b')	
//Start = 'a' *~ 'b' //'G'
//Start = 'k' *~ myOne //'k'
//Start= 'z' /~ 'A' //''

::University={uniName::String,students::[Student],teachers::[Teacher]}
::Teacher={tname::String,subject::String}
::Student={studentName::String,age::Int,grades::{Int},favoriteTeacher::Teacher}
ELTE::University
ELTE={uniName="ELTE",students=[Marko,Nikola,Josh,Dame],teachers=[Mary,Peter,John]}
BME::University
BME={uniName="BMI",students=[Ana,Josh,Sofi,Nikola],teachers=[Viktor,John,Peter]}
EmptyUni::University
EmptyUni={uniName="Empty",students=[],teachers=[]}
Peter::Teacher
Peter={tname="Peter",subject="Functional"}
Viktor::Teacher
Viktor={tname="Viktor",subject="Math"}
Mary::Teacher
Mary={tname="Mary",subject="OOP"}
John::Teacher
John={tname="John",subject="Functional"}
Marko::Student
Marko={studentName="Marko",age=19,grades={4,4,4,5},favoriteTeacher= Mary}
Sofi::Student
Sofi={studentName="Sofi",age=22,grades={5,5,4,5,5},favoriteTeacher=John}
Dame::Student
Dame={studentName="Dame",age=21,grades={2,3,4,5},favoriteTeacher=Peter}
Ana::Student
Ana={studentName="Ana",age=18,grades={5,5,5,5},favoriteTeacher=Viktor}
Nikola::Student
Nikola={studentName="Nikola",age=19,grades={4,4,4,4,2},favoriteTeacher=Peter}
Nik::Student
Nik={studentName="Nik",age=20,grades={4,4,4,4,3},favoriteTeacher=Peter}
Nik2::Student
Nik2={studentName="Nik2",age=22,grades={4,4,4,4,5},favoriteTeacher=Peter}
Josh::Student
Josh={studentName="Josh",age=22,grades={4,5,5},favoriteTeacher=John}


/*2 Given a University, return an array of all the 
students or teachers names which are shorter than 6*/
/*shorterThan6::University->{String}
shorterThan6 uni ={y \\ y <- [x.studentName \\ x <- uni.students | size(x.studentName) < 6 ] ++ [x.tname \\ x <- uni.teachers | size(x.tname) < 6 ]}
*/
shorterThan6::University->{String}
shorterThan6 uni = {k \\ k <- ([  x.studentName \\ x <- uni.students | (size x.studentName) < 6] ++ [  x.tname \\ x <- uni.teachers | (size x.tname) < 6])}


//Start=shorterThan6 BME//{"Ana","Josh","Sofi","John","Peter"}
//Start=shorterThan6 ELTE//{"Marko","Josh","Dame","Mary","Peter","John"}
//Start=shorterThan6 EmptyUni//{}
/*
stAVG :: Student -> Int
stAVG st = avg[ x \\ x <-: st.grades]

uniAVG :: University -> Int
uniAVG uni 
| uni.uniName == "Empty" = 0
uniAVG uni = avg(map (\x = stAVG x) (uni.students))



/*3 Write a function which will take an array of Universities and return the University with the highest overall gpa (the average of the average of each student)*/
highestGpa::{University}->{String}
highestGpa arr = {k \\ k <- [ x.uniName \\ x <- lst | uniAVG x == maxList ( map (\x = uniAVG x ) lst) ]}
where 	
	lst = [x \\ x <-: arr]

*/

Stavg :: Student -> Int
Stavg st = avg[ x \\ x <-: st.grades]

Uniavg :: University -> Int
Uniavg uni
| uni.uniName == "Empty" = 0
 = avg[ Stavg x \\ x <- uni.students]

highestGpa :: {University} -> {String}
highestGpa arr = {y \\ y <-[ x.uniName \\ x <- lst | Uniavg x == maxList (map(\x = Uniavg x) lst)]}
where 
	lst = [ x \\ x <-: arr]

//Start=highestGpa {ELTE,BME,EmptyUni}//"BMI"
//Start=highestGpa {ELTE,BME} //"BMI"
//Start=highestGpa {EmptyUni,EmptyUni}//"Empty"
//Start=highestGpa {ELTE} //"ELTE"
//Start=highestGpa {}//"No universities given"

//Start = 23
/*4	Create a toString instance for Student such that for given student ex. Nikola={studentName="Nikola",age=19,grades={4,4,4,4,2},
favoriteTeacher=Peter} it gives "Nikola 3.6 Peter" where 3.6 is the student's gpa and Peter is the student's favorite teacher's name*/

/*
instance toString Student
where 
	toString st = st.studentName +++ " " +++ fromInt(stAVG st)+++" " +++ st.favoriteTeacher.tname

//Start=toString Nikola//"Nikola  3.6  Peter"
//Start=toString Marko//"Marko  4.25  Mary"
//Start=toString Nik//"Nik  3.8  Peter"
//Start=toString Dame//"Dame  3.5  Peter"
*/

instance toString Student
where 
	toString st = st.studentName +++ " " +++ fromInt(Stavg st) +++ " " +++ st.favoriteTeacher.tname
	
//Start=toString Nikola//"Nikola  3.6  Peter"
//Start=toString Marko//"Marko  4.25  Mary"
//Start=toString Nik//"Nik  3.8  Peter"
//Start=toString Dame//"Dame  3.5  Peter"

/* 5
A good person is the person who never lies, so let's test this quote, we have list of people, each person has name
and list of the names of the people that he lies to, 
your task is to get the people who can say the truth to all the people in the given list 
list can be empty if all the people did lie.
Example : goodPeople [{fake_name = "Rafaat Ismail", peopleToLie = ["Adel"]},{fake_name = "Lucifier", peopleToLie = ["Rafaat Ismail"]}
Output : [{fake_name = "Rafaat Ismail", peopleToLie = ["Adel"]}] because Adel is not on the given list.
note : ofcourse we will consider fake names for this expirement so all the names here are fictional.
*/

::Person = {fake_name :: String, peopleToLie :: [String]}
/*
auxx :: [Person] -> [String]
auxx lst = [x.fake_name \\ x <- lst ]

goodPeople :: [Person] -> [Person]
goodPeople lst = [ p \\ p <- lst |  and(map(\x =not( isMember x p.peopleToLie)) (auxx lst))]//not(isMember (p.peopleToLie) (auxx lst))]    //not(isMember (p.peopleToLie) (auxx lst))]

*/



//Start = duxx [{fake_name = "Rafaat Ismail", peopleToLie = ["Adel","Maggi"]},{fake_name = "Lucifier", peopleToLie = ["Adel","Rafaat Ismail"]},{fake_name = "elkenona", peopleToLie = ["Adel","Lucifier"]}]




//Start = goodPeople [{fake_name = "Rafaat Ismail", peopleToLie = ["Adel","Maggi"]},{fake_name = "Lucifier", peopleToLie = ["Adel","Rafaat Ismail"]},{fake_name = "elkenona", peopleToLie = ["Adel","Lucifier"]}] // [{fake_name = "Rafaat Ismail", peopleToLie = ["Adel","Maggi"]}]
//Start = goodPeople [{fake_name = "Alaa Abdelazim", peopleToLie = ["Brnadt","Shelby"]},{fake_name = "Bartaleaa", peopleToLie = ["Alaa Abdelazim","Shelby"]},{fake_name = "Shelby", peopleToLie = ["Bartaleaa"]}] // []

//6. Given two arrays, return new array such that i-th element of it is maximum of i-th element of first and second arrays.
// So for example, when we calculate 5th element of result array, we look at 5th element of first and 5th element of second arrays
// And choose maximum of the two.
// You can assume that arrays have same length. 

maxOfTwo :: {Int} {Int} -> {Int}
maxOfTwo arr1 arr2 = { max a b \\ a <-: arr1 & b <-: arr2 }

maxy :: {Int} {Int} -> {Int}
maxy arr1 arr2 = { max a b \\ a <-: arr1 & b <-: arr2}

//Start = maxOfTwo {} {} // {}
//Start = maxOfTwo {1} {5} // {5}
//Start = maxOfTwo {1,5,4} {2,3,6} // {2,5,6}
//Start = maxOfTwo {1,2,3,4,5} {1,2,3,4,5} // {1,2,3,4,5}

//7. You are given array of integers.
// Your function should return true if each value appears at least twice in the array, and it should return false
// if any element is distinct.
fj :: Int [Int] -> Int
fj n [] = 0
fj n [x:xs] 
| n == x = 1 + fj n xs
= fj n xs

tj :: Int [Int] -> Int
tj n [] = 0
tj n [x:xs]
| n == x = 1 + tj n xs 
= tj n xs 

f77 :: {Int} -> Bool 
f77 arr = and[ tj x lst >= 2 \\ x <-: arr]
where 
	lst = [ x \\ x <-: arr]

f7 :: {Int} -> Bool
f7 arr = and [(fj x lst > 1) \\ x <-: arr]
where 
	lst = [ x \\ x <-: arr]



//Start = f7 {1,2,3,1,3,2,2,2} // True
//Start = f77 {1,2,3,4,3,2,1} // False
//Start = f7 {1,1,1,3,3,4,3,2,4,2} // True




//8.Array is monotonic if it is either monotone increasing or monotone decreasing
// A is monotone increasing if for all i<=j, A[i]<=A[j]
// A is monotone decreasing if for all i<=j, A[i]>=A[j]
// Given array, your task is to decide if it is monotonic.

//hii sort

//Start = sortBy (>) [3,5,2,1,6,9,8]

func :: {Int} -> [Int]
func arr = [x \\ x <-: arr]

my :: {Int} -> Bool
my arr 
| sort (func arr) == (func arr) = True
| sortBy (>) (func arr) == (func arr) = True
= False

//Start = sort [4,5,6,2,1]
//Start = sortBy (<) [4,5,6,2,1]

hi :: {Int} -> [Int]
hi arr = [ a \\ a <-: arr]

bye :: [Int] -> Bool
bye [] = True
bye [x] = True
bye [x,y,z:xs] 
| ((y-x) == (z-y)) = bye xs 
= False

isMonotonic :: {Int} -> Bool
isMonotonic arr = bye (hi arr)

//Start = my {6,5,4,4} // True
//Start = isMonotonic {6,5,4,4} // True
//Start = my {1,3,2} // False
//Start = isMonotonic {1,2,4,5} // False
//Start = isMonotonic {1,1,1} // True





/* 9
Please write a function that, given a Tree and a predicate,
will find nodes that do not return True for the predicate
and will remove those nodes and their subtrees.
Note: The expected return results are listed below with an equality
for your convenience, so that you do not have to manually check your result.
If your result is correct, the Start statement should return a True.
*/


jk :: (Tree a) (a->Bool) -> (Tree a) | Eq a
jk Leaf cond = Leaf
jk (Node x l r) cond 
| cond x = Node x (jk l cond) (jk r cond)
=Leaf

:: Tree a = Node a (Tree a) (Tree a) | Leaf 

//Start = jk specialTree isEven == (Node 10 (Node 4 Leaf Leaf) Leaf) //True
//Start = pruneTree specialTree ((<)7) == (Node 10 Leaf (Node 15 (Node 12 (Node 11 Leaf Leaf) (Node 13 Leaf Leaf)) (Node 17 (Node 16 Leaf Leaf) (Node 19 (Node 18 Leaf Leaf) (Node 20 Leaf Leaf))))) //True
//Start = pruneTree specialTree notPrime == (Node 10 (Node 4 (Node 1 (Node 0 Leaf Leaf) Leaf) Leaf) (Node 15 (Node 12 Leaf Leaf) Leaf)) //True


instance == (Tree a) | Eq a
where 
	(==) Leaf Leaf = True
	(==) (Node x1 l1 r1) (Node x2 l2 r2) = and [ (x1==x2) && (l2==l1) && (r1 == r2)]
	(==) _ _ = False



specialTree :: (Tree Int)
specialTree = Node 10 (Node 4 (Node 1 (Node 0 Leaf Leaf)(Node 2 Leaf Leaf))(Node 5 Leaf (Node 6 Leaf Leaf)))(Node 15 (Node 12 (Node 11 Leaf Leaf)(Node 13 Leaf Leaf))(Node 17 (Node 16 Leaf Leaf)(Node 19 (Node 18 Leaf Leaf)(Node 20 Leaf Leaf))))
pruneTree :: (Tree a) (a -> Bool) -> (Tree a)
pruneTree Leaf cond = Leaf 
pruneTree (Node x l r) cond 
| cond x = Node x (pruneTree l cond) (pruneTree r cond)
= Leaf 

notPrime :: Int -> Bool
notPrime x
| x <= 1 = True
= not(isEmpty[n\\n<-[2..(x-1)]|x rem n == 0])


//Start = pruneTree specialTree isEven == (Node 10 (Node 4 Leaf Leaf) Leaf) //True
//Start = pruneTree specialTree ((<)7) == (Node 10 Leaf (Node 15 (Node 12 (Node 11 Leaf Leaf) (Node 13 Leaf Leaf)) (Node 17 (Node 16 Leaf Leaf) (Node 19 (Node 18 Leaf Leaf) (Node 20 Leaf Leaf))))) //True
//Start = pruneTree specialTree notPrime == (Node 10 (Node 4 (Node 1 (Node 0 Leaf Leaf) Leaf) Leaf) (Node 15 (Node 12 Leaf Leaf) Leaf)) //True





/*10
Given a tree and an integer. Find all the nodes that equal to the integer and give the sum 
of their direct children.(Leaf count as 0).
*/

exNode :: (Tree Int) -> Int
exNode (Node x l r) = x

f10 :: (Tree Int) Int -> Int
f10 Leaf n = 0
f10 (Node x l r) n
| n == x = exNode l + exNode r + f10 l n + f10 r n 
= f10 l n + f10 r n 


//Start = f10 (Node 2 Leaf Leaf) 3 //0
//Start = f10 (Node 3 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf)) 3 //2
//Start = f10 (Node 1 (Node 0 Leaf Leaf)(Node 2 Leaf Leaf)) 1 //2
//Start = f10 (Node 2 (Node 1 Leaf Leaf)(Node 2 (Node 3 Leaf Leaf) (Node 1 Leaf Leaf))) 2 //7
//Start = f10 (Node 2 (Node 1 Leaf Leaf)(Node 2 Leaf (Node 1 Leaf Leaf))) 2 //4