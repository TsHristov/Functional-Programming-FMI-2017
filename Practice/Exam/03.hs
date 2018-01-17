data Book = Book { isbn :: String,
                   title :: String,
                   authors :: [String]
                 } deriving (Show, Eq)

testBook1 = Book "0-86381-580-4" "Tile1" ["Author1", "Author3"]
testBook2 = Book "0-86381-580-4" "Tile2" ["Author1", "Author3"]
bookShelf = [testBook1, testBook2]

type Titles = [String]
type Books  = [Book]
type Author = String

-- a):
allTitles :: Books -> Author -> Titles
allTitles books author = map title $ filter (\book -> author `elem` (authors book)) books

-- b):

type ISBN = String
testISBN = "0-86381-580-4"

isbnToNumbers :: ISBN -> [Int]
isbnToNumbers = map (\x -> read [x] :: Int) . filter (/='-')  

validISBN :: ISBN -> Bool
validISBN isbn  = sumISBN `mod` 11 == 0 
  where sumISBN = sum $ zipWith (*) (isbnToNumbers isbn) code
        code = [10,9..1]

-- c):
data Person = Person { name    :: String,
                       friends :: [Person]
                     } deriving (Show)

instance Eq Person where
  a == b = name a == name b
  
type BookRatings = [(Person, Int, Book)]

person1 = Person "First" []
person2 = Person "Second" [person1]
ratings = [(person1, 3, testBook1), (person2, 4, testBook1)]

-- findFriends :: Person -> Book -> BookRatings -> [Person]
findFriends person book bookRatings = whichFriends ratedThisBook
  where ratedThisBook = filter (\(_,_,ratedBook) -> book == ratedBook) bookRatings
        whichFriends  = filter (\(friend,_,_) -> friend `elem` (friends person))

-- d):
