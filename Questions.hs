module Questions (countMales, eldest,  ageDifferenceString) where
import AddressBook
import Data.Time
import Data.List
import Data.Ord

countMales :: AddressBook -> String
countMales = show . length . filter ( (==) Male . gender) 

eldest :: AddressBook -> String
eldest book = 
    firstName entry ++ " " ++ lastName entry
    where
        entry = minimumBy (comparing date) book
    

ageDifference :: AddressBook -> Maybe Integer
ageDifference book = 
    diffDays <$>(dateFor "Paul") <*> (dateFor "Bill") 
    where
        dateFor name = date <$> ( find ((==)name . firstName) book )

ageDifferenceString:: AddressBook -> String
ageDifferenceString book = case ageDifference book of 
                            Just n ->show n
                            Nothing -> "Look up failed"

questions :: [AddressBook -> String]
questions = [countMales, eldest, ageDifferenceString]

