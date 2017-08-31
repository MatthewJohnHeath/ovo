module Questions (countMales, eldest,  ageDifferenceString) where
import AddressBook
import Data.Time
import Data.List
import Data.Ord
--Counts males
countMales :: AddressBook -> String
countMales = show . length . filter ( (==) Male . gender) 
--returns eldest
eldest :: AddressBook -> String
eldest book = 
    firstName entry ++ " " ++ lastName entry
    where
        entry = minimumBy (comparing date) book
    
--Will return Just DIFFERENCE where DIFFERENCE is difference in age between *a* Bill and *a* Paul if there is at least on of each. Else returns Nthing
ageDifference :: AddressBook -> Maybe Integer
ageDifference book = 
    diffDays <$>(dateFor "Paul") <*> (dateFor "Bill") 
    where
        dateFor name = date <$> ( find ((==)name . firstName) book )
--Converts ageDifference to string else complains
ageDifferenceString:: AddressBook -> String
ageDifferenceString book = case ageDifference book of 
                            Just n ->show n
                            Nothing -> "Look up failed"


