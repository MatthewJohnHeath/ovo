module AddressBook (Gender, AddressBookEntry) where
import Data.Time
import Data.Maybe
import Data.List.Split

data Gender = 
	Male |Female
	deriving(Eq)

data  AddressBookEntry = AddressBookEntry
	{
		firstName	:: String,
		lastName	:: String,
		gender		:: Gender,
		date		:: Day
	} deriving(Eq)



parseGender :: String -> Maybe Gender
parseGender "Male" = Just Male
parseGender "Female" = Just Female
parseGender _ = Nothing

parseDay :: String -> Maybe Day
parseDay  = parseTime defaultTimeLocale "%d%m%Y" 

fillVals :: [String] -> Maybe AddressBookEntry
fillVals [fn, ln, gen, d] =
	AddressBookEntry fn ln <$> (parseGender gen) <*> (parseDay d)
fillVals _ = Nothing	

parseLine :: String ->  Maybe AddressBookEntry
parseLine = fillVals . (splitOn ",")

parseAddressBook :: String -> [AddressBookEntry] 
parseAddressBook string = catMaybes (map parseLine (lines string ) ) 