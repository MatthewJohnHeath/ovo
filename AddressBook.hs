
module AddressBook (Gender, AddressBookEntry, AddressBook) where
import Data.Time

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

data AddressBook = List AddressBookEntry

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

-- parseLine :: String ->  Maybe AddressBookEntry
	-- AddressBook <*> values!!0 


-- parseAddressBook :: String -> AddressBook 
-- parseAddressBook file