module AddressBook (Gender(..), AddressBookEntry(..), AddressBook,  readAddressBook) where
import Data.Time
import Data.Time.Calendar
import Data.Maybe
import Data.List.Split
import System.Environment
import Text.Read
--Type for gender. TODO? extended with "NotStated" and possibly "Other(String)"
data Gender = 
    Male |Female
    deriving(Eq)

--Type representing the data in a line of the address book file. TODO? handle missing data, names in different formats etc...
data  AddressBookEntry = AddressBookEntry
    {
        firstName   :: String,
        lastName    :: String,
        gender      :: Gender,
        date        :: Day
    } deriving(Eq)
--Type for data in address book file
type AddressBook = [AddressBookEntry]

--Returns Just gender if string is "gender" (gender in {Male, Female})} else Nothing
parseGender :: String -> Maybe Gender
parseGender " Male" = Just Male
parseGender " Female" = Just Female
parseGender _ = Nothing

--If string parses as date retuens Just date. Else returns nothing. TODO use proper parseTimeM (had trouble with format string) 
parseDay :: String -> Maybe Day
parseDay  s = case map readMaybe (splitOn "/" s) of
			[Just d, Just m, Just y] -> fromGregorianValid ( y + 100 * century)  (fromIntegral m)  (fromIntegral d)
						where century = if y < 17 then 20 else 19 --this should vary with date?
			_ -> Nothing

--Turns strings for the fields values into an address book entry
fillVals :: [String] -> Maybe AddressBookEntry
fillVals [fnln, gen, d] =
    case words fnln of
    [fn, ln]->
        AddressBookEntry fn ln <$> (parseGender gen) <*> (parseDay d)
    _ -> Nothing
fillVals _ = Nothing    

--Turns line of an address book file into an AddressBookEntry
parseLine :: String ->  Maybe AddressBookEntry
parseLine = fillVals . (splitOn ",")

--Turns content of address book file into an AddressBook
parseAddressBook :: String -> AddressBook 
parseAddressBook string = catMaybes (map parseLine (lines string ) ) 


readAddressBook :: FilePath -> IO AddressBook
readAddressBook  file = parseAddressBook <$> readFile file