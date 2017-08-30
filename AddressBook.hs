module AddressBook (Gender(..), AddressBookEntry(..), AddressBook,  readAddressBook) where
import Data.Time
import Data.Maybe
import Data.List.Split
import System.Environment

--Type for gender. TODO? extended with "NotStated" and possibly "Other(String)"
data Gender = 
    Male |Female
    deriving(Eq)

--Type representing the data in a line of the address book file. TODO? handle missing data
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
parseGender "Male" = Just Male
parseGender "Female" = Just Female
parseGender _ = Nothing

--If string parses as date retuens Just date. Else returns nothing. TODO replace deprecated parsing function
parseDay :: String -> Maybe Day
parseDay  = parseTime defaultTimeLocale "%d%m%Y" 

--Turns strings for the fields values into an address book entry
fillVals :: [String] -> Maybe AddressBookEntry
fillVals [fn, ln, gen, d] =
    AddressBookEntry fn ln <$> (parseGender gen) <*> (parseDay d)
fillVals _ = Nothing    

--Turns line of an address book file into an AddressBookEntry
parseLine :: String ->  Maybe AddressBookEntry
parseLine = fillVals . (splitOn ",")

--Turns content of address book file into an AddressBook
parseAddressBook :: String -> AddressBook 
parseAddressBook string = catMaybes (map parseLine (lines string ) ) 


readAddressBook :: FilePath -> IO AddressBook
readAddressBook  file = parseAddressBook <$> readFile file