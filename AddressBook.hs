import Data.Time
module AddressBook (Gender, AddressBookEntry, AddressBook) where


data Gender = 
	Male |Female
	deriving(Eq)

data  AddressBookEntry = AddressBookEntry
	{
		firstName	:: String,
		lastName	:: String,
		gender		:: Gender,
		date		:: DateTime
	} deriving(Eq)

data AddressBook = [AddressBookEntry]

