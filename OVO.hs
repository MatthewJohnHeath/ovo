import AddressBook
import Questions

main = do
    addressBook <- readAddressBook "addressBook.txt"
    putStrLn (countMales addressBook)
    putStrLn (eldest addressBook)
    putStrLn (ageDifferenceString addressBook)