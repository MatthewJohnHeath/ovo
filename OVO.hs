import AddressBook
import Questions

main = do
    addressBook <- readAddressBook "addressBook.txt"
    putStrLn ("1. " ++ countMales addressBook)
    putStrLn ("2. " ++ eldest addressBook)
    putStrLn ("3. " ++ ageDifferenceString addressBook)