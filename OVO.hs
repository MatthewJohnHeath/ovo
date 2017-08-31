import AddressBook
import Questions

main = do
	addressBook <- readAddressBook "addressBook.txt"
	mapM_ print$ 	
	map (\ (n, f) ->  (show n ++ ". " ++ f addressBook) ) $
	zip [1..] questions 

 

	   


	