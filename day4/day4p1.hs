import Data.List
import Data.Maybe

data Passport = 
	Passport {
		byr :: Maybe String,
		iyr :: Maybe String,
		eyr :: Maybe String,
		hgt :: Maybe String,
		hcl :: Maybe String,
		ecl :: Maybe String,
		pid :: Maybe String,
		cid :: Maybe String
	}
		deriving(Eq, Show, Read)

main :: IO()
main = do
	input <- readFile "input.txt"
	print . length . (filter (validatePassport)) . map (parsePassport) $ getGroupsOfStrings (lines input) [] [[]]

parsePassport :: [String] -> Passport
parsePassport pairs =
	Passport {
		byr= getValueFromKey "byr" pairs,
		iyr= getValueFromKey "iyr" pairs,
		eyr= getValueFromKey "eyr" pairs,
		hgt= getValueFromKey "hgt" pairs,
		hcl= getValueFromKey "hcl" pairs,
		ecl= getValueFromKey "ecl" pairs,
		pid= getValueFromKey "pid" pairs,
		cid= getValueFromKey "cid" pairs
	}

validatePassport :: Passport -> Bool
validatePassport Passport {
		byr= Just _, 
		iyr= Just _, 
		eyr= Just _, 
		hgt= Just _, 
		hcl= Just _, 
		ecl= Just _, 
		pid= Just _, 
		cid= _ }
	= True
validatePassport _ = False

getValueFromKey :: String -> [String] -> Maybe String
getValueFromKey key values =
	if pairs == [] then
		Nothing
	else
		Just (tail (head pairs))
	where 
		pairs = (catMaybes . map (stripPrefix key)) values

getGroupsOfStrings :: [String] -> [String] -> [[String]] -> [[String]]
getGroupsOfStrings [] _ acc = acc
getGroupsOfStrings ("":xs) curr acc = getGroupsOfStrings xs [] (curr:acc)
getGroupsOfStrings (x:xs) curr acc = getGroupsOfStrings xs ((words x)++curr) acc