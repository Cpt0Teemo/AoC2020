import Data.List
import Data.Maybe
import Data.Char

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
		byr= Just byr, 
		iyr= Just iyr, 
		eyr= Just eyr, 
		hgt= Just hgt, 
		hcl= Just hcl, 
		ecl= Just ecl, 
		pid= Just pid, 
		cid= _ }
		| all (==True) [validateByr byr, validateIyr iyr, validateEyr eyr, validateHgt hgt, validateHcl hcl, validateEcl ecl, validatePid pid]

	= True
validatePassport _ = False

validateByr year = length year == 4 && all isDigit year && (1920 <= read year) && (read year <= 2002)
validateIyr year = length year == 4 && all isDigit year && (2010 <= read year) && (read year <= 2020)
validateEyr year = length year == 4 && all isDigit year && (2020 <= read year) && (read year <= 2030)
validateHgt height = ((dropWhile isDigit height) == "cm" && 150 <= (read number) && (read number) <= 193)
					|| ((dropWhile isDigit height) == "in" && 59 <= (read number) && (read number) <= 76)
					where number = takeWhile isDigit height
validateHcl color = head color == '#' && all (\ x -> isDigit x || isBetweenAF x) (tail color)
					where isBetweenAF letter = (ord 'a') <= (ord letter) && (ord letter) <= (ord 'f')
validateEcl color = any (==color) ["amb","blu","brn","gry","grn","hzl","oth"]
validatePid pid = all isDigit pid && length (filter isDigit pid) == 9


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