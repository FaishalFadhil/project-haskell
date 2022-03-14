import Control.Concurrent
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Char (generalCategory, isAlpha, isAlphaNum, isPunctuation, isSymbol)
import Data.Data (typeOf)
import Data.Functor.Identity (Identity (runIdentity))
import Data.List (elemIndex, find, intersperse, isInfixOf)
import Data.List.Split
import GHC.IO.Handle
import System.Directory
import System.IO
import Text.Read (lift, readMaybe)

data PersonData = Empty | PersonData {name :: String, age :: Int, address :: String, temperature :: Int, symptoms :: String, isMeetPositive :: Bool, vaccinated :: Bool, pcrResult :: String} deriving (Show, Eq)

data List t = E | C t (List t)

getUser = do
  user <- readFile "loginUser.txt"
  return user

checkIsEmpty :: List a -> Maybe a
checkIsEmpty E = Nothing
checkIsEmpty (C x _) = Just x

getPassPhrase :: MaybeT IO String
getPassPhrase = MaybeT $ do
  putStrLn "Input Your password with minimum 8 characters and 1 special symbol"
  s <- getLine
  putStrLn ""
  if isValid s
    then return $ Just s
    else return Nothing

specialCharacter :: [Char] -> Bool
specialCharacter = or . map (\x -> if isPunctuation x || isSymbol x then True else False)

alphaNum :: [Char] -> Bool
alphaNum = or . map (\x -> isAlphaNum x)

isValid :: String -> Bool
isValid s =
  if length s >= 8
    then
      if alphaNum s
        then
          if specialCharacter s
            then True
            else False
        else False
    else False

checkUser :: MaybeT IO String
checkUser = MaybeT $ do
  putStrLn "Input your User ID with minimum 5 characters"
  value <- getLine
  putStrLn ""
  if length value > 4
    then return $ Just value
    else return Nothing

askUserIDPass :: String -> IO ()
askUserIDPass params = do
  maybeInput <- runMaybeT $ do
    user_id <- checkUser
    password <- getPassPhrase
    return (user_id, password)
  case maybeInput of
    Nothing -> do
      putStrLn "User ID or password false, please try again or register\n"
      askUserIDPass params
    Just (user_id, password) -> do
      case params of
        "login" -> do
          login user_id password
        "register" -> do
          register user_id password

login :: [Char] -> String -> IO ()
login user_id maybe_value = do
  fileOpen <- openFile "listuserpass.txt" ReadMode
  threadDelay 500000
  text <- hGetContents fileOpen
  let list = lines text
  let filtered = find (\x -> isInfixOf user_id x) list
  case filtered of
    Just value -> do
      let a = words value
      let admin = a !! 0
      let old_password = a !! 2
      if maybe_value == old_password
        then do
          writeFile "loginUser.txt" admin
          putStrLn "Success login\n"
          hClose fileOpen
          dataMenu
        else do
          putStrLn "User ID or password false, please try again or register\n"
          hClose fileOpen
          mainMenu
    Nothing -> do
      putStrLn "User ID or password false, please try again or register\n"
      hClose fileOpen
      mainMenu

register :: [Char] -> [Char] -> IO ()
register user_id maybe_value = do
  fileOpen <- openFile "listuserpass.txt" AppendMode
  threadDelay 500000
  hPutStrLn fileOpen (user_id ++ " : " ++ maybe_value)
  putStrLn "storing in file..."
  putStrLn "Register success, you can login now"
  hClose fileOpen
  mainMenu

checkValue :: IO Bool
checkValue = do
  answer <- getLine
  case answer of
    "Y" -> return True
    "N" -> return False
    _ -> do
      putStrLn "Please enter the appropriate input"
      checkValue

checkChar :: Int -> IO String
checkChar num = do
  answer <- getLine
  if length answer > num
    then do
      putStrLn "Please retype and no longer than 15 characters"
      checkChar num
    else return answer

checkNum :: IO Int
checkNum = do
  answer <- getLine
  case (readMaybe answer :: Maybe Int) of
    Just num -> return num
    Nothing -> do
      putStrLn "Input with number"
      checkNum

createData :: IO ()
createData = do
  putStrLn "Input name max 15 characters"
  name <- checkChar 15
  putStrLn ""
  fileOpen <- openFile "covidData.txt" ReadMode
  threadDelay 500000
  text <- hGetContents fileOpen
  let list = lines text
  let filtered = find (\x -> isInfixOf name x) list
  case filtered of
    Just value -> do
      putStrLn $ "the name " ++ name ++ " is taken, please try again\n"
      createData
    Nothing -> do
      hClose fileOpen
      putStrLn "Input age"
      age <- checkNum
      putStrLn ""
      putStrLn "Input address max 20 characters"
      address <- checkChar 20
      putStrLn ""
      putStrLn "Input his/her last recorded body temperature"
      temperature <- checkNum
      putStrLn ""
      putStrLn "Enter the symptoms experienced such as flu, fever, runny nose, shortness of breath max 30 characters\nIf it doesn't exist, write \"none\""
      symptoms <- checkChar 30
      putStrLn ""
      putStrLn "Have you ever run into someone who is suspected to be positive? (Y/N)"
      isMeetPositive <- checkValue
      putStrLn ""
      putStrLn "Have you been vaccinated before? (Y/N)"
      vaccinated <- checkValue
      putStrLn ""
      putStrLn "PCR/antigen results for the last three days (positive/negative)"
      pcrResult <- getLine
      putStrLn ""
      fileAppend <- openFile "covidData.txt" AppendMode
      threadDelay 500000
      let newData = PersonData {name = name, age = age, address = address, temperature = temperature, symptoms = symptoms, isMeetPositive = isMeetPositive, vaccinated = vaccinated, pcrResult = pcrResult}
      let fileData = name ++ "," ++ show age ++ "," ++ address ++ "," ++ show temperature ++ "," ++ symptoms ++ "," ++ show isMeetPositive ++ "," ++ show vaccinated ++ "," ++ pcrResult
      hPutStrLn fileAppend fileData
      putStrLn "New data created"
      print newData
      putStrLn ""
      hClose fileAppend
      settingLog "created" $ show newData

printHeadTable :: IO ()
printHeadTable = do
  putStrLn "--------------------------------------------------------------------------------------------------------------------------------------------------------"
  putStrLn "||     Name       ||  age   ||      address        ||  Temperature   ||           symptoms            || isMeetPositive  || vaccinated  || pcrResult  ||"
  putStrLn "--------------------------------------------------------------------------------------------------------------------------------------------------------"

printRowTable :: IO ()
printRowTable = do
  fileOpen <- openFile "covidData.txt" ReadMode
  threadDelay 500000
  text <- hGetContents fileOpen
  let line = tail $ lines text
  mapM_ (selectRowTable) line
  hClose fileOpen
  printBorderTable
  settingLog "read" $ "total " ++ show (length line) ++ " data"

selectRowTable :: [Char] -> IO ()
selectRowTable x = do
  let part = splitOn "," x
  printValue part

printWord :: Monad m => Int -> [Char] -> m [Char]
printWord num value = return $ " " ++ value ++ concat (replicate (num - length value) " ") ++ "||"

printValue :: [[Char]] -> IO ()
printValue [a, s, d, f, g, h, j, k] = do
  name <- printWord 15 a
  age <- printWord 7 s
  address <- printWord 20 d
  temp <- printWord 15 f
  symptoms <- printWord 30 g
  isMeet <- printWord 16 h
  vaccinated <- printWord 12 j
  status <- printWord 11 k
  putStrLn $ "||" ++ name ++ age ++ address ++ temp ++ symptoms ++ isMeet ++ vaccinated ++ status

printBorderTable :: IO ()
printBorderTable = do
  putStrLn "--------------------------------------------------------------------------------------------------------------------------------------------------------\n"

pickData :: Monad m => [String] -> m PersonData
pickData [a, s, d, f, g, h, j, k] = return PersonData {name = a, age = (read s :: Int), address = d, temperature = (read f :: Int), symptoms = g, isMeetPositive = (read h :: Bool), vaccinated = (read j :: Bool), pcrResult = k}

getName :: PersonData -> String
getName (PersonData {name = x}) = x

getAge :: PersonData -> Int
getAge (PersonData {age = x}) = x

getAddress :: PersonData -> String
getAddress (PersonData {address = x}) = x

getTemperature :: PersonData -> Int
getTemperature (PersonData {temperature = x}) = x

getSymptoms :: PersonData -> String
getSymptoms (PersonData {symptoms = x}) = x

getIsMeetPositive :: PersonData -> Bool
getIsMeetPositive (PersonData {isMeetPositive = x}) = x

getVaccinated :: PersonData -> Bool
getVaccinated (PersonData {vaccinated = x}) = x

getPcrResult :: PersonData -> String
getPcrResult (PersonData {pcrResult = x}) = x

saveData :: PersonData -> [Char] -> IO ()
saveData newData targetName = do
  fileOpen <- openFile "covidData.txt" ReadMode
  text <- hGetContents fileOpen
  let theHead = (head $ lines text) ++ "\n"
  let list = tail $ lines text
  let filtered = filter (\x -> (isInfixOf targetName x) == False) list
  let newLine = ((getName newData) ++ "," ++ show (getAge newData) ++ "," ++ (getAddress newData) ++ "," ++ show (getTemperature newData) ++ "," ++ (getSymptoms newData) ++ "," ++ show (getIsMeetPositive newData) ++ "," ++ show (getVaccinated newData) ++ "," ++ (getPcrResult newData))
  let newList = filtered ++ [newLine]
  let newInput = theHead ++ concat (intersperse "\n" newList)
  when (length newInput > 0) $
    finalizeUpdate newInput newData

finalizeUpdate :: Show a => String -> a -> IO ()
finalizeUpdate newInput newData = do
  fileOpen <- openFile "covidData.txt" WriteMode
  threadDelay 500000
  hPutStrLn fileOpen newInput
  putStrLn "Success update Data\n"
  hClose fileOpen
  settingLog "updated" $ show newData

menuEdit :: [Char] -> [Char] -> IO ()
menuEdit value targetName = do
  let part = splitOn "," value
  getData <- pickData part
  putStrLn "select data will be update:\n\"N\" = Name, \"AG\" = Age, \"AD\" = Address, \"T\" = Temperature, \"SY\" = Symptoms, \"IM\" = isMeetPositive, \"V\" = Vaccinated, \"ST\" = Status"
  selectMenu <- getLine
  putStrLn ""
  case selectMenu of
    "N" -> do
      putStrLn "Input new name max 15 characters"
      name <- checkChar 15
      fileOpen <- openFile "covidData.txt" ReadMode
      text <- hGetContents fileOpen
      let list = lines text
      let filtered = find (\x -> isInfixOf name x) list
      case filtered of
        Just newValue -> do
          putStrLn $ "the name " ++ name ++ " is taken, please try again\n"
          menuEdit value targetName
        Nothing -> do
          putStrLn "Editing in proccess\n"
          let newData = getData {name = name}
          saveData newData targetName
    "AG" -> do
      putStrLn "Input new age"
      age <- checkNum
      putStrLn "Editing in proccess\n"
      let newData = getData {age = age}
      saveData newData targetName
    "AD" -> do
      putStrLn "Input new address max 20 characters"
      address <- checkChar 20
      putStrLn "Editing in proccess\n"
      let newData = getData {address = address}
      saveData newData targetName
    "T" -> do
      putStrLn "Input his/her new last recorded body temperature"
      temperature <- checkNum
      putStrLn "Editing in proccess\n"
      let newData = getData {temperature = temperature}
      saveData newData targetName
    "SY" -> do
      putStrLn "Enter the new symptoms experienced such as flu, fever, runny nose, shortness of breath max 30 characters\nformat answer: \"flu-fever-runny_noe\""
      putStrLn "If it doesn't exist, write \"none\""
      symptoms <- checkChar 30
      putStrLn "Editing in proccess\n"
      let newData = getData {symptoms = symptoms}
      saveData newData targetName
    "IM" -> do
      putStrLn "Have you ever run into someone who is suspected to be positive? (Y/N)"
      isMeetPositive <- checkValue
      putStrLn "Editing in proccess\n"
      let newData = getData {isMeetPositive = isMeetPositive}
      saveData newData targetName
    "V" -> do
      putStrLn "Have you been vaccinated before? (Y/N)"
      vaccinated <- checkValue
      putStrLn "Editing in proccess\n"
      let newData = getData {vaccinated = vaccinated}
      saveData newData targetName
    "ST" -> do
      putStrLn "PCR/antigen results for the last three days (positive/negative)"
      pcrResult <- getLine
      putStrLn "Editing in proccess\n"
      let newData = getData {pcrResult = pcrResult}
      saveData newData targetName
    _ -> do
      putStrLn "Please select the right menu\n"
      menuEdit value targetName

editData :: IO ()
editData = do
  putStrLn "Input name will be update max. 15 Chararcters"
  name <- checkChar 15
  putStrLn ""
  fileOpen <- openFile "covidData.txt" ReadMode
  text <- hGetContents fileOpen
  let list = lines text
  let filtered = find (\x -> isInfixOf name x) list
  case filtered of
    Just value -> do
      threadDelay 500000
      hClose fileOpen
      menuEdit value name
    Nothing -> do
      putStrLn "There's no data, check again\n"
      threadDelay 500000
      hClose fileOpen
      dataMenu

deleteData :: IO ()
deleteData = do
  putStrLn "Input name will be delete max. 15 Chararcters\nCaution, the same name will be deleted too"
  name <- checkChar 15
  fileOpen <- openFile "covidData.txt" ReadMode
  threadDelay 500000
  text <- hGetContents fileOpen
  let theHead = (head $ lines text) ++ "\n"
  let line = tail $ lines text
  let filtered = filter (\x -> (isInfixOf name x) == False) line
  let deleted = find (\x -> isInfixOf name x) line
  let newData = theHead ++ concat (intersperse "\n" filtered)
  if (length filtered == length line)
    then do
      putStrLn "No data changed\n"
      dataMenu
      hClose fileOpen
    else do
      hClose fileOpen
      when (length newData > 0) $
        finalizeData newData deleted

finalizeData :: String -> Maybe [Char] -> IO ()
finalizeData newData (Just deleted) = do
  fileOpen <- openFile "covidData.txt" WriteMode
  let part = splitOn "," deleted
  deletedData <- pickData part
  threadDelay 500000
  hPutStrLn fileOpen newData
  putStrLn "Success delete Data\n"
  hClose fileOpen
  settingLog "deleted" $ show deletedData

action :: String -> WriterT String Identity String
action a = do
  tell $ "has done " ++ a ++ " on covidData.txt"
  return a

result :: String -> String -> WriterT String Identity String
result a b = do
  tell $ ", result: " ++ a ++ " " ++ b
  return a

writingLog :: String -> String -> String -> WriterT String Identity String
writingLog admin act res = do
  a <- action act
  b <- result act res
  return admin

settingLog :: String -> String -> IO ()
settingLog act res = do
  admin <- readFile "loginUser.txt"
  let result = runIdentity . runWriterT $ writingLog admin act res
  appendFile "saveLog.txt" $ "Log: user ( " ++ (fst result) ++ " ) => " ++ (snd result) ++ "\n"
  dataMenu

dataMenu :: IO ()
dataMenu = do
  putStrLn "Please select menu below"
  putStrLn "Create new data ('C'), Read data ('R'), Edit data ('U'), Delete data ('D'), Quit ('Q')"
  pilihanMenu <- getLine
  case pilihanMenu of
    "C" -> do
      putStrLn ""
      createData
    "R" -> do
      putStrLn ""
      printHeadTable
      printRowTable
    "U" -> do
      putStrLn ""
      editData
      dataMenu
    "D" -> do
      putStrLn ""
      deleteData
    "Q" -> do
      writeFile "loginUser.txt" ""
      putStrLn "Thank you for using this app, see you later"
    _ -> do
      putStrLn "Please input your code correctly\n"
      dataMenu

mainMenu :: IO ()
mainMenu = do
  putStrLn "Please select menu: Login ('L'), Register ('R'), atau Quit ('Q')"
  pilihanMenu <- getLine
  case pilihanMenu of
    "L" -> do
      putStrLn ""
      askUserIDPass "login"
    "R" -> do
      putStrLn ""
      askUserIDPass "register"
    "Q" -> do
      putStrLn ""
      putStrLn "Thank you for using this app, see you later"
    _ -> do
      putStrLn "Please input your code correctly"
      putStrLn ""
      mainMenu

main :: IO ()
main = do
  putStrLn "Welcome to Civil Covid Database 1.0.0."
  mainMenu
