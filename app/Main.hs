import Data.Char (generalCategory, isAlpha, isAlphaNum, isPunctuation, isSymbol)
import Data.List (find, isInfixOf)

data DataOrang = Empty | DataOrang {nama :: String, umur :: Int, alamat :: String, suhu :: Int, gejala :: String, bertemu_dgn_positif :: Bool, sudah_vaksin :: Bool, status :: String} deriving (Show)

data List t = E | C t (List t)

checkIsEmpty :: List t -> Maybe t
checkIsEmpty E = Nothing
checkIsEmpty (C x _) = Just x

getPassPhrase :: IO (Maybe String)
getPassPhrase = do
  s <- getLine
  if isValid s
    then return $ Just s
    else return Nothing

specialCharacter = or . map (\x -> if isPunctuation x || isSymbol x then True else False)

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

askUserIDPass params = do
  putStrLn "Masukkan User ID kamu"
  user_id <- getLine
  putStrLn "Masukkan password minimal 8 character dengan tambahan minimal 1 symbol"
  maybe_value <- getPassPhrase
  case params of
    "login" -> do
      login user_id maybe_value
    "register" -> do
      register user_id maybe_value

login user_id maybe_value = do
  text <- readFile "listuserpass.txt"
  let list = lines text
  let filtered = find (\x -> isInfixOf user_id x) list
  case filtered of
    Just value -> do
      case maybe_value of
        Just value_password -> do
          print value_password
          let a = words value
          let old_password = a !! 2
          if value_password == old_password
            then do
              dataMenu
            else do
              putStrLn "User ID atau password salah, silahkan coba lagi atau daftar"
              mainMenu
        Nothing -> do
          putStrLn "User ID atau password salah, silahkan coba lagi atau daftar"
          mainMenu
    Nothing -> do
      putStrLn "User ID atau password salah, silahkan coba lagi atau daftar"
      mainMenu

register user_id maybe_value = do
  case maybe_value of
    Just value -> do
      appendFile "listuserpass.txt" (user_id ++ " : " ++ value ++ "\n")
      putStrLn "storing in file..."
      putStrLn "Register berhasil , silahkan login"
      mainMenu
    Nothing -> do
      putStrLn "password invalid"
      mainMenu

checkValue = do
  answer <- getLine
  case answer of
    "Y" -> return True
    "N" -> return False
    _ -> return False

createData = do
  putStrLn "Masukkan Nama Maks. 12 karakter"
  name <- getLine
  putStrLn "Masukkan Umur"
  ageValue <- getLine
  let age = read ageValue :: Int
  putStrLn "Masukkan Alamat Maks. 20 karakter"
  address <- getLine
  putStrLn "Masukkan Suhu yang terdata saat ini"
  tempValue <- getLine
  let temp = read tempValue :: Int
  putStrLn "Masukkan Gejala yang dialami seperti flu, demam, pilek, sesak"
  putStrLn "Jika tidak ada, tulis \"tidak ada\""
  condition <- getLine
  putStrLn "Apakah pernah berpapasan dengan seseorang yang diduga positif? (Y/N)"
  isMeetPositive <- checkValue
  putStrLn "Apakah sudah vaksin sebelumnya? (Y/N)"
  isVaksin <- checkValue
  putStrLn "Hasil PCR/antigen tiga hari terakhir? (positif/negatif)"
  pcrResult <- getLine
  let newData = DataOrang {nama = name, umur = age, alamat = address, suhu = temp, gejala = condition, bertemu_dgn_positif = isMeetPositive, sudah_vaksin = isVaksin, status = pcrResult}
  appendFile "listKelurahan.txt" (show newData ++ "\n")
  print newData

dataMenu = do
  putStrLn "Sukses masuk, silahkan pilih menu dibawah ini"
  putStrLn "Buat data baru ('C'), Baca data ('R'), Edit data ('U'), Hapus data ('C')"
  pilihanMenu <- getLine
  case pilihanMenu of
    "C" -> do
      createData
    "R" -> do
      askUserIDPass "register"
    "U" -> do
      putStrLn "Terima kasih telah menggunakan aplikasi ini"
    "D" -> do
      putStrLn "Terima kasih telah menggunakan aplikasi ini"
    _ -> do
      putStrLn "Masukkan kode dengan benar"
      dataMenu

mainMenu = do
  putStrLn "Silahkan Login ('L'), Register ('R'), atau keluar ('Q')"
  pilihanMenu <- getLine
  case pilihanMenu of
    "L" -> do
      askUserIDPass "login"
    "R" -> do
      askUserIDPass "register"
    "Q" -> do
      putStrLn "Terima kasih telah menggunakan aplikasi ini"
    _ -> do
      putStrLn "Masukkan kode dengan benar"
      mainMenu

main = do
  putStrLn "Selamat Datang di Data Covid kelurahan Melong"
  mainMenu