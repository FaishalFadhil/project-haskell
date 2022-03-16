# Covid Database Application 1.0.0 by Haskell

Application about CRUD documentation on Covid situation for Admin role using Haskell and Cabal

## Setup

```bash
cabal build

cabal run
```

## Menu List

```text
- Opening Menu

    1. "R" - for Register new admin
    2. "L" - for Login by email and password
    3. "Q" - for Quit application

- Main Menu (after success login)

    1. "C" - for Create new Covid data
    2. "R" - for Read data list on covidData.txt
    3. "U" - for Update/Edit data from covidData.txt
    4. "D" - for Delete data from covidData.txt
    5. "Q" - for Quit menu and Logout to Opening Menu
```

## Data

```text
PersonData
  1. name           ::  String   ( Maximum 15 characters and must be unique )
  2. age            ::  Int
  3. address        ::  String ( Maximum 20 characters )
  4. temperature    ::  Int
  5. symptoms       ::  String ( Maximum 30 characters )
  6. isMeetPositive :: Bool
  7. vaccinated     :: Bool
  8. pcrResult      :: String
```

## File Involved

```text
saveLog.txt
    To save Log activity by Admin who logged in application

covidData.txt
    To save population data regarding the condition of Covid on them

listuserpass.txt
    To save admin data who can get access to this application

loginUser.txt
    To temporary saving user_id for Log activity logging
```
