module Character where

data Character = Character
        { char_name :: String
        , char_money :: Int
        }
    deriving (Show)

defChar = Character "Jabs" 9001
