module Journal where

data JournalEntry = JournalEntry
    { j_id :: String
    , j_qid :: TVar Quest
    , j_text :: String
    }
    deriving (Show)

type Journal = [JournalEntry]

instance Eq Journal where
    (==) = on (==) j_id

add :: Journal -> TVar Quest -> String -> String -> Journal
add j qid id text
