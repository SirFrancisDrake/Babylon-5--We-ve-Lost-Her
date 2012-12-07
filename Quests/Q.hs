
module Q where

type Quest =  Zipper State

type State = String
type Pattern = String

data QState = QState
    { qs_id :: String
    , qs_descr :: String
    , qs_descrRet :: String
    , qs_options :: [QOption]
    }

data QOption = QOption
    { qo_verbosity :: Verbosity
    , qo_patterns :: [String]
    , qo_success :: GlobalV -> Bool
    , qo_successState :: String
    , qo_failState :: String
    }

data Verbosity = Verbose String
               | Silent

questMe :: Quest -> Quest
questMe q = questMe

data Zipper a = Zipper
  { zip_current :: a
  , zip_others :: [a]
  }
  deriving (Eq, Show)

toList :: Zipper a -> [a]
toList (Zipper z zs) = z:zs

selectBy :: Zipper a -> (a -> Bool) -> Zipper a
selectBy (Zipper z zs) f
    | length (filter f (z:zs)) > 1 = error "FA selectBy: more than one match"
    | length (filter f (z:zs)) == 0 = error "FA selectBy: state not found"
    | otherwise = 
        let t = head $ filter f (z:zs)
        in Zipper t (filter (not . f) (z:zs))

select :: (Eq a) => Zipper a -> a -> Zipper a
select z a = selectBy z (==a)
