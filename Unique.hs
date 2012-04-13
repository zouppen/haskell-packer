module Unique where

import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State

data Name = Name { fromModule   :: String
                 , originalName :: String
                   -- Not very informative.
                 } deriving (Show)

data UniqueData = UniqueData { nameMap  :: Map String Name
                             , reserved :: Set String
                             } deriving (Show)

type UniqueState = State UniqueData

possibleUnique :: String -> [String]
possibleUnique n = n : map ((n++) . show) [2..]

-- |Filters elemets from array which are not in a given set. Useful in
-- |filtering out already reserved names.
setFilter :: (Ord a) => Set a -> [a] -> [a]
setFilter set = filter (flip S.notMember set) 

-- |Picks a free name by following the naming rules given in
-- |possibleUnique function.
takeUnique :: Set String -> String -> String
takeUnique reserved name = head $ setFilter reserved $ possibleUnique name

rename :: Name -> UniqueState String
rename name = do
  res <- gets reserved
  let uniq = takeUnique res $ originalName name
  addReserved uniq
  addReplacement uniq name
  return uniq

-- |Adds the current reserved word to the state.
addReserved :: String -> UniqueState ()
addReserved e = modify $ \x -> x{ reserved = S.insert e (reserved x) }

-- |Adds replacement information to the state.
addReplacement :: String -> Name -> UniqueState ()
addReplacement e name = modify $ \x -> x{ nameMap = M.insert e name (nameMap x) }

-- Some debug helpers, not actually needed.

toDummyNames :: [String] -> [Name]
toDummyNames = map (Name "dummy")

emptyState :: UniqueData
emptyState = UniqueData M.empty S.empty

tryMe :: [String] -> ([String], UniqueData)
tryMe xs = runState (mapM rename $ toDummyNames xs) emptyState