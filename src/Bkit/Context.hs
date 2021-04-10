module Bkit.Context where

import Prelude hiding (lookup)
import Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

type Context k a = [Map.Map k a]

insert :: Ord k => k -> a -> Context k a -> Context k a
insert k v (x:xs) = Map.insert k v x : xs

member :: Ord k => k -> Context k a -> Bool
member k = any (Map.member k)

lookup :: Ord k => k -> Context k a -> Maybe a
lookup k (x:xs) = case Map.lookup k x of
			   Nothing -> lookup k xs
			   Just v -> Just v
lookup _ [] = Nothing
