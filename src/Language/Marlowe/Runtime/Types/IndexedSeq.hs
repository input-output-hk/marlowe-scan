{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module Language.Marlowe.Runtime.Types.IndexedSeq (Indexed(..), IndexedSeq, cons, drop, findMatchingTail, fromFoldable, empty, length, toSeq) where

import Data.Sequence (Seq)
import Data.Map (Map)
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Foldable (Foldable(toList))
import Prelude hiding (drop, length)
import qualified Data.List as List

class Indexed a b where
    getIdentifier :: a -> b

data IndexedSeq a b =
    IndexedSeq {
        innerSeq :: Seq a
      , index :: [Map b Int]
    }
  deriving (Eq)

instance Show a => Show (IndexedSeq a b) where
  showsPrec :: Int -> IndexedSeq a b -> ShowS
  showsPrec p xs = showParen (p > 10) $
        showString "fromFoldable " . shows (toList $ toSeq xs)

empty :: IndexedSeq a b
empty = IndexedSeq {
            innerSeq = Seq.empty
          , index = [Map.empty]
        }

cons :: (Ord b, Indexed a b) => a -> IndexedSeq a b -> IndexedSeq a b
cons x IndexedSeq {
           innerSeq = srcInnerSeq
         , index = (srcIndex:rest)
         } = IndexedSeq {
                 innerSeq = Seq.singleton x <> srcInnerSeq
               , index = insertUnique (getIdentifier x) (Seq.length srcInnerSeq) srcIndex : srcIndex : rest
               }
  where
    insertUnique :: Ord k => k -> a -> Map k a -> Map k a
    insertUnique = Map.insertWithKey (\_ _ _ -> error "Duplicate identifier")
cons _ IndexedSeq {
           innerSeq = _
         , index = []
         } = error "IndexedSeq in inconsistent state"

fromFoldable :: (Ord b, Indexed a b, Foldable f) => f a -> IndexedSeq a b
fromFoldable = foldr cons empty

toSeq :: IndexedSeq a b -> Seq a
toSeq = innerSeq

drop :: Int -> IndexedSeq a b -> IndexedSeq a b
drop n IndexedSeq {
         innerSeq = srcInnerSeq
       , index = srcIndexes
       } = IndexedSeq {
             innerSeq = Seq.drop n srcInnerSeq
           , index = List.drop n srcIndexes
           }

findMatchingTail :: (Ord b, Indexed a b) => a -> IndexedSeq a b -> Maybe (IndexedSeq a b)
findMatchingTail v is@IndexedSeq {
                        innerSeq = srcInnerSeq
                      , index = (srcIndex:_)
                      } = case Map.lookup (getIdentifier v) srcIndex of
                            Nothing -> Nothing
                            Just pos -> Just $ drop (Seq.length srcInnerSeq - pos - 1) is
findMatchingTail _ IndexedSeq { index = [] } = error "IndexedSeq in inconsistent state"

length :: IndexedSeq a b -> Int
length IndexedSeq {
         innerSeq = srcInnerSeq
       } = Seq.length srcInnerSeq
