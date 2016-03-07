module Types where

import Data.Map.Strict (Map)
import Prelude hiding (Word)

type Word     = String
type Tag      = String
type Sentence = [String]
type BiProbMap= Map (Tag,Tag) Double
type WTProbMap = Map (Word,String) Double
type TagsMap = Map Tag Int
type TaggedSentence = [(Word,Tag)]
type Scores = Map (Tag,Word) Double
type BackTrack = Map (Tag,Word) String
