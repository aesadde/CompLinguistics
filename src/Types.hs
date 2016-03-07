module Types where

import Data.Map(Map)

type Sentence = [String]
type BiProbMap= Map (String,String) Double
type WTProbMap = Map (String,String) Double
type TagsMap = Map String Int
type TaggedSentence = [(String,String)]
type Scores = Map (String,String) Double
type BackTrack = Map (String,String) String
