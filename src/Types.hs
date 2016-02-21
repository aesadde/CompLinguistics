module Types where

import Data.Map(Map)

type Sentence       = [String]
type BiProbMap      = Map (String,String) Float
type WTProbMap      = Map (String,String) Float
type TagsMap        = Map String Int
type TaggedSentence = [(String,String)]
type Scores         = Map (String,String) Float
type BackTrack      = Map (String,String) String
