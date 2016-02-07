module Types where

import qualified Data.ByteString.Lazy as B


type Sentence = B.ByteString
type Sentences = [Sentence]

type Word = B.ByteString
type Words = [Types.Word]
type WordPair = (String,[String])


