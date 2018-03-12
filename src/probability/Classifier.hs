module Probability.Classifier
where

data Class = Positive | Negative deriving (Eq, Show, Ord)

doc1 :: String
doc1 = "I loved the movie"

doc2 :: String
doc2 = "I hated the movie"

doc3 = "a great movie. good movie"

doc4 :: String
doc4 = "poor acting"

doc5 = "great acting. a good movie"

docs :: [String]
docs = [doc1, doc2, doc3, doc4, doc5]

correspondingClasses :: [Class]
correspondingClasses = [Positive, Negative, Positive, Negative, Positive]

classifiedDocs :: [(String, Class)]
classifiedDocs = zip docs correspondingClasses
