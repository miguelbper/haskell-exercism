module WordCount (wordCount) where

import Data.Text as T (Text, toLower, split, null, empty, head, last, tail, init)
import Data.MultiSet as M (fromList, toOccurList)

wordCount :: Text -> [(Text, Int)]
wordCount = M.toOccurList 
          . M.fromList
          . map removeBorderApostrophes
          . filter (not . T.null) 
          . T.split (`elem` punctuation) 
          . T.toLower
    where
        punctuation = " \n\t,;.:-_!?~^´`&@$%\""

        removeBorderApostrophes :: Text -> Text
        removeBorderApostrophes = removeFirstApostrophe . removeLastApostrophe
            where
                removeFirstApostrophe :: Text -> Text
                removeFirstApostrophe xs
                    | T.null xs         = T.empty
                    | T.head xs == '\'' = T.tail xs
                    | otherwise         = xs

                removeLastApostrophe :: Text -> Text
                removeLastApostrophe xs
                    | T.null xs         = T.empty
                    | T.last xs == '\'' = T.init xs
                    | otherwise         = xs
