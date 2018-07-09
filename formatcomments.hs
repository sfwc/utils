{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Loops (untilM)
import System.IO (isEOF)


data Block = Comment T.Text [T.Text] | Ignored T.Text

lineToBlock :: T.Text -> Block
lineToBlock text
    | marker == "" || T.length prefix > 74 = Ignored text
    | otherwise = Comment prefix $ T.words content
  where
    (indent, comment) = T.span (== ' ') text
    (marker, content) = T.span (== '#') comment
    prefix = T.append indent marker

merge :: [Block] -> [Block]
merge (Comment prefix words : Comment nextPrefix moreWords : rest)
    | prefix == nextPrefix = merge $ Comment prefix (words ++ moreWords) : rest
merge (block : rest) = block : merge rest
merge [] = []

blockToLines :: Block -> [T.Text]
blockToLines (Ignored text) = [text]
blockToLines (Comment prefix words) =
    reverse $ foldl linesFromWords [prefix] words
  where
    linesFromWords (line : lines) word
        | T.length line + 1 + T.length word > 79 = prefix : line : lines
        | otherwise = T.concat [line, " ", word] : lines

main :: IO ()
main =
    untilM TIO.getLine isEOF >>=
      mapM_ TIO.putStrLn . concatMap blockToLines . merge . map lineToBlock
