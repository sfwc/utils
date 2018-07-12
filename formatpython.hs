{-# LANGUAGE OverloadedStrings #-}

import Language.Python.Version3.Parser (parseModule)
import Language.Python.Common.Pretty (prettyText)
import Language.Python.Common.PrettyAST ()


main :: IO ()
main = interact reformat
  where
    reformat input =
        unlines $
            map (replicate minDepth ' ' ++) $
                case parseModule (unlines content) "" of
                    Left error -> ("# " ++ show error) : content
                    Right (statements, _) -> lines (prettyText statements)
      where
        content = map (drop minDepth) $ lines input
        minDepth =
            case map depth $ lines input of
                [] -> 0
                n : ns -> foldl min n ns
        depth = length . takeWhile (== ' ')

