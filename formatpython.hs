import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Language.Python.Common.AST
import Language.Python.Common.Pretty (prettyText)
import Language.Python.Common.PrettyAST ()
import Language.Python.Common.SrcLocation (SrcSpan)
import Language.Python.Version3.Parser (parseModule)
import Safe
import System.Environment (getArgs)


type Output = [String] -- A list of lines
type Choices = [Output]


renderStmt :: Statement SrcSpan -> Choices
renderStmt (Assign (target : moreTargets) value annotation) = do
    targetChoice <- renderExpr target
    valueChoice <-
        if moreTargets == [] then renderExpr value
        else renderStmt $ Assign moreTargets value annotation

    let singleline = suffix (" = " ++ concat valueChoice) targetChoice
    let multiline = suffix " =\\" targetChoice ++ indent valueChoice
    if colinear valueChoice then [singleline, multiline] else [multiline]

renderStmt (Return (Just expr) _) = map (prefix "return ") $ renderExpr expr
renderStmt (StmtExpr expr _) = renderExpr expr
renderStmt stmt = [lines $ prettyText stmt]


colinear :: Output -> Bool
colinear = (== 1) . length


prefix :: String -> Output -> Output
prefix _ [] = []
prefix s (l : ls) = (s ++ l) : ls


suffix :: String -> Output -> Output
suffix _ [] = []
suffix s [l] = [l ++ s]
suffix s (l : ls) = l : suffix s ls


indent :: Output -> Output
indent = map ("    " ++)


renderExpr :: Expr SrcSpan -> Choices
renderExpr (List exprs@(_ : _) _) = do
    choice <- choices exprs
    let singleline = ["[" ++ intercalate ", " (concat choice)  ++ "]"]
    let multiline = ["["] ++ concatMap (suffix "," . indent) choice ++ ["]"]
    if all colinear choice then [singleline, multiline] else [multiline]
  where
    choices [] = [[]]
    choices (expr : exprs) =
        [first : rest | first <- renderExpr expr, rest <- choices exprs]

renderExpr expr = [lines $ prettyText expr]


choose :: Output -> Choices -> Output
choose defaultOutput = bestFit . sortBy (comparing length)
  where
    bestFit [] = defaultOutput
    bestFit choices@(shortest : _) =
        headDef shortest $ filter ((< 80) . maximumDef 0 . map length) choices


main :: IO ()
main = do
    args <- getArgs
    interact $ reformat $ elem "--debug" args
  where
    reformat debugging input =
        unlines $
            map (replicate minDepth ' ' ++) $
                case parseModule (unlines content) "" of
                    Left error -> ("# " ++ show error) : content
                    Right (Module (stmt : _), _) ->
                        if debugging then
                            ("# " ++ show stmt)
                                : intercalate ["###"]
                                    (map (map ("# " ++)) $ renderStmt stmt)
                                ++ content
                        else
                            choose (lines $ prettyText stmt) $ renderStmt stmt
                    Right (stmts, _) -> lines $ prettyText stmts
      where
        content = map (drop minDepth) $ lines input
        minDepth = minimumDef 0 $ map depth $ lines input
        depth = length . takeWhile (== ' ')
