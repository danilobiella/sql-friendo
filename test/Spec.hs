import SQLParser
import System.Directory
import System.Exit
import System.FilePath.Posix
import Test.HUnit
import Test.QuickCheck
import Text.Megaparsec

main :: IO ()
main = do
  putStrLn "Running tests..."
  putStrLn "QuickCheck tests:"
  quickCheck $ forAll nameWithUnderscoreGen checkNameWithUnderscore

  filepaths <- fmap (queryDir <>) . filter (isExtensionOf "sql") <$> listDirectory queryDir
  queries <- mapM readFile filepaths
  let sqlFileTestList = shouldParseSQLFile <$> zip queries filepaths

  putStrLn "HUnit tests:"
  counts <- runTestTT (test $ testList <> sqlFileTestList)
  -- counts <- runTestTT (test $ testList )

  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure

nameWithUnderscoreGen :: Gen String
nameWithUnderscoreGen = do
  firstChar <- elements ['a' .. 'z']
  restOfString <-
    listOf $
      elements
        ( ['a' .. 'z'] <> ['0' .. '9'] <> ['_']
        )
  return $ firstChar : restOfString

checkNameWithUnderscore :: String -> Bool
checkNameWithUnderscore name =
  case parse nameWithUnderscore "" name of
    Left _ -> False
    Right _ -> True

shouldParse :: Parser a -> String -> Test
shouldParse parser s = TestCase $ case runParser parser "" s of
  Left _ -> assertString "Failed to parse"
  Right _ -> assertBool "Parsed" True

shouldParseSQLFile :: (String, FilePath) -> Test
shouldParseSQLFile s = TestCase $ case runParser parseSQL (snd s) (fst s) of
  Left _ -> assertString $ "Failed to parse " <> snd s
  Right _ -> assertBool ("Parsed " <> snd s) True

shouldParseTo :: (Eq a, Show a) => Parser a -> String -> a -> Test
shouldParseTo parser s expected = TestCase $ case runParser parser "" s of
  Left _ -> assertString $ "Failed to parse" <> s
  Right result -> assertEqual ("Parsed '" <> s <> "' but got: ") expected result

testList :: [Test]
testList =
  [ shouldParseTo parseExpression "tab.column where " [(mkPos 1, Just "tab", "column")],
    shouldParseTo parseExpression "column where " [(mkPos 1, Nothing, "column")],
    shouldParseTo parseExpression "tab.column = 2 where " [(mkPos 1, Just "tab", "column")],
    shouldParseTo parseExpression "tab.column = 2 where " [(mkPos 1, Just "tab", "column")],
    shouldParseTo parseExpression "tab.column in ('a', 'b') where " [(mkPos 1, Just "tab", "column")],
    shouldParseTo parseExpression "tab.column = 'literal_string' where " [(mkPos 1, Just "tab", "column")],
    shouldParseTo parseExpression "tab.column between date '2023-01-01' and '2023-01-31' where " [(mkPos 1, Just "tab", "column")],
    shouldParseTo parseExpression "st_contains(tab.column, tab.other_col) where " [(mkPos 1, Just "tab", "column"), (mkPos 1, Just "tab", "other_col")],
    shouldParseTo parseExpression "tab.column = 'literal_string' and tab.other_col = 2 where " [(mkPos 1, Just "tab", "column"), (mkPos 1, Just "tab", "other_col")],
    shouldParseTo parseExpression "tab.column is not null where " [(mkPos 1, Just "tab", "column")],
    shouldParseTo column "tab.col as alias" ("alias", [(mkPos 1, Just "tab", "col")]),
    shouldParseTo column "1.0 * tab.n / tab2.tot as perc" ("perc", [(mkPos 1, Just "tab", "n"), (mkPos 1, Just "tab2", "tot")]),
    shouldParseTo column "date_trunc('month', tab.ds) as month_ds" ("month_ds", [(mkPos 1, Just "tab", "ds")]),
    shouldParseTo column "case\nwhen tab.col = 1 then 'a'\nwhen tab.col = 2 then 'b'\nelse 'c'\nend as alias" ("alias", [(mkPos 2, Just "tab", "col"), (mkPos 3, Just "tab", "col")]),
    shouldParseTo column "sqrt(pow(lon, 2) + pow(lat, 2)) as distance" ("distance", [(mkPos 1, Nothing, "lon"), (mkPos 1, Nothing, "lat")])
  ]

queryDir :: String
queryDir = "test/test_queries/"
