module Main (main) where

import CustomErrors
import Data.Either (lefts, rights)
import Data.List (find)
import Data.Maybe (fromMaybe)
import SQLParser
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Show.Pretty

main :: IO ()
main = do
  args <- getArgs
  let exampleContent = unlines args
  case parse parseSQL "sql_example.sql" exampleContent of
    Left err -> putStr (customErrorBundlePretty err)
    Right cte -> do
      let newCte = map (removeAliases . fillTableAliases) cte
      putStrLn $ ppShow $ lefts newCte
      mapM_ putStrLn $ rights newCte
      mapM_ (print . errorMessage) $ findBadReferenceCols $ lefts newCte

fillTableAliases :: Table -> Table
fillTableAliases table = table {referencedTables = map fillAlias (referencedTables table)}
  where
    fillAlias :: ReferencedTable -> ReferencedTable
    fillAlias (tab, Nothing) = (tab, Just tab)
    fillAlias (tab, alias) = (tab, alias)

removeAliases :: Table -> Either Table String
removeAliases t = do
  let newColumns = map removeAlias (referencedColumns t)
  case rights newColumns of
    [] -> Left t {referencedColumns = lefts newColumns}
    errors -> Right $ unlines errors
  where
    removeAlias :: ReferencedColumn -> Either ReferencedColumn String
    removeAlias (pos, Nothing, col) = Left (pos, Nothing, col)
    removeAlias (pos, alias, col) = case getfullTableName alias of
      Nothing ->
        Right $
          ":"
            <> (show . unPos) pos
            <> " error/sql alias "
            <> fromMaybe "ATTENZIONE" alias
            <> " does not exist"
      Just tableName -> Left (pos, Just tableName, col)

    getfullTableName :: Alias -> Alias
    getfullTableName alias = case filter (\x -> snd x == alias) (referencedTables t) of
      [] -> Nothing
      [name] -> Just $ fst name
      _ -> error "alias is not unique"

findBadReferenceCols :: [Table] -> [ReferencedColumn]
findBadReferenceCols tables =
  concatMap
    getBadCols
	(filter (not <$> hasOnlyOneReferencedTable) tables)
  where
    getBadCols :: Table -> [ReferencedColumn]
    getBadCols table = filter isBadCol (referencedColumns table)

    isBadCol :: ReferencedColumn -> Bool
    isBadCol col = case getTable (mysnd col) of
      Nothing -> False
      Just table -> not $ doesTableContainCol table (mytrd col)

    doesTableContainCol :: Table -> Column -> Bool
    doesTableContainCol table col = col `elem` columns table

    getTable :: Maybe String -> Maybe Table
    getTable table = find (\t -> tableName t == table) tables

    hasOnlyOneReferencedTable :: Table -> Bool
    hasOnlyOneReferencedTable table = length (referencedTables table) == 1

errorMessage :: ReferencedColumn -> String
errorMessage refCol =
  ":"
    <> (show . unPos . myfst) refCol
    <> " error/sql column "
    <> mytrd refCol
    <> " does not exist in table "
    <> (fromMaybe "ATTENZIONE2" . mysnd) refCol

myfst :: (a, b, c) -> a
myfst (a, _, _) = a

mysnd :: (a, b, c) -> b
mysnd (_, b, _) = b

mytrd :: (a, b, c) -> c
mytrd (_, _, c) = c
