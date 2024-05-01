module SQLParser where

import Data.List.NonEmpty
import qualified Data.Semigroup.Foldable as F
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)

type Column = String

type Alias = Maybe String

type ReferencedColumn = (Pos, Alias, Column)

type ReferencedTable = (String, Alias)

data Table = Table
  { tableName :: Maybe String,
    columns :: [Column],
    referencedTables :: [ReferencedTable],
    referencedColumns :: [ReferencedColumn]
  }
  deriving (Show)

type Parser = Parsec Void String

instance Semigroup Table where
  (Table t1 c1 rt1 rc1) <> (Table t2 c2 rt2 rc2) = Table (t1 <> t2) (c1 <> c2) (rt1 <> rt2) (rc1 <> rc2)

-- Lexers
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "--"
    blockComment = L.skipBlockComment "/*" "*/"

-- Parsers

parseSQL :: Parser [Table]
parseSQL = do
  _ <- optional . try $ parseInsertInto
  parseSQLWithSubqueries <|> parseSQLOneCTE

parseInsertInto :: Parser ()
parseInsertInto = do
  _ <- symbol "insert"
  _ <- symbol "into"
  _ <- dotName <|> parametrizedTable
  return ()

parseSQLOneCTE :: Parser [Table]
parseSQLOneCTE = do
  table <- parseCTEwithUnions
  return [table]

parseSQLWithSubqueries :: Parser [Table]
parseSQLWithSubqueries = do
  _ <- symbol "with"
  tables <- parseCTE `sepBy1` lexeme (char ',')
  lastTable <- parseCTE_
  return $ tables <> [lastTable]

parseCTE :: Parser Table
parseCTE =
  -- dbg "parseCTE" $
  lexeme $ do
    cteName <- lexeme nameWithUnderscore
    _ <- symbol "as"
    table <- lexeme $ between (symbol "(") (symbol ")") parseCTEwithUnions
    return table {tableName = Just cteName}

parseCTEwithUnions :: Parser Table
parseCTEwithUnions = lexeme $ F.fold1 . fromList <$> parseCTE_ `sepBy1` unionKeyword
  where
    unionKeyword = lexeme $ choice $ try <$> [symbol "union" <* symbol "all", symbol "union"]

parseCTE_ :: Parser Table
parseCTE_ =
  -- dbg "parseCTE_" $
  lexeme $ do
    _ <- symbol "select"
    _ <- optional $ symbol "distinct"
    selectStuff <- lexeme $ column `sepBy1` lexeme (char ',')
    table <- parseFrom
    joinStuff <- optional $ lexeme (try parseCrossJoin <|> try parseJoin)

    whereCols <- optional $ lexeme $ symbol "where" *> parseExpression
    groupByCols <- optional $ lexeme $ symbol "group by" *> parseGroupby
    havingCols <- optional $ lexeme $ symbol "having" *> parseExpression
    orderByCols <- optional $ lexeme $ symbol "order by" *> parseExpression
    _ <- optional $ lexeme $ symbol "limit" *> some digitChar

    let columns = fst <$> selectStuff
    let selectReferencedCols = concatMap snd selectStuff
    let otherReferenceCols =
          (concat <$> (fmap . fmap) snd joinStuff)
            <> whereCols
            <> groupByCols
            <> havingCols
            <> orderByCols

    return
      Table
        { tableName = Nothing,
          columns = columns,
          referencedTables = case joinStuff of
            Nothing -> [table]
            Just b -> table : (fst <$> b),
          referencedColumns = case otherReferenceCols of
            Nothing -> selectReferencedCols
            Just b -> selectReferencedCols ++ b
        }

column :: Parser (Column, [ReferencedColumn])
column =
    lexeme $
      try columnWithAlias <|> simpleCol
  where
    columnWithAlias = do
      referencedCols <- parseExpressionColumn
      space
      _ <- symbol "as"
      alias <- nameWithUnderscore
      return (alias, referencedCols)

    simpleCol = do
      (pos, tableAlias, col) <- dotName
      return (col, [(pos, tableAlias, col)])

parseFrom :: Parser ReferencedTable
parseFrom =
  do
    _ <- symbol "from"
    parseTable

parseTable :: Parser ReferencedTable
parseTable =
  lexeme $ do
    (_, _, table) <- try dotName <|> parametrizedTable
    _ <- optional $ symbol "as"
    lala <- optional keywords
    case lala of
      Just _ -> return (table, Nothing)
      Nothing -> do
        tableAlias <- lexeme $ optional nameWithUnderscore
        return (table, tableAlias)

parseJoin :: Parser [(ReferencedTable, [ReferencedColumn])]
parseJoin =
  some $ do
    _ <-
      optional $
        choice
          [ symbol "inner",
            symbol "left",
            symbol "right",
            symbol "full outer",
            symbol "outer"
          ]
    _ <- symbol "join"
    (table, tableAlias) <- parseTable
    _ <- symbol "on"
    exprCols <- parseExpression
    return ((table, tableAlias), exprCols)

parseCrossJoin :: Parser [(ReferencedTable, [ReferencedColumn])]
parseCrossJoin =
  lexeme . some $ do
    _ <- symbol "cross"
    _ <- symbol "join"
    (table, tableAlias) <- parseTable
    return ((table, tableAlias), [])

dotName :: Parser (Pos, Maybe String, String)
dotName =
  lexeme $
    do
      pos <- sourceLine <$> getSourcePos
      tableAlias <- optional . try $ nameWithUnderscore <* symbol "."
      alias <- nameWithUnderscore
      return (pos, tableAlias, alias)

parametrizedTable :: Parser (Pos, Maybe String, String)
parametrizedTable =
  lexeme $ do
    pos <- sourceLine <$> getSourcePos
    between (symbol "{") (symbol "}") $ do
      alias <- nameWithUnderscore
      return (pos, Nothing, alias)

parseExpressionColumn :: Parser [ReferencedColumn]
parseExpressionColumn =
  concat <$> lexeme (manyTill expressionPiece (try keywords <|> symbol ","))

parseExpression :: Parser [ReferencedColumn]
parseExpression =
  concat <$> lexeme (manyTill expressionPiece keywords)

expressionPiece :: Parser [ReferencedColumn]
expressionPiece =
    lexeme $ do
      expr <- optional . try $ choice [exprSymbols, stringLiteral]
      case expr of
        Just _ -> return []
        Nothing ->
          choice $
            try
              <$> [ parseFunction,
                    (: []) <$> dotName
                  ]

parseFunction :: Parser [ReferencedColumn]
parseFunction =
    do
      _ <- optional . try $ nameWithUnderscore
      concat <$> between (symbol "(") (symbol ")") (many expressionPiece)

stringLiteral :: Parser String
stringLiteral =
  lexeme $
    between (char '\'') (char '\'') (many $ noneOf ['\''])

exprSymbols :: Parser String
exprSymbols =
  choice $
    try
      <$> [ symbol "+",
            symbol "-",
            symbol "*",
            symbol "^",
            symbol "/",
            symbol "=",
            symbol "<",
            symbol ">",
            symbol "<=",
            symbol ">=",
            symbol "!=",
            symbol ",",
            show <$> lexeme L.float,
            show <$> lexeme L.decimal,
            lexeme $ some digitChar,
            keyword "and",
            keyword "true",
            keyword "false",
            keyword "between",
            sqlType "date",
            sqlType "bigint",
            sqlType "double",
            sqlType "float",
            sqlType "varbinary",
            sqlType "string",
            keyword "is",
            keyword "in",
            keyword "not",
            keyword "null",
            keyword "or",
            keyword "like",
            keyword "case",
            keyword "when",
            keyword "then",
            keyword "else",
            keyword "end",
            keyword "partition",
            keyword "by",
            keyword "over",
            keyword "order",
            keyword "distinct",
            keyword "as"
          ]

keyword :: String -> Parser String
keyword s = string s <* space1

sqlType :: String -> Parser String
sqlType s =
  string s
    <* choice
      [ space1,
        do
          _ <- lookAhead (symbol ")")
          return ()
      ]

keywords :: Parser String
keywords =
  lookAhead $
    choice $
      try
        <$> [ keyword "select",
              keyword "from",
              keyword "where",
              keyword "join",
              keyword "inner",
              keyword "left",
              keyword "right",
              keyword "full outer",
              keyword "outer",
              keyword "group by",
              keyword "having",
              keyword "order by",
              keyword "limit",
              symbol ")",
              keyword "as",
              keyword "union",
              "" <$ eof
            ]

parseGroupby :: Parser [ReferencedColumn]
parseGroupby =
  parseExpression <|> do
    _ <- many (digitChar <|> lexeme (char ','))
    return []

nameWithUnderscore :: Parser String
nameWithUnderscore = do
  firstChar <- letterChar
  restOfName <- many (alphaNumChar <|> char '_')
  return (firstChar : restOfName)

ppNameWithUnderscore :: String -> String
ppNameWithUnderscore = show
