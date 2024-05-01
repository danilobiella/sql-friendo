{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- modified from 
-- https://github.com/mrkkrp/megaparsec/blob/58f2adf29d864cf54824bc19af2fa314bb1db5c0/Text/Megaparsec/Error.hs#L356

module CustomErrors (customErrorBundlePretty) where

import Text.Megaparsec

customErrorBundlePretty ::
  forall s e.
  ( VisualStream s,
    TraversableStream s,
    ShowErrorComponent e
  ) =>
  -- | Parse error bundle to display
  ParseErrorBundle s e ->
  -- | Textual rendition of the bundle
  String
customErrorBundlePretty ParseErrorBundle {..} =
  let (r, _) = foldl f (id, bundlePosState) bundleErrors
   in drop 1 (r "")
  where
    f ::
      (ShowS, PosState s) ->
      ParseError s e ->
      (ShowS, PosState s)
    f (o, !pst) e = (o . (outChunk ++), pst')
      where
        (msline, pst') = reachOffset (errorOffset e) pst
        epos = pstateSourcePos pst'
        outChunk =
          ":\n"
            <> offendingLine
            <> customFormatError (parseErrorTextPretty e)
        offendingLine =
          case msline of
            Nothing -> ""
            Just _ ->
              let lineNumber = (show . unPos . sourceLine) epos
               in ":"
                    <> lineNumber
                    <> " errors/sql "

customFormatError :: String -> String
customFormatError [] = []
customFormatError [x] = [x]
customFormatError ('\n' : xs) = ' ' : customFormatError xs
customFormatError (x : xs) = x : customFormatError xs
