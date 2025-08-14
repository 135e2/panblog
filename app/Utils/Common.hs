{-# LANGUAGE FlexibleContexts #-}

module Utils.Common where

import Data.Map as M
import Data.Text
import Text.DocTemplates (Context (..), Val (..), toVal)
import Text.Pandoc
import Text.Pandoc.Readers.Markdown (yamlToMeta)
import Utils.Pandoc

readConfigYaml :: FilePath -> PandocIO (Context Text)
readConfigYaml fp = do
  content <- readFileStrict fp
  meta <- yamlToMeta readerOptions (Just fp) content
  return $ Context (metaToVal meta)

setVariable :: Text -> Text -> Context Text -> PandocIO (Context Text)
setVariable key val (Context ctx) = return $ Context $ M.alter go key ctx
  where
    go Nothing = Just $ toVal val
    go (Just x) = Just x

setListVariable ::
  Text ->
  [Text] ->
  Context Text ->
  PandocIO (Context Text)
setListVariable _ [] ctx = return ctx
setListVariable k vs ctx = do
  let ctxMap = unContext ctx
  return $
    Context $
      case M.lookup k ctxMap of
        Just (ListVal xs) ->
          M.insert
            k
            (ListVal $ xs ++ Prelude.map toVal vs)
            ctxMap
        Just v ->
          M.insert
            k
            (ListVal $ v : Prelude.map toVal vs)
            ctxMap
        Nothing -> M.insert k (toVal vs) ctxMap

-- ctx2 has higher priority
mergeContext :: Context Text -> Context Text -> Context Text
mergeContext (Context ctx1) (Context ctx2) =
  Context (M.union ctx2 ctx1)