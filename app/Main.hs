{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List
import Data.Map as M
import Data.Text as T
import Data.Text.IO as TIO
import System.Directory
import System.FilePath
import Text.DocTemplates (Context (..), Val (..), toVal)
import Text.Pandoc
import Utils.Common
import Utils.Pandoc

#include "Config.hs"

main :: IO ()
main = do
  result <- runIO $ do
    globalCtx <- readConfigYaml "config.yaml"

    (pages, cssFiles) <- liftIO $ do
      whenM (doesDirectoryExist outputDir) $ removeDirectoryRecursive outputDir
      createDirectory outputDir
      pages <- listDirectory contentDir
      notHTMLFiles <- fmap (sort . Prelude.filter (\f -> takeExtension f /= ".html")) $ listDirectory templatesDir
      let isTopLevelCss path = takeExtension path == ".css" && takeDirectory path == templatesDir
      -- prepend slash to stay root-level
      cssFiles <-
        concatMapM
          ( \f ->
              fmap (Prelude.map (('/' :) . takeFileName)) $ copyRecursive isTopLevelCss False (templatesDir </> f) (outputDir </> f)
          )
          notHTMLFiles
      return (pages, cssFiles)

    globalCtx' <- setListVariable "css" (Prelude.map T.pack cssFiles) globalCtx
    mPageTemplate <- compileTmpl (templatesDir </> "page.html")
    mHomeTemplate <- compileTmpl (templatesDir </> "home.html")
    (_, homeTemplate) <- case (mPageTemplate, mHomeTemplate) of
      (Just p, Just h) -> return (p, h)
      (Nothing, _) -> throwError $ PandocAppError "cannot compile page template"
      (_, Nothing) -> throwError $ PandocAppError "cannot compile home template"

    blogList <- fmap Prelude.concat $
      forM pages $ \entry -> do
        let entryDir = contentDir </> entry
            mdPath = entryDir </> "index.md"
            outEntryDir = outputDir </> entry

        liftIO $ createDirectoryIfMissing False entryDir
        mdExists <- liftIO $ doesFileExist mdPath
        if not mdExists
          then liftIO $ ioError $ userError $ "Missing file: " ++ mdPath
          else do
            _ <- liftIO $ do
              copyRecursive (\p -> takeExtension p == ".md") True entryDir outEntryDir

            pandocDoc <- readToPandocDoc readerOptions mdPath
            let metaValMap = metaToVal $ extractMeta pandocDoc
                pageCtx = mergeContext globalCtx' $ Context metaValMap
                pageWriterOpts = writerOptions {writerVariables = pageCtx, writerTemplate = mPageTemplate}

            writePandocDocToFile pageWriterOpts pandocDoc (outEntryDir </> "index.html")

            let metaValMapWithUrl = M.insert "url" (toVal (pack $ entry </> "index.html")) metaValMap
            return [MapVal (Context metaValMapWithUrl) :: Val Text]

    -- sort by date descending (newest first)
    let blogListSorted = sortBy (\a b -> compare (extractAttr "date" b) (extractAttr "date" a)) blogList

    let homeCtx = mergeContext globalCtx' $ Context $ M.fromList [("entries", ListVal blogListSorted)]
    liftIO $ TIO.writeFile (outputDir </> "index.html") $ renderTmpl homeTemplate homeCtx

  case result of
    Left err -> print $ T.pack ("Pandoc error: " ++ show err)
    Right _ -> return ()