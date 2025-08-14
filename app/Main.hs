{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, forM_, when)
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
    tmplFiles <- liftIO $ listDirectory templatesDir
    let notHTMLFiles = sort $ Prelude.filter (\f -> takeExtension f /= ".html") tmplFiles
        -- prepend slash to stay root-level
        cssFiles = Prelude.map ('/' :) $ Prelude.filter ((== ".css") . takeExtension) notHTMLFiles
    globalCtx' <- setListVariable "css" (Prelude.map T.pack cssFiles) globalCtx
    mPageTemplate <- compileTmpl (templatesDir </> "page.html")
    mHomeTemplate <- compileTmpl (templatesDir </> "home.html")
    (_, homeTemplate) <- case (mPageTemplate, mHomeTemplate) of
      (Just p, Just h) -> return (p, h)
      (Nothing, _) -> throwError $ PandocAppError "cannot compile page template"
      (_, Nothing) -> throwError $ PandocAppError "cannot compile home template"

    liftIO $ do
      exists <- doesDirectoryExist outputDir
      when exists (removeDirectoryRecursive outputDir)
      createDirectory outputDir
      forM_ notHTMLFiles $ \f -> copyFile (templatesDir </> f) (outputDir </> f)

    pages <- liftIO $ listDirectory contentDir

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
            liftIO $ do
              createDirectory outEntryDir
              files <- listDirectory entryDir
              forM_ files $ \f ->
                when (f /= "index.md") $
                  copyFile (entryDir </> f) (outEntryDir </> f)

            pandocDoc <- readToPandocDoc readerOptions mdPath
            let metaValMap = metaToVal $ extractMeta pandocDoc
                pageCtx = mergeContext globalCtx' $ Context metaValMap
                pageWriterOpts = writerOptions {writerVariables = pageCtx, writerTemplate = mPageTemplate}

            writePandocDocToFile pageWriterOpts pandocDoc (outEntryDir </> "index.html")

            let metaValMapWithUrl = M.insert "url" (toVal (pack $ entry </> "index.html")) metaValMap
            return [MapVal (Context metaValMapWithUrl) :: Val Text]

    let homeCtx = mergeContext globalCtx' $ Context $ M.fromList [("entries", ListVal blogList)]
    liftIO $ TIO.writeFile (outputDir </> "index.html") $ renderTmpl homeTemplate homeCtx

  case result of
    Left err -> print $ T.pack ("Pandoc error: " ++ show err)
    Right _ -> return ()