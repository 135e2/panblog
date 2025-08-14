module Utils.Pandoc where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map as M
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO as TIO
import System.FilePath
import Text.DocLayout (render)
import Text.DocTemplates (Context (..), Val (..), toVal)
import Text.Pandoc

readerOptions :: ReaderOptions
readerOptions =
  def
    { readerStandalone = True,
      readerExtensions = pandocExtensions
    }

writerOptions :: WriterOptions
writerOptions =
  def
    { writerTableOfContents = True,
      writerReferenceLinks = True
    }

readToPandocDoc ::
  ReaderOptions ->
  FilePath ->
  PandocIO Pandoc
readToPandocDoc readerOpts fp = do
  md <- readFileStrict fp
  readMarkdown readerOpts $ decodeUtf8 md

writePandocDocToFile :: WriterOptions -> Pandoc -> FilePath -> PandocIO ()
writePandocDocToFile writerOpts pandocDoc fp = do
  output <- writeHtml5String writerOpts pandocDoc
  liftIO $ TIO.writeFile fp output

compileTmpl :: (PandocMonad m) => FilePath -> m (Maybe (Template Text))
compileTmpl templatePath = do
  template <- getTemplate templatePath
  cTemplate <- runWithPartials $ compileTemplate (takeDirectory templatePath) template
  return $ either (const Nothing) Just cTemplate

renderTmpl :: Template Text -> Context Text -> Text
renderTmpl tmpl (Context ctx) = render Nothing $ renderTemplate tmpl ctx

extractMeta :: Pandoc -> Meta
extractMeta (Pandoc meta _) = meta

metaToVal :: Meta -> M.Map Text (Val Text)
metaToVal (Meta m) = M.map metaValueToVal m
  where
    metaValueToVal :: MetaValue -> Val Text
    metaValueToVal (MetaString s) = toVal s
    metaValueToVal (MetaInlines ils) = toVal (stringify ils)
    metaValueToVal (MetaBlocks _) = NullVal
    metaValueToVal (MetaBool b) = toVal b
    metaValueToVal (MetaList xs) = ListVal (Prelude.map metaValueToVal xs)
    metaValueToVal (MetaMap m') = MapVal $ Context (M.map metaValueToVal m')

stringify :: [Inline] -> Text
stringify = mconcat . Prelude.map inlineToText
  where
    inlineToText (Str s) = s
    inlineToText Space = pack " "
    inlineToText _ = pack ""
