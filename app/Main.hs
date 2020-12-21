{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Arrow (first, second)
import           Control.Monad ((<=<), when)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Csv as Csv
import           Data.Foldable (toList)
import           Data.Function ((&))
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Text.Lazy (toStrict, fromStrict)
import qualified Data.Text.Read as Read
import           Data.Text.Template (Template, Context)
import qualified Data.Text.Template as Template
import           Mail (parseEmacsMail, EmacsMail(..))
import           Network.Mail.Mime
import qualified Options.Applicative
import           Options.Applicative
import           System.Process.Typed
import qualified System.Process.Typed as Process
import           Text.Parsec (parse)


--------------------------------------------------------------------------------


-- data OutputOption = Send | Store FilePath deriving (Show,Eq,Read,Ord)

data Options = Options { templatePath :: FilePath
                       , dataPath     :: FilePath
                       , send         :: Bool
                       , store        :: Maybe FilePath
                       } deriving (Show,Eq)


optionsParser :: Parser Options
optionsParser = Options <$> strOption (  long "template"
                                      <> metavar "TEMPLATE"
                                      <> help "path to the template file"
                                      )
                        <*> strOption ( long "adresses"
                                      <> metavar "ADDRESSFILE"
                                      <> help "path to the csv file with the adresses"
                                      )
                        <*> switch ( long "send"
                                     <> help "should send the messages"
                                            )
                        <*> optional (strOption (long "store"
                                               <> metavar "OUTPUTDIR"
                                               <> help "directory in which to store the messages"
                                      ))

--------------------------------------------------------------------------------

main :: IO ()
main = mainWith =<< execParser opts
  where
    opts = info optionsParser mempty

mainWith      :: Options -> IO ()
mainWith opts = do t        <- loadTemplate $ templatePath opts
                   (hdr,rs) <- loadData $ dataPath opts
                   mails    <- mapM (renderEmail t) rs
                   runStoreAction mails (store opts)
                   runSendAction  mails (send  opts)

runStoreAction       :: [Mail] -> Maybe FilePath -> IO ()
runStoreAction mails = \case
  Nothing  -> pure ()
  Just dir -> mapM_ (storeEmail dir) mails

runSendAction                  :: [Mail] -> Bool ->  IO ()
runSendAction mails shouldSend = when shouldSend (mapM_ sendEmail mails)

-- | runs
sendEmail   :: Mail -> IO ()
sendEmail m = renderMail' m >>= sendEmail' m

sendEmail'      :: Mail -> ByteString.ByteString -> IO ()
sendEmail' m bs = do withProcessWait processCfg $ \msmtpProc ->
                       pure ()
  where
    processCfg = Process.proc "msmtp" [ "--read-recipients"
                                      , "--read-envelope-from"
                                      ]
               & setStdin (byteStringInput $ bs)

--------------------------------------------------------------------------------
-- * Rendering a Text Template

-- | Loads the template from file
loadTemplate    :: FilePath -> IO Template
loadTemplate fp = Template.template <$> Text.readFile fp

-- | Renders a template to a Text
render   :: Template -> Row -> Text
render t = toStrict . Template.render t . context

-- | Create 'Context' from a Map
context   :: Row -> Context
context r = \k -> let err = error $ "Could not find key: " ++ Text.unpack k
                  in fromMaybe err $ Map.lookup k r

--------------------------------------------------------------------------------
-- * Loading CSV Data

type Field = Text
type Header = [Field]

type Row = Map.Map Field Text
type CSVData = [Row]

-- | Loads a data file into memory, returns
loadData    :: FilePath -> IO (Header, CSVData)
loadData fp = ByteString.readFile fp >>= \bs ->
                 case Csv.decodeByName bs of
                   Left err        -> error err
                   Right (_hdr,rs) -> pure $ case toList rs of
                     []       -> ([],[]) -- no data!
                     rs@(r:_) -> (Map.keys r,rs)

--------------------------------------------------------------------------------
--

-- | Renders an email with attachments
renderEmail     :: Template -> Row -> IO Mail
renderEmail t r = let EmacsMail m ats = renderPureEmail t r
                  in addAttachments ats m

-- | Renders an email without attachments
renderPureEmail     :: Template -> Row -> EmacsMail
renderPureEmail t r = case parse parseEmacsMail "template" (render t r) of
                        Left err -> error $ show err
                        Right m  -> m

storeEmail       :: FilePath -> Mail -> IO ()
storeEmail dir m = let fp = toFP dir . head . mailTo $ m
                   in renderMail' m >>= ByteString.writeFile fp

toFP         :: FilePath -> Address -> FilePath
toFP dir adr = dir <> "/" <> f (addressEmail adr) <> ".txt"
  where
    f = Text.unpack . Text.replace " " "_" . Text.strip
