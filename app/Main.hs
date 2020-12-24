{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           Control.Concurrent(threadDelay)
import           Control.Monad (when)
import           Data.Attoparsec.Text (parseOnly)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Csv as Csv
import           Data.Foldable (toList)
import           Data.Function ((&))
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Text.Lazy (toStrict)
import           Data.Text.Template (Template, Context)
import qualified Data.Text.Template as Template
import           Mail (parseEmacsMail, EmacsMail(..))
import           Network.Mail.Mime
import           Options.Applicative
import           Prelude hiding (log)
import           System.Process.Typed
import qualified System.Process.Typed as Process


--------------------------------------------------------------------------------

data Options = Options { templatePath :: FilePath
                       , dataPath     :: FilePath
                       , send         :: Bool
                       , store        :: Maybe FilePath
                       , dump         :: Maybe FilePath
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
                                               <> help "directory in which to store the messages (as text)"
                                      ))
                        <*> optional (strOption (long "dump"
                                               <> metavar "DUMPDIR"
                                               <> help "directory in which to store the messages (in mime format)"
                                      ))

--------------------------------------------------------------------------------

main :: IO ()
main = mainWith =<< execParser opts
  where
    opts = info optionsParser mempty

mainWith      :: Options -> IO ()
mainWith opts = do t        <- loadTemplate $ templatePath opts
                   (_,rs)   <- loadData $ dataPath opts
                   mails    <- mapM (renderEmail t) rs
                   mapM_ (runAction opts) mails


runAction        :: Options -> (Text,Mail) -> IO ()
runAction opts m = mapM_ (\f -> f m) [ runStore $ store opts
                                     , runDump  $ dump opts
                                     , runSend  $ send opts
                                     ]

runStore       :: Maybe FilePath -> (Text,Mail) -> IO ()
runStore mfp m = case mfp of
  Nothing  -> pure ()
  Just dir -> storeEmail dir m

----------------------------------------

runDump              :: Maybe FilePath -> (Text, Mail) -> IO ()
runDump mfp (_,mail) = case mfp of
  Nothing  -> pure ()
  Just dir -> dumpEmail dir mail

----------------------------------------

runSend                     :: Bool ->  (Text,Mail) -> IO ()
runSend shouldSend (_,mail) = when shouldSend (sendEmail mail)

-- | runs
sendEmail   :: Mail -> IO ()
sendEmail m = renderMail' m >>= sendEmail' m

sendEmail'      :: Mail -> ByteString.ByteString -> IO ()
sendEmail' m bs = do log $ "Sending email to " <> (Text.unpack . renderAddress . head . mailTo $ m)
                     withProcessWait processCfg $ \_msmtpProc ->
                        pure ()
                     log "done"
                     threadDelay waitingTime
  where
    processCfg = Process.proc "msmtp" [ "--read-recipients"
                                      , "--read-envelope-from"
                                      ]
               & setStdin (byteStringInput bs)

waitingTime :: Int -- wait one second
waitingTime = 1000000

--------------------------------------------------------------------------------
-- * Rendering a Text Template

-- | Loads the template from file
loadTemplate    :: FilePath -> IO Template
loadTemplate fp = Template.template <$> Text.readFile fp

-- | Renders a template to a Text
render   :: Template -> Row -> Text
render t = toStrict . Template.render t . context

-- | Create 'Context' from a Map
{-# HLint ignore context #-}
context   :: Row -> Context
context r = \k -> let err = error $ "Could not find key: " <> Text.unpack k
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
renderEmail     :: Template -> Row -> IO (Text,Mail)
renderEmail t r = let (tm, EmacsMail m ats) = renderPureEmail t r
                  in (tm,) <$> addAttachments ats m

-- | Renders an email without attachments
renderPureEmail     :: Template -> Row -> (Text,EmacsMail)
renderPureEmail t r = let tm = render t r
                      in case parseOnly parseEmacsMail tm of
                           Left err -> error $ show tm <> "\n" <> show err
                           Right m  -> (tm,m)


--------------------------------------------------------------------------------

storeEmail           :: FilePath -> (Text,Mail) -> IO ()
storeEmail dir (t,m) = let fp = toFP dir "txt" . head . mailTo $ m
                       in do log $ "storing email in " <> show fp
                             Text.writeFile fp t
                             log "done"

dumpEmail       :: FilePath -> Mail -> IO ()
dumpEmail dir m = let fp = toFP dir "mail" . head . mailTo $ m
                  in do log $ "dumping email to " <> show fp
                        renderMail' m >>= ByteString.writeFile fp
                        log "done"

-- | Construct the appropritae file path
toFP             :: FilePath -> String -> Address -> FilePath
toFP dir ext adr = dir <> "/" <> f (addressEmail adr) <> "." <> ext
  where
    f = Text.unpack . Text.replace " " "_" . Text.strip

log :: String -> IO ()
log = putStrLn
