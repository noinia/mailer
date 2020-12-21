{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Mail where

import           Control.Arrow (second)
import qualified Data.Map as Map
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Lazy (fromStrict)
import           Network.Mail.Mime
import           Text.Parsec
import           Text.Parsec.Text

--------------------------------------------------------------------------------
type ContentTypeString = Text

type Attachment = (ContentTypeString,FilePath)

data EmacsMail = EmacsMail { mail        :: Mail
                           , attachments :: [Attachment]
                           } deriving (Show)

--------------------------------------------------------------------------------

-- | Given an email, in "emacs format" parses it as an email
parseEmacsMail :: Parser EmacsMail
parseEmacsMail = (\mkMail (body,ats) -> EmacsMail (mkMail body) ats)
                 <$> parseHeaders
                 <*  string headerBodySeparator
                 <*> parseBody

--------------------------------------------------------------------------------

headerBodySeparator = "--text follows this line--"






parseHeaders' = Map.fromList <$> (parseHeader `sepEndBy` newline)


lookup'     :: (Show k, Ord k) => k -> Map.Map k v -> Parser v
lookup' k m = case Map.lookup k m of
                Nothing -> fail $ "field not found: " <> show k
                Just v  -> pure v

parseHeaders :: Parser (Text -> Mail)
parseHeaders = do hdrs    <- parseHeaders'
                  from    <- toAddress =<< lookup' "from"    hdrs
                  to      <- toAddress =<< lookup' "to"      hdrs
                  subject <- lookup' "subject" hdrs
                  -- other headers
                  pure $ (simpleMail' to from subject . fromStrict)

parseHeader :: Parser (Text,Text)
parseHeader = (,) <$> parseField <* char ':' <*> parseValue

parseField :: Parser Text
parseField = Text.toLower . Text.pack <$> many1 alphaNum


parseValue :: Parser Text
parseValue = (Text.strip . Text.pack) <$> many1 (noneOf "\n\r")



-- parseHeaders


--   case Text.splitOn headerBodySeparator t of
--                      [hdrT,bdyT] -> do let hdrs = parseHeaders hdrT
--                                        f       <- toAddress =<< lookupH "from" hdrs
--                                        t       <- toAddress =<< lookupH "to" hdrs
--                                        s       <- lookupH "subject" hdrs
--                                        (b,ats) <- parseBody bdyT
--                                        pure $ EmacsMail (simpleMail' t f s $ fromStrict b) ats
--                      _           -> Left "separator between header and body not found"








type MailHeaders = Map.Map Text Text


toAddress :: Text -> Parser Address
toAddress = pure . fromString . Text.unpack
-- TODO: Fix this

lookupH h = maybe (Left $ "header " <> show h <> " not found") Right . Map.lookup h


split1   :: Char -> Text -> (Text,Text)
split1 c = second (Text.drop 1) . Text.break (== c)


--------------------------------------------------------------------------------

parseBody :: Parser (Text,[Attachment])
parseBody = (\s -> (Text.pack s,[])) <$> many anyChar


test = parseFromFile parseEmacsMail "/home/frank/teaching/2019/geometric_algorithms/template_final_mail.txt"

parseAttachment :: Parser Attachment
parseAttachment = (,) <$  string "<#part "
                      <*> parseType
                      <*  space
                      <*> parsePath
                      <*  space
                      <*  parseDisposition
                      <*  string ">\n<#/part>"

parseKeyVal      :: String -> Parser a -> Parser (String,a)
parseKeyVal k pv = (,) <$> string k <* char '=' <*> pv'
  where
    pv' = between (char '"') (char '"') pv <|> pv


parseType :: Parser ContentTypeString
parseType = snd <$> parseKeyVal "type" (Text.pack <$> many1 (noneOf " \""))

parsePath  :: Parser FilePath
parsePath  = snd <$> parseKeyVal "filename" (many1 (noneOf "\""))

parseDisposition :: Parser String
parseDisposition = snd <$> parseKeyVal "disposition" (string "attachment" <|> string "inline")


test2 = parse parseAttachment "foo" "<#part type=\"application/pdf\" filename=\"/home/frank/rondetijd en.pdf\" disposition=attachment>\n<#/part>"
