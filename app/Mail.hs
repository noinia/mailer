{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Mail where

import           Control.Applicative
import           Control.Arrow (first, second)
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as Attoparsec
import           Data.Char (isAlphaNum)
import qualified Data.Map as Map
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Lazy (fromStrict)
import           Network.Mail.Mime
import           Text.Email.Parser
-- import           Text.Parsec
-- import           Text.Parsec.Text
import qualified Data.Text.IO as Text

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

parseHeaders' = Map.fromList <$> (parseHeader `sepEndBy` endOfLine)

sepEndBy            :: Parser a -> Parser b -> Parser [a]
sepEndBy pItem pSep = pItem `sepBy` pSep <* pSep

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
                  pure (simpleMail' to from subject . fromStrict)

parseHeader :: Parser (Text,Text)
parseHeader = (,) <$> parseField <* char ':' <*> parseValue

parseField :: Parser Text
parseField = Text.toLower <$> Attoparsec.takeWhile1 isAlphaNum

parseValue :: Parser Text
parseValue = Text.strip <$> Attoparsec.takeWhile1 (not . isEndOfLine)



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


-- parseAddress :: Parser Address
-- parseAddress = read . show <$> addrSpec

toAddress :: Text -> Parser Address
toAddress = pure . fromString . Text.unpack
-- TODO: Fix this

lookupH h = maybe (Left $ "header " <> show h <> " not found") Right . Map.lookup h


split1   :: Char -> Text -> (Text,Text)
split1 c = second (Text.drop 1) . Text.break (== c)


--------------------------------------------------------------------------------

parseBody :: Parser (Text,[Attachment])
parseBody = (\(t,ats) t' -> (t <> t', ats)) <$> parseBodyWithAttachments
                                            <*> takeText
                                            <*  endOfInput

parseBodyWithAttachments :: Parser (Text,[Attachment])
parseBodyWithAttachments = first Text.concat . unzip <$> many parseBodyAndAttachment


parseBodyAndAttachment  :: Parser (Text,Attachment)
parseBodyAndAttachment = (,) <$> parseBodyText <*> parseAttachment'

parseBodyText :: Parser Text
parseBodyText = Text.pack <$> manyTill anyChar parseAttachmentStart



-- test = parseOnly parseBody
--   <$> Text.readFile "/home/frank/teaching/2019/geometric_algorithms/template_final_mail.txt"
test = parseOnly parseBody
  <$> Text.readFile "/tmp/body.txt"

parseAttachmentStart :: Parser Text
parseAttachmentStart = string "<#part "

parseAttachment :: Parser Attachment
parseAttachment = parseAttachmentStart *> parseAttachment'

parseAttachment' :: Parser Attachment
parseAttachment' = (,) <$> parseType
                       <*  space
                       <*> parsePath
                       <*  space
                       <*  parseDisposition
                       <*  string ">\n<#/part>"

parseKeyVal      :: Text -> Parser a -> Parser (Text,a)
parseKeyVal k pv = (,) <$> string k <* char '=' <*> pv'
  where
    pv' = between (char '"') (char '"') pv <|> pv

between :: Parser a -> Parser b -> Parser c -> Parser c
between pStart pEnd pItem = pStart *> pItem <* pEnd


parseType :: Parser ContentTypeString
parseType = snd <$> parseKeyVal "type" (Attoparsec.takeWhile1 (`notElem` [' ', '"']))


parsePath  :: Parser FilePath
parsePath  = Text.unpack . snd <$> parseKeyVal "filename" (Attoparsec.takeWhile1 (/= '\"'))

parseDisposition :: Parser Text
parseDisposition = snd <$> parseKeyVal "disposition" (string "attachment" <|> string "inline")


-- test2 = parseOnly parseAttachment <#part type=\"application/pdf\" filename=\"/home/frank/rondetijd en.pdf\" disposition=attachment>\n<#/part>"

foo = "type=\"application/octet-stream\" filename=\"/home/frank/.gitconfig\" disposition=attachment>\n<#/part>\n\nRest\n\n--\n\n- Frank\n"
