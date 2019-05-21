{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Streaming parsers for the Atom 1.0 standard.
module Text.Atom.Conduit.Parse
  ( -- * Top-level
    atomFeed
    -- * Elements
  , atomEntry
  , atomContent
  , atomCategory
  , atomLink
  , atomGenerator
  , atomSource
    -- * Constructs
  , atomPerson
  , atomText
  ) where

-- {{{ Imports
import           Blaze.ByteString.Builder (toByteString)
import           Conduit
import           Control.Applicative      hiding (many)
import           Control.Exception.Safe   as Exception
import           Control.Monad
import           Control.Monad.Fix
import           Data.Maybe
import           Data.Monoid
import           Data.Text                as Text (Text, unpack)
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC3339
import           Data.XML.Types as XML
import           Lens.Simple
import           Refined
import           Text.Atom.Types
import           Text.XML.Stream.Parse
import qualified Text.XML.Stream.Render   as Render
import qualified Text.XML.Unresolved as Unresolved

import           URI.ByteString
-- }}}

-- {{{ Util
data AtomException = InvalidDate Text
                   | InvalidURI URIParseError Text
                   | MissingElement Text

deriving instance Eq AtomException
deriving instance Show AtomException

instance Exception AtomException where
  displayException (InvalidDate t)    = "Invalid date: " <> unpack t
  displayException (InvalidURI e t)   = "Invalid URI reference: " <> show e <> " in " <> unpack t
  displayException (MissingElement t) = "Missing element: " <> unpack t

asURIReference :: MonadThrow m => Text -> m AtomURI
asURIReference t = case (parseURI' t, parseRelativeRef' t) of
  (Right u, _)     -> return $ AtomURI u
  (_, Right u)     -> return $ AtomURI u
  (Left _, Left e) -> throw $ InvalidURI e t
  where parseURI' = parseURI laxURIParserOptions . encodeUtf8
        parseRelativeRef' = parseRelativeRef laxURIParserOptions . encodeUtf8

liftMaybe :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
liftMaybe e = maybe (throw e) return

-- | Like 'tagName' but ignores the namespace.
tagName' :: MonadThrow m => Text -> AttrParser a -> (a -> ConduitM Event o m b) -> ConduitM Event o m (Maybe b)
tagName' t = tag' (matching $ \n -> nameLocalName n == t && nameNamespace n == Just "http://www.w3.org/2005/Atom")

-- | Tag which content is a date-time that follows RFC 3339 format.
tagDate :: MonadThrow m => Text -> ConduitM Event o m (Maybe UTCTime)
tagDate name = tagIgnoreAttrs' name $ do
  text <- content
  zonedTimeToUTC <$> liftMaybe (InvalidDate text) (parseTimeRFC3339 text)

-- | Like 'tagName'' but ignores all attributes.
tagIgnoreAttrs' :: MonadThrow m => Text -> ConduitM Event o m a -> ConduitM Event o m (Maybe a)
tagIgnoreAttrs' name handler = tagName' name ignoreAttrs $ const handler

xhtmlContent :: MonadThrow m => ConduitM Event o m XML.Element
xhtmlContent = force "element" $ many_ takeAnyTreeContent .| mapC (Nothing, ) .| Unresolved.elementFromEvents


projectC :: Monad m => Fold a a' b b' -> ConduitT a b m ()
projectC prism = fix $ \recurse -> do
  item <- await
  case (item, item ^? (_Just . prism)) of
    (_, Just a) -> yield a >> recurse
    (Just _, _) -> recurse
    _           -> return ()

headRequiredC :: MonadThrow m => Text -> ConduitT a o m a
headRequiredC e = liftMaybe (MissingElement e) =<< headC

atomId :: MonadThrow m => ConduitM Event o m (Maybe Text)
atomId = tagIgnoreAttrs' "id" content

atomIcon, atomLogo :: MonadThrow m => ConduitM Event o m (Maybe AtomURI)
atomIcon = tagIgnoreAttrs' "icon" $ content >>= asURIReference
atomLogo = tagIgnoreAttrs' "logo" $ content >>= asURIReference
-- }}}


data PersonPiece = PersonName (Refined (Not Null) Text)
                 | PersonEmail Text
                 | PersonUri AtomURI

makeTraversals ''PersonPiece

-- | Parse an Atom person construct.
-- Example:
--
-- > <author>
-- >   <name>John Doe</name>
-- >   <email>JohnDoe@example.com</email>
-- >   <uri>http://example.com/~johndoe</uri>
-- > </author>
atomPerson :: MonadThrow m => Text -> ConduitM Event o m (Maybe AtomPerson)
atomPerson name = tagIgnoreAttrs' name $ (manyYield' (choose piece) .| parser) <* many ignoreAnyTreeContent where
  parser = getZipConduit $ AtomPerson
    <$> ZipConduit (projectC _PersonName .| headRequiredC "Missing or invalid <name> element")
    <*> ZipConduit (projectC _PersonEmail .| headDefC "")
    <*> ZipConduit (projectC _PersonUri .| headC)
  piece = [ fmap PersonName <$> tagIgnoreAttrs' "name" (content >>= refineThrow)
          , fmap PersonEmail <$> tagIgnoreAttrs' "email" content
          , fmap PersonUri <$> tagIgnoreAttrs' "uri" (content >>= asURIReference)
          ]


-- | Parse an @atom:category@ element.
-- Example:
--
-- > <category term="sports"/>
atomCategory :: MonadThrow m => ConduitM Event o m (Maybe AtomCategory)
atomCategory = tagName' "category" categoryAttrs $ \(t, s, l) -> do
  term <- refineThrow t
  return $ AtomCategory term s l
  where categoryAttrs = (,,) <$> requireAttr "term"
                             <*> (requireAttr "scheme" <|> pure mempty)
                             <*> (requireAttr "label" <|> pure mempty)
                             <* ignoreAttrs

-- | Parse an @atom:content@ element.
atomContent :: MonadThrow m => ConduitM Event o m (Maybe AtomContent)
atomContent = tagName' "content" contentAttrs handler where
  contentAttrs = (,) <$> optional (requireAttr "type") <*> optional (requireAttr "src" >>= asURIReference) <* ignoreAttrs
  handler (Just "xhtml", _) = AtomContentInlineXHTML <$> xhtmlContent
  handler (ctype, Just uri) = return $ AtomContentOutOfLine (fromMaybe mempty ctype) uri
  handler (Just "html", _) = AtomContentInlineText TypeHTML <$> content
  handler (Nothing, _) = AtomContentInlineText TypeText <$> content
  handler (Just ctype, _) = AtomContentInlineOther ctype <$> content

-- | Parse an @atom:link@ element.
-- Examples:
--
-- > <link rel="self" href="/feed" />
--
-- > <link rel="alternate" href="/blog/1234"/>
atomLink :: MonadThrow m => ConduitM Event o m (Maybe AtomLink)
atomLink = tagName' "link" linkAttrs $ \(href, rel, ltype, lang, title, length') ->
  return $ AtomLink href rel ltype lang title length'
  where linkAttrs = (,,,,,) <$> (requireAttr "href" >>= asURIReference)
                            <*> (requireAttr "rel" <|> pure mempty)
                            <*> (requireAttr "type" <|> pure mempty)
                            <*> (requireAttr "hreflang" <|> pure mempty)
                            <*> (requireAttr "title" <|> pure mempty)
                            <*> (requireAttr "length" <|> pure mempty)
                            <* ignoreAttrs

-- | Parse an Atom text construct.
-- Examples:
--
-- > <title type="text">AT&amp;T bought by SBC!</title>
--
-- > <title type="html">
-- >   AT&amp;amp;T bought &lt;b&gt;by SBC&lt;/b&gt;!
-- > </title>
--
-- > <title type="xhtml">
-- >   <div xmlns="http://www.w3.org/1999/xhtml">
-- >     AT&amp;T bought <b>by SBC</b>!
-- >   </div>
-- > </title>
atomText :: MonadThrow m => Text -> ConduitM Event o m (Maybe AtomText)
atomText name = tagName' name (optional (requireAttr "type") <* ignoreAttrs) handler
  where handler (Just "xhtml") = AtomXHTMLText <$> xhtmlContent
        handler (Just "html") = AtomPlainText TypeHTML <$> content
        handler _ = AtomPlainText TypeText <$> content

-- | Parse an @atom:generator@ element.
-- Example:
--
-- > <generator uri="/myblog.php" version="1.0">
-- >   Example Toolkit
-- > </generator>
atomGenerator :: MonadThrow m => ConduitM Event o m (Maybe AtomGenerator)
atomGenerator = tagName' "generator" generatorAttrs $ \(uri, version) -> AtomGenerator uri version <$> (refineThrow =<< content)
  where generatorAttrs = (,) <$> optional (requireAttr "uri" >>= asURIReference) <*> (requireAttr "version" <|> pure mempty) <* ignoreAttrs


data SourcePiece = SourceAuthor AtomPerson
                 | SourceCategory AtomCategory
                 | SourceContributor AtomPerson
                 | SourceGenerator AtomGenerator
                 | SourceIcon AtomURI
                 | SourceId Text
                 | SourceLink AtomLink
                 | SourceLogo AtomURI
                 | SourceRights AtomText
                 | SourceSubtitle AtomText
                 | SourceTitle AtomText
                 | SourceUpdated UTCTime

makeTraversals ''SourcePiece

-- | Parse an @atom:source@ element.
-- Example:
--
-- > <source>
-- >   <id>http://example.org/</id>
-- >   <title>Fourty-Two</title>
-- >   <updated>2003-12-13T18:30:02Z</updated>
-- >   <rights>© 2005 Example, Inc.</rights>
-- > </source>
atomSource :: MonadThrow m => ConduitM Event o m (Maybe AtomSource)
atomSource = tagIgnoreAttrs' "source" $ manyYield' (choose piece) .| zipConduit where
  zipConduit = getZipConduit $ AtomSource
    <$> ZipConduit (projectC _SourceAuthor .| sinkList)
    <*> ZipConduit (projectC _SourceCategory .| sinkList)
    <*> ZipConduit (projectC _SourceContributor .| sinkList)
    <*> ZipConduit (projectC _SourceGenerator .| headC)
    <*> ZipConduit (projectC _SourceIcon .| headC)
    <*> ZipConduit (projectC _SourceId .| headDefC "")
    <*> ZipConduit (projectC _SourceLink .| sinkList)
    <*> ZipConduit (projectC _SourceLogo .| headC)
    <*> ZipConduit (projectC _SourceRights .| headC)
    <*> ZipConduit (projectC _SourceSubtitle .| headC)
    <*> ZipConduit (projectC _SourceTitle .| headC)
    <*> ZipConduit (projectC _SourceUpdated .| headC)
  piece = [ fmap SourceAuthor <$> atomPerson "author"
          , fmap SourceCategory <$> atomCategory
          , fmap SourceContributor <$> atomPerson "contributor"
          , fmap SourceGenerator <$> atomGenerator
          , fmap SourceIcon <$> atomIcon
          , fmap SourceId <$> atomId
          , fmap SourceLink <$> atomLink
          , fmap SourceLogo <$> atomLogo
          , fmap SourceRights <$> atomText "rights"
          , fmap SourceSubtitle <$> atomText "subtitle"
          , fmap SourceTitle <$> atomText "title"
          , fmap SourceUpdated <$> tagDate "updated"
          ]


data EntryPiece = EntryAuthor      AtomPerson
                | EntryCategory    AtomCategory
                | EntryContent     AtomContent
                | EntryContributor AtomPerson
                | EntryId          Text
                | EntryLink        AtomLink
                | EntryPublished   UTCTime
                | EntryRights      AtomText
                | EntrySource      AtomSource
                | EntrySummary     AtomText
                | EntryTitle       AtomText
                | EntryUpdated     UTCTime

makeTraversals ''EntryPiece

-- | Parse an @atom:entry@ element.
atomEntry :: MonadThrow m => ConduitM Event o m (Maybe AtomEntry)
atomEntry = tagIgnoreAttrs' "entry" $ manyYield' (choose piece) .| zipConduit where
  zipConduit = getZipConduit $ AtomEntry
    <$> ZipConduit (projectC _EntryAuthor .| sinkList)
    <*> ZipConduit (projectC _EntryCategory .| sinkList)
    <*> ZipConduit (projectC _EntryContent .| headC)
    <*> ZipConduit (projectC _EntryContributor .| sinkList)
    <*> ZipConduit (projectC _EntryId .| headRequiredC "Missing <id> element")
    <*> ZipConduit (projectC _EntryLink .| sinkList)
    <*> ZipConduit (projectC _EntryPublished .| headC)
    <*> ZipConduit (projectC _EntryRights .| headC)
    <*> ZipConduit (projectC _EntrySource .| headC)
    <*> ZipConduit (projectC _EntrySummary .| headC)
    <*> ZipConduit (projectC _EntryTitle .| headRequiredC "Missing or invalid <title> element.")
    <*> ZipConduit (projectC _EntryUpdated .| headRequiredC "Missing or invalid <updated> element.")
  piece = [ fmap EntryAuthor <$> atomPerson "author"
          , fmap EntryCategory <$> atomCategory
          , fmap EntryContent <$> atomContent
          , fmap EntryContributor <$> atomPerson "contributor"
          , fmap EntryId <$> atomId
          , fmap EntryLink <$> atomLink
          , fmap EntryPublished <$> tagDate "published"
          , fmap EntryRights <$> atomText "rights"
          , fmap EntrySource <$> atomSource
          , fmap EntrySummary <$> atomText "summary"
          , fmap EntryTitle <$> atomText "title"
          , fmap EntryUpdated <$> tagDate "updated"
          ]


data FeedPiece = FeedAuthor AtomPerson
               | FeedCategory AtomCategory
               | FeedContributor AtomPerson
               | FeedEntry AtomEntry
               | FeedGenerator AtomGenerator
               | FeedIcon AtomURI
               | FeedId Text
               | FeedLink AtomLink
               | FeedLogo AtomURI
               | FeedRights AtomText
               | FeedSubtitle AtomText
               | FeedTitle AtomText
               | FeedUpdated UTCTime

makeTraversals ''FeedPiece

-- | Parse an @atom:feed@ element.
atomFeed :: MonadThrow m => ConduitM Event o m (Maybe AtomFeed)
atomFeed = tagIgnoreAttrs' "feed" $ manyYield' (choose piece) .| zipConduit where
  zipConduit = getZipConduit $ AtomFeed
    <$> ZipConduit (projectC _FeedAuthor .| sinkList)
    <*> ZipConduit (projectC _FeedCategory .| sinkList)
    <*> ZipConduit (projectC _FeedContributor .| sinkList)
    <*> ZipConduit (projectC _FeedEntry .| sinkList)
    <*> ZipConduit (projectC _FeedGenerator .| headC)
    <*> ZipConduit (projectC _FeedIcon .| headC)
    <*> ZipConduit (projectC _FeedId .| headRequiredC "Missing <id> element")
    <*> ZipConduit (projectC _FeedLink .| sinkList)
    <*> ZipConduit (projectC _FeedLogo .| headC)
    <*> ZipConduit (projectC _FeedRights .| headC)
    <*> ZipConduit (projectC _FeedSubtitle .| headC)
    <*> ZipConduit (projectC _FeedTitle .| headRequiredC "Missing <title> element.")
    <*> ZipConduit (projectC _FeedUpdated .| headRequiredC "Missing <updated> element.")
  piece = [ fmap FeedAuthor <$> atomPerson "author"
          , fmap FeedCategory <$> atomCategory
          , fmap FeedContributor <$> atomPerson "contributor"
          , fmap FeedEntry <$> atomEntry
          , fmap FeedGenerator <$> atomGenerator
          , fmap FeedIcon <$> atomIcon
          , fmap FeedId <$> atomId
          , fmap FeedLink <$> atomLink
          , fmap FeedLogo <$> atomLogo
          , fmap FeedRights <$> atomText "rights"
          , fmap FeedSubtitle <$> atomText "subtitle"
          , fmap FeedTitle <$> atomText "title"
          , fmap FeedUpdated <$> tagDate "updated"
          ]
