{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
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
import           Text.Atom.Types

import           Control.Applicative
import           Control.Foldl           hiding (mconcat, set)
import           Control.Monad           hiding (foldM)
import           Control.Monad.Catch

import           Data.Conduit.Parser
import           Data.Conduit.Parser.XML
import           Data.Maybe
import           Data.Monoid
import           Data.MonoTraversable
import           Data.NonNull            (NonNull, fromNullable, toNullable)
import           Data.Text               as Text (Text)
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC3339
import           Data.XML.Types

import           Lens.Simple

import           Prelude                 hiding (last, lookup)

import           Text.Parser.Combinators

import           URI.ByteString
-- }}}

-- {{{ Util
data AtomException = InvalidURI URIParseError
                   | NullElement

deriving instance Eq AtomException
deriving instance Show AtomException

instance Exception AtomException where
  displayException (InvalidURI e) = "Invalid URI reference: " ++ show e
  displayException NullElement = "Null element"

asURIReference :: (MonadThrow m) => Text -> m AtomURI
asURIReference t = case (parseURI' t, parseRelativeRef' t) of
  (Right u, _) -> return $ AtomURI u
  (_, Right u) -> return $ AtomURI u
  (Left _, Left e) -> throwM $ InvalidURI e
  where parseURI' = parseURI laxURIParserOptions . encodeUtf8
        parseRelativeRef' = parseRelativeRef laxURIParserOptions . encodeUtf8

asNonNull :: (MonoFoldable a, MonadThrow m) => a -> m (NonNull a)
asNonNull = maybe (throwM NullElement) return . fromNullable

-- | Like 'tagName' but ignores the namespace.
tagName' :: (MonadCatch m) => Text -> AttrParser a -> (a -> ConduitParser Event m b) -> ConduitParser Event m b
tagName' t = tagPredicate (\n -> nameLocalName n == t)

-- | Tag which content is a date-time that follows RFC 3339 format.
tagDate :: (MonadCatch m) => Text -> ConduitParser Event m UTCTime
tagDate name = tagIgnoreAttrs' name $ content (fmap zonedTimeToUTC . parseTimeRFC3339)

-- | Like 'tagName'' but ignores all attributes.
tagIgnoreAttrs' :: (MonadCatch m) => Text -> ConduitParser Event m a -> ConduitParser Event m a
tagIgnoreAttrs' name handler = tagName' name ignoreAttrs $ const handler

unknownTag :: (MonadCatch m) => ConduitParser Event m ()
unknownTag = anyTag $ \_ _ -> void $ many (void unknownTag <|> void textContent)

atomId :: (MonadCatch m) => ConduitParser Event m (NonNull Text)
atomId = tagIgnoreAttrs' "id" $ content asNonNull

atomIcon, atomLogo :: (MonadCatch m) => ConduitParser Event m AtomURI
atomIcon = tagIgnoreAttrs' "icon" $ content asURIReference
atomLogo = tagIgnoreAttrs' "logo" $ content asURIReference

lastRequired :: (Monad m, Parsing m) => String -> FoldM m a a
lastRequired e = FoldM (\_ a -> return $ Right a) (return $ Left e) (either unexpected return)
-- }}}


data PersonPiece = PersonName (NonNull Text)
                 | PersonEmail Text
                 | PersonUri AtomURI
                 | PersonUnknown

makeTraversals ''PersonPiece

-- | Parse an Atom person construct.
-- Example:
--
-- > <author>
-- >   <name>John Doe</name>
-- >   <email>JohnDoe@example.com</email>
-- >   <uri>http://example.com/~johndoe</uri>
-- > </author>
atomPerson :: (MonadCatch m) => Text -> ConduitParser Event m AtomPerson
atomPerson name = named ("Atom person construct <" <> name <> ">") $ tagIgnoreAttrs' name $ do
  p <- many piece
  flip foldM p $ AtomPerson
    <$> handlesM _PersonName (lastRequired "Missing or invalid <name> element.")
    <*> generalize (handles _PersonEmail $ lastDef "")
    <*> generalize (handles _PersonUri last)
  where piece :: (MonadCatch m) => ConduitParser Event m PersonPiece
        piece = choice [ PersonName <$> tagIgnoreAttrs' "name" (content asNonNull)
                       , PersonEmail <$> tagIgnoreAttrs' "email" textContent
                       , PersonUri <$> tagIgnoreAttrs' "uri" (content asURIReference)
                       , PersonUnknown <$ unknownTag
                       ]


-- | Parse an @atom:category@ element.
-- Example:
--
-- > <category term="sports"/>
atomCategory :: (MonadCatch m) => ConduitParser Event m AtomCategory
atomCategory = tagName' "category" categoryAttrs $ \(t, s, l) -> do
  term <- asNonNull t
  return $ AtomCategory term s l
  where categoryAttrs = (,,) <$> textAttr "term"
                             <*> (textAttr "scheme" <|> pure mempty)
                             <*> (textAttr "label" <|> pure mempty)
                             <* ignoreAttrs

-- | Parse an @atom:content@ element.
atomContent :: (MonadCatch m) => ConduitParser Event m AtomContent
atomContent = tagName' "content" contentAttrs handler
  where contentAttrs = (,) <$> optional (textAttr "type") <*> optional (attr "src" asURIReference) <* ignoreAttrs
        handler (Just "xhtml", _) = AtomContentInlineXHTML <$> tagIgnoreAttrs' "div" textContent
        handler (ctype, Just uri) = return $ AtomContentOutOfLine (fromMaybe mempty ctype) uri
        handler (Just "html", _) = AtomContentInlineText TypeHTML <$> textContent
        handler (Nothing, _) = AtomContentInlineText TypeText <$> textContent
        handler (Just ctype, _) = AtomContentInlineOther ctype <$> textContent

-- | Parse an @atom:link@ element.
-- Examples:
--
-- > <link rel="self" href="/feed" />
--
-- > <link rel="alternate" href="/blog/1234"/>
atomLink :: (MonadCatch m) => ConduitParser Event m AtomLink
atomLink = tagName' "link" linkAttrs $ \(href, rel, ltype, lang, title, length') ->
  return $ AtomLink href rel ltype lang title length'
  where linkAttrs = (,,,,,) <$> attr "href" asURIReference
                            <*> (textAttr "rel" <|> pure mempty)
                            <*> (textAttr "type" <|> pure mempty)
                            <*> (textAttr "hreflang" <|> pure mempty)
                            <*> (textAttr "title" <|> pure mempty)
                            <*> (textAttr "length" <|> pure mempty)
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
atomText :: (MonadCatch m) => Text -> ConduitParser Event m AtomText
atomText name = named ("Atom text construct <" <> name <> ">") $ tagName' name (optional (textAttr "type") <* ignoreAttrs) handler
  where handler (Just "xhtml") = AtomXHTMLText <$> tagIgnoreAttrs' "div" xhtmlContent
        handler (Just "html") = AtomPlainText TypeHTML <$> textContent
        handler _ = AtomPlainText TypeText <$> textContent
        xhtmlContent :: MonadCatch m => ConduitParser Event m Text
        xhtmlContent = mconcat <$> many (textContent <|> anyTag (\name attrs -> renderTag name attrs <$> xhtmlContent))
        renderTag name attrs content = "<" <> nameLocalName name <> renderAttrs attrs <> ">" <> content <> "</" <> nameLocalName name <> ">"
        renderAttrs [] = ""
        renderAttrs ((name, content):t) = " " <> nameLocalName name <> "=\"" <> mconcat (renderContent <$> content) <> "\"" <> renderAttrs t
        renderContent (ContentText t) = t
        renderContent (ContentEntity t) = t

-- | Parse an @atom:generator@ element.
-- Example:
--
-- > <generator uri="/myblog.php" version="1.0">
-- >   Example Toolkit
-- > </generator>
atomGenerator :: (MonadCatch m) => ConduitParser Event m AtomGenerator
atomGenerator = tagName' "generator" generatorAttrs $ \(uri, version) -> AtomGenerator uri version <$> (asNonNull =<< textContent)
  where generatorAttrs = (,) <$> optional (attr "uri" asURIReference) <*> (textAttr "version" <|> pure mempty) <* ignoreAttrs


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
                 | SourceUnknown

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
atomSource :: (MonadCatch m) => ConduitParser Event m AtomSource
atomSource = named "Atom <source> element" $ tagIgnoreAttrs' "source" $ do
  p <- many piece
  flip foldM p $ AtomSource
    <$> generalize (handles _SourceAuthor list)
    <*> generalize (handles _SourceCategory list)
    <*> generalize (handles _SourceContributor list)
    <*> generalize (handles _SourceGenerator last)
    <*> generalize (handles _SourceIcon last)
    <*> generalize (handles _SourceId $ lastDef "")
    <*> generalize (handles _SourceLink list)
    <*> generalize (handles _SourceLogo last)
    <*> generalize (handles _SourceRights last)
    <*> generalize (handles _SourceSubtitle last)
    <*> generalize (handles _SourceTitle last)
    <*> generalize (handles _SourceUpdated last)
  where piece :: (MonadCatch m) => ConduitParser Event m SourcePiece
        piece = choice [ SourceAuthor <$> atomPerson "author"
                       , SourceCategory <$> atomCategory
                       , SourceContributor <$> atomPerson "contributor"
                       , SourceGenerator <$> atomGenerator
                       , SourceIcon <$> atomIcon
                       , SourceId . toNullable <$> atomId
                       , SourceLink <$> atomLink
                       , SourceLogo <$> atomLogo
                       , SourceRights <$> atomText "rights"
                       , SourceSubtitle <$> atomText "subtitle"
                       , SourceTitle <$> atomText "title"
                       , SourceUpdated <$> tagDate "updated"
                       , SourceUnknown <$ unknownTag
                       ]


data EntryPiece = EntryAuthor      AtomPerson
                | EntryCategory    AtomCategory
                | EntryContent     AtomContent
                | EntryContributor AtomPerson
                | EntryId          (NonNull Text)
                | EntryLink        AtomLink
                | EntryPublished   UTCTime
                | EntryRights      AtomText
                | EntrySource      AtomSource
                | EntrySummary     AtomText
                | EntryTitle       AtomText
                | EntryUpdated     UTCTime
                | EntryUnknown

makeTraversals ''EntryPiece

-- | Parse an @atom:entry@ element.
atomEntry :: (MonadCatch m) => ConduitParser Event m AtomEntry
atomEntry = named "Atom <entry> element" $ tagIgnoreAttrs' "entry" $ do
  p <- many piece
  flip foldM p $ AtomEntry
    <$> generalize (handles _EntryAuthor list)
    <*> generalize (handles _EntryCategory list)
    <*> generalize (handles _EntryContent last)
    <*> generalize (handles _EntryContributor list)
    <*> handlesM _EntryId (lastRequired "Missing or invalid <id> element.")
    <*> generalize (handles _EntryLink list)
    <*> generalize (handles _EntryPublished last)
    <*> generalize (handles _EntryRights last)
    <*> generalize (handles _EntrySource last)
    <*> generalize (handles _EntrySummary last)
    <*> handlesM _EntryTitle (lastRequired "Missing or invalid <title> element.")
    <*> handlesM _EntryUpdated (lastRequired "Missing or invalid <updated> element.")
  where piece :: (MonadCatch m) => ConduitParser Event m EntryPiece
        piece = choice [ EntryAuthor <$> atomPerson "author"
                       , EntryCategory <$> atomCategory
                       , EntryContent <$> atomContent
                       , EntryContributor <$> atomPerson "contributor"
                       , EntryId <$> atomId
                       , EntryLink <$> atomLink
                       , EntryPublished <$> tagDate "published"
                       , EntryRights <$> atomText "rights"
                       , EntrySource <$> atomSource
                       , EntrySummary <$> atomText "summary"
                       , EntryTitle <$> atomText "title"
                       , EntryUpdated <$> tagDate "updated"
                       , EntryUnknown <$ unknownTag
                       ]


data FeedPiece = FeedAuthor AtomPerson
               | FeedCategory AtomCategory
               | FeedContributor AtomPerson
               | FeedEntry AtomEntry
               | FeedGenerator AtomGenerator
               | FeedIcon AtomURI
               | FeedId (NonNull Text)
               | FeedLink AtomLink
               | FeedLogo AtomURI
               | FeedRights AtomText
               | FeedSubtitle AtomText
               | FeedTitle AtomText
               | FeedUpdated UTCTime
               | FeedUnknown

makeTraversals ''FeedPiece

-- | Parse an @atom:feed@ element.
atomFeed :: (MonadCatch m) => ConduitParser Event m AtomFeed
atomFeed = named "Atom <feed> element" $ tagIgnoreAttrs' "feed" $ do
  p <- many piece
  flip foldM p $ AtomFeed
    <$> generalize (handles _FeedAuthor list)
    <*> generalize (handles _FeedCategory list)
    <*> generalize (handles _FeedContributor list)
    <*> generalize (handles _FeedEntry list)
    <*> generalize (handles _FeedGenerator last)
    <*> generalize (handles _FeedIcon last)
    <*> handlesM _FeedId (lastRequired "Missing or empty <id> element.")
    <*> generalize (handles _FeedLink list)
    <*> generalize (handles _FeedLogo last)
    <*> generalize (handles _FeedRights last)
    <*> generalize (handles _FeedSubtitle last)
    <*> handlesM _FeedTitle (lastRequired "Missing <title> element.")
    <*> handlesM _FeedUpdated (lastRequired "Missing <updated> element.")
  where piece :: MonadCatch m => ConduitParser Event m FeedPiece
        piece = choice [ FeedAuthor <$> atomPerson "author"
                       , FeedCategory <$> atomCategory
                       , FeedContributor <$> atomPerson "contributor"
                       , FeedEntry <$> atomEntry
                       , FeedGenerator <$> atomGenerator
                       , FeedIcon <$> atomIcon
                       , FeedId <$> atomId
                       , FeedLink <$> atomLink
                       , FeedLogo <$> atomLogo
                       , FeedRights <$> atomText "rights"
                       , FeedSubtitle <$> atomText "subtitle"
                       , FeedTitle <$> atomText "title"
                       , FeedUpdated <$> tagDate "updated"
                       , FeedUnknown <$ unknownTag
                       ]
