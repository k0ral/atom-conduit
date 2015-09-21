{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | Streaming renderers for the Atom 1.0 standard.
module Text.Atom.Conduit.Render
  ( -- * Top-level
    renderAtomFeed
    -- * Elements
  , renderAtomEntry
  , renderAtomContent
  , renderAtomSource
  , renderAtomGenerator
  , renderAtomLink
  , renderAtomCategory
    -- * Constructs
  , renderAtomPerson
  , renderAtomText
  ) where

-- {{{ Imports
import           Text.Atom.Types

import           Control.Lens.Getter
import           Control.Monad

import           Data.Conduit
import           Data.Monoid
import           Data.NonNull
import           Data.Text              as Text
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC3339
import           Data.XML.Types

import           Text.XML.Stream.Render

import           URI.ByteString
-- }}}

-- | Render the top-level @atom:feed@ element.
renderAtomFeed :: (Monad m) => AtomFeed -> Source m Event
renderAtomFeed f = tag "feed" (attr "xmlns" "http://www.w3.org/2005/Atom") $ do
  forM_ (f^.feedAuthors_) $ renderAtomPerson "author"
  forM_ (f^.feedCategories_) renderAtomCategory
  forM_ (f^.feedContributors_) $ renderAtomPerson "contributor"
  forM_ (f^.feedEntries_) renderAtomEntry
  forM_ (f^.feedGenerator_) renderAtomGenerator
  forM_ (f^.feedIcon_) $ tag "icon" mempty . content . decodeUtf8 . serializeUriReference'
  tag "id" mempty . content . toNullable $ f^.feedId_
  forM_ (f^.feedLinks_) renderAtomLink
  forM_ (f^.feedLogo_) $ tag "logo" mempty . content . decodeUtf8 . serializeUriReference'
  forM_ (f^.feedRights_) $ renderAtomText "rights"
  forM_ (f^.feedSubtitle_) $ renderAtomText "subtitle"
  renderAtomText "title" $ f^.feedTitle_
  dateTag "updated" $ f^.feedUpdated_

-- | Render an @atom:entry@ element.
renderAtomEntry :: (Monad m) => AtomEntry -> Source m Event
renderAtomEntry e = tag "entry" mempty $ do
  forM_ (e^.entryAuthors_) $ renderAtomPerson "author"
  forM_ (e^.entryCategories_) renderAtomCategory
  forM_ (e^.entryContent_) renderAtomContent
  forM_ (e^.entryContributors_) $ renderAtomPerson "contributor"
  tag "id" mempty . content . toNullable $ e^.entryId_
  forM_ (e^.entryLinks_) renderAtomLink
  forM_ (e^.entryPublished_) $ dateTag "published"
  forM_ (e^.entryRights_) $ renderAtomText "rights"
  forM_ (e^.entrySource_) renderAtomSource
  forM_ (e^.entrySummary_) $ renderAtomText "summary"
  renderAtomText "title" (e^.entryTitle_)
  dateTag "updated" (e^.entryUpdated_)

-- | Render an @atom:content@ element.
renderAtomContent :: (Monad m) => AtomContent -> Source m Event
renderAtomContent (AtomContentInlineXHTML t) = tag "content" (attr "type" "xhtml")
  . tag "div" mempty $ content t
renderAtomContent (AtomContentOutOfLine ctype uri) = tag "content" (nonEmptyAttr "type" ctype <> attr "src" (decodeUtf8 $ serializeUriReference' uri)) $ return ()
renderAtomContent (AtomContentInlineText TypeHTML t) = tag "content" (attr "type" "html") $ content t
renderAtomContent (AtomContentInlineText TypeText t) = tag "content" mempty $ content t
renderAtomContent (AtomContentInlineOther ctype t) = tag "content" (attr "type" ctype) $ content t

-- | Render an @atom:source@ element.
renderAtomSource :: (Monad m) => AtomSource -> Source m Event
renderAtomSource s = tag "source" mempty $ do
  forM_ (s^.sourceAuthors_) $ renderAtomPerson "author"
  forM_ (s^.sourceCategories_) renderAtomCategory
  forM_ (s^.sourceContributors_) $ renderAtomPerson "contributor"
  forM_ (s^.sourceGenerator_) renderAtomGenerator
  forM_ (s^.sourceIcon_) $ tag "icon" mempty . content . decodeUtf8 . serializeUriReference'
  unless (Text.null $ s^.sourceId_) . tag "id" mempty . content $ s^.sourceId_
  forM_ (s^.sourceLinks_) renderAtomLink
  forM_ (s^.sourceLogo_) $ tag "logo" mempty . content . decodeUtf8 . serializeUriReference'
  forM_ (s^.sourceRights_) $ renderAtomText "rights"
  forM_ (s^.sourceSubtitle_) $ renderAtomText "subtitle"
  forM_ (s^.sourceTitle_) $ renderAtomText "title"
  forM_ (s^.sourceUpdated_) $ dateTag "updated"

-- | Render an @atom:generator@ element.
renderAtomGenerator :: (Monad m) => AtomGenerator -> Source m Event
renderAtomGenerator g = tag "generator" attributes . content . toNullable $ g^.generatorContent_
  where attributes = optionalAttr "uri" (decodeUtf8 . serializeUriReference' <$> g^.generatorUri_)
                     <> nonEmptyAttr "version" (g^.generatorVersion_)

-- | Render an @atom:link@ element.
renderAtomLink :: (Monad m) => AtomLink -> Source m Event
renderAtomLink l = tag "link" linkAttrs $ return ()
  where linkAttrs = attr "href" (decodeUtf8 . serializeUriReference' $ l^.linkHref_)
                    <> nonEmptyAttr "rel" (l^.linkRel_)
                    <> nonEmptyAttr "type" (l^.linkType_)
                    <> nonEmptyAttr "hreflang" (l^.linkLang_)
                    <> nonEmptyAttr "title" (l^.linkTitle_)
                    <> nonEmptyAttr "length" (l^.linkLength_)

-- | Render an @atom:category@ element.
renderAtomCategory :: (Monad m) => AtomCategory -> Source m Event
renderAtomCategory c = tag "category" attributes $ return ()
  where attributes = attr "term" (toNullable $ c^.categoryTerm_)
                     <> nonEmptyAttr "scheme" (c^.categoryScheme_)
                     <> nonEmptyAttr "label" (c^.categoryLabel_)

-- | Render an atom person construct.
renderAtomPerson :: (Monad m) => Name -> AtomPerson -> Source m Event
renderAtomPerson name p = tag name mempty $ do
  tag "name" mempty . content . toNullable $ p^.personName_
  unless (Text.null $ p^.personEmail_) $ tag "email" mempty . content $ p^.personEmail_
  forM_ (p^.personUri_) $ tag "uri" mempty . content . decodeUtf8 . serializeUriReference'

-- | Render an atom text construct.
renderAtomText :: (Monad m) => Name -> AtomText -> Source m Event
renderAtomText name (AtomXHTMLText t) = tag name (attr "type" "xhtml")
  . tag "div" mempty $ content t
renderAtomText name (AtomPlainText TypeHTML t) = tag name (attr "type" "html") $ content t
renderAtomText name (AtomPlainText TypeText t) = tag name mempty $ content t


dateTag :: (Monad m) => Name -> UTCTime -> Source m Event
dateTag name = tag name mempty . content . formatTimeRFC3339 . utcToZonedTime utc

nonEmptyAttr :: Name -> Text -> Attributes
nonEmptyAttr name value
  | value == mempty = mempty
  | otherwise = attr name value

-- serializeUriReference' :: UriReference -> ByteString
serializeUriReference' (UriReferenceUri u) = serializeURI' u
serializeUriReference' (UriReferenceRelativeRef r) = serializeRelativeRef' r
