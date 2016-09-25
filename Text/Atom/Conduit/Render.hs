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
import           Text.Atom.Lens
import           Text.Atom.Types

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

import           Lens.Simple

import           Text.XML.Stream.Render

import           URI.ByteString
-- }}}

-- | Render the top-level @atom:feed@ element.
renderAtomFeed :: (Monad m) => AtomFeed -> Source m Event
renderAtomFeed f = tag "feed" (attr "xmlns" "http://www.w3.org/2005/Atom") $ do
  forM_ (f^.feedAuthorsL) $ renderAtomPerson "author"
  forM_ (f^.feedCategoriesL) renderAtomCategory
  forM_ (f^.feedContributorsL) $ renderAtomPerson "contributor"
  forM_ (f^.feedEntriesL) renderAtomEntry
  forM_ (f^.feedGeneratorL) renderAtomGenerator
  forM_ (feedIcon f) $ tag "icon" mempty . content . decodeUtf8 . withAtomURI serializeURIRef'
  tag "id" mempty . content $ f^.feedIdL
  forM_ (f^.feedLinksL) renderAtomLink
  forM_ (feedLogo f) $ tag "logo" mempty . content . decodeUtf8 . withAtomURI serializeURIRef'
  forM_ (f^.feedRightsL) $ renderAtomText "rights"
  forM_ (f^.feedSubtitleL) $ renderAtomText "subtitle"
  renderAtomText "title" $ f^.feedTitleL
  dateTag "updated" $ f^.feedUpdatedL

-- | Render an @atom:entry@ element.
renderAtomEntry :: (Monad m) => AtomEntry -> Source m Event
renderAtomEntry e = tag "entry" mempty $ do
  forM_ (e^.entryAuthorsL) $ renderAtomPerson "author"
  forM_ (e^.entryCategoriesL) renderAtomCategory
  forM_ (e^.entryContentL) renderAtomContent
  forM_ (e^.entryContributorsL) $ renderAtomPerson "contributor"
  tag "id" mempty . content $ e^.entryIdL
  forM_ (e^.entryLinksL) renderAtomLink
  forM_ (e^.entryPublishedL) $ dateTag "published"
  forM_ (e^.entryRightsL) $ renderAtomText "rights"
  forM_ (e^.entrySourceL) renderAtomSource
  forM_ (e^.entrySummaryL) $ renderAtomText "summary"
  renderAtomText "title" (e^.entryTitleL)
  dateTag "updated" (e^.entryUpdatedL)

-- | Render an @atom:content@ element.
renderAtomContent :: (Monad m) => AtomContent -> Source m Event
renderAtomContent (AtomContentInlineXHTML t) = tag "content" (attr "type" "xhtml")
  . tag "div" mempty $ content t
renderAtomContent (AtomContentOutOfLine ctype uri) = tag "content" (nonEmptyAttr "type" ctype <> attr "src" (decodeUtf8 $ withAtomURI serializeURIRef' uri)) $ return ()
renderAtomContent (AtomContentInlineText TypeHTML t) = tag "content" (attr "type" "html") $ content t
renderAtomContent (AtomContentInlineText TypeText t) = tag "content" mempty $ content t
renderAtomContent (AtomContentInlineOther ctype t) = tag "content" (attr "type" ctype) $ content t

-- | Render an @atom:source@ element.
renderAtomSource :: (Monad m) => AtomSource -> Source m Event
renderAtomSource s = tag "source" mempty $ do
  forM_ (s^.sourceAuthorsL) $ renderAtomPerson "author"
  forM_ (s^.sourceCategoriesL) renderAtomCategory
  forM_ (s^.sourceContributorsL) $ renderAtomPerson "contributor"
  forM_ (s^.sourceGeneratorL) renderAtomGenerator
  forM_ (sourceIcon s) $ tag "icon" mempty . content . decodeUtf8 . withAtomURI serializeURIRef'
  unless (Text.null $ s^.sourceIdL) . tag "id" mempty . content $ s^.sourceIdL
  forM_ (s^.sourceLinksL) renderAtomLink
  forM_ (sourceLogo s) $ tag "logo" mempty . content . decodeUtf8 . withAtomURI serializeURIRef'
  forM_ (s^.sourceRightsL) $ renderAtomText "rights"
  forM_ (s^.sourceSubtitleL) $ renderAtomText "subtitle"
  forM_ (s^.sourceTitleL) $ renderAtomText "title"
  forM_ (s^.sourceUpdatedL) $ dateTag "updated"

-- | Render an @atom:generator@ element.
renderAtomGenerator :: (Monad m) => AtomGenerator -> Source m Event
renderAtomGenerator g = tag "generator" attributes . content . toNullable $ g^.generatorContentL
  where attributes = optionalAttr "uri" (decodeUtf8 . withAtomURI serializeURIRef' <$> generatorUri g)
                     <> nonEmptyAttr "version" (g^.generatorVersionL)

-- | Render an @atom:link@ element.
renderAtomLink :: (Monad m) => AtomLink -> Source m Event
renderAtomLink l = tag "link" linkAttrs $ return ()
  where linkAttrs = attr "href" (decodeUtf8 . withAtomURI serializeURIRef' $ linkHref l)
                    <> nonEmptyAttr "rel" (l^.linkRelL)
                    <> nonEmptyAttr "type" (l^.linkTypeL)
                    <> nonEmptyAttr "hreflang" (l^.linkLangL)
                    <> nonEmptyAttr "title" (l^.linkTitleL)
                    <> nonEmptyAttr "length" (l^.linkLengthL)

-- | Render an @atom:category@ element.
renderAtomCategory :: (Monad m) => AtomCategory -> Source m Event
renderAtomCategory c = tag "category" attributes $ return ()
  where attributes = attr "term" (toNullable $ c^.categoryTermL)
                     <> nonEmptyAttr "scheme" (c^.categorySchemeL)
                     <> nonEmptyAttr "label" (c^.categoryLabelL)

-- | Render an atom person construct.
renderAtomPerson :: (Monad m) => Name -> AtomPerson -> Source m Event
renderAtomPerson name p = tag name mempty $ do
  tag "name" mempty . content . toNullable $ p^.personNameL
  unless (Text.null $ p^.personEmailL) $ tag "email" mempty . content $ p^.personEmailL
  forM_ (personUri p) $ tag "uri" mempty . content . decodeUtf8 . withAtomURI serializeURIRef'

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
