{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
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
import           Control.Lens.Cons
import           Control.Lens.Getter
import           Control.Lens.Setter
import           Control.Lens.TH
import           Control.Lens.Tuple
import           Control.Monad
import           Control.Monad.Catch

import           Data.Conduit
import           Data.Conduit.Parser
import           Data.Conduit.Parser.XML
import           Data.Containers
import           Data.Maybe
import           Data.Monoid
import           Data.MonoTraversable
import           Data.NonNull
import           Data.Text               as Text hiding (cons, map, snoc)
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC3339
import           Data.XML.Types

import           GHC.Generics

import           Prelude                 hiding (lookup)

import           Network.URI

import           Text.Parser.Combinators
import           Text.XML
-- }}}

-- | Parse an @atom:feed@ element.
atomFeed :: (MonadCatch m) => ConduitParser Event m AtomFeed
atomFeed = named "Atom <feed> element" $ tagIgnoreAttrs "feed" $ do
  builders <- many $ choice atomFeedBuilders
  let result = (appEndo $ mconcat builders) dummy
  when (checkDummy result) $ unexpected "Missing sub-elements in <feed>."
  return result
  where atomFeedBuilders :: (MonadCatch m) => [ConduitParser Event m (Endo AtomFeed)]
        atomFeedBuilders = [ liftM (Endo . over feedAuthors_ . cons) (atomPerson "author")
                           , liftM (Endo . over feedCategories_ . cons) atomCategory
                           , liftM (Endo . over feedContributors_ . cons) (atomPerson "contributor")
                           , liftM (Endo . over feedEntries_ . cons) atomEntry
                           , liftM (Endo . set feedGenerator_ . Just) atomGenerator
                           , liftM (Endo . set feedIcon_ . Just) atomIcon
                           , liftM (Endo . set feedId_) atomId
                           , liftM (Endo . over feedLinks_ . cons) atomLink
                           , liftM (Endo . set feedLogo_ . Just) atomLogo
                           , liftM (Endo . set feedRights_ . Just) (atomText "rights")
                           , liftM (Endo . set feedSubtitle_ . Just) (atomText "subtitle")
                           , liftM (Endo . set feedTitle_) (atomText "title")
                           , liftM (Endo . set feedUpdated_) (tagDate "updated")
                           , unknownTag
                           ]

-- | Parse an @atom:entry@ element.
atomEntry :: (MonadCatch m) => ConduitParser Event m AtomEntry
atomEntry = named "Atom <entry> element" $ tagIgnoreAttrs "entry" $ do
  builders <- many $ choice atomEntryBuilders
  let result = (appEndo $ mconcat builders) dummy
  when (checkDummy result) $ unexpected "Missing sub-elements in <entry>."
  return result
  where atomEntryBuilders :: (MonadCatch m) => [ConduitParser Event m (Endo AtomEntry)]
        atomEntryBuilders = [ liftM (Endo . over entryAuthors_ . cons) (atomPerson "author")
                            , liftM (Endo . over entryCategories_ . cons) atomCategory
                            , liftM (Endo . set entryContent_ . Just) atomContent
                            , liftM (Endo . over entryContributors_ . cons) (atomPerson "contributor")
                            , liftM (Endo . set entryId_) atomId
                            , liftM (Endo . over entryLinks_ . cons) atomLink
                            , liftM (Endo . set entryPublished_ . Just) (tagDate "published")
                            , liftM (Endo . set entryRights_ . Just) (atomText "rights")
                            , liftM (Endo . set entrySource_ . Just) atomSource
                            , liftM (Endo . set entrySummary_ . Just) (atomText "summary")
                            , liftM (Endo . set entryTitle_) (atomText "title")
                            , liftM (Endo . set entryUpdated_) (tagDate "updated")
                            , unknownTag
                            ]

-- | Parse an @atom:content@ element.
atomContent :: (MonadCatch m) => ConduitParser Event m AtomContent
atomContent = tagName' "content" contentAttrs handler
  where contentAttrs = (,) <$> optional (textAttr "type") <*> optional (attr "src" asURI) <* ignoreAttrs
        handler (Just "xhtml", _) = AtomContentInlineXHTML <$> tagIgnoreAttrs "div" textContent
        handler (ctype, Just uri) = return $ AtomContentOutOfLine (fromMaybe mempty ctype) uri
        handler (Just "html", _) = AtomContentInlineText TypeHTML <$> textContent
        handler (Nothing, _) = AtomContentInlineText TypeText <$> textContent
        handler (Just ctype, _) = AtomContentInlineOther ctype <$> textContent

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
atomSource = named "Atom <source> element" $ tagIgnoreAttrs "source" $ do
  builders <- many $ choice atomSourceBuilders
  return $ (appEndo $ mconcat builders) dummy
  where atomSourceBuilders :: (MonadCatch m) => [ConduitParser Event m (Endo AtomSource)]
        atomSourceBuilders = [ liftM (Endo . over sourceAuthors_ . cons) (atomPerson "author")
                             , liftM (Endo . over sourceCategories_ . cons) atomCategory
                             , liftM (Endo . over sourceContributors_ . cons) (atomPerson "contributor")
                             , liftM (Endo . set sourceGenerator_ . Just) atomGenerator
                             , liftM (Endo . set sourceIcon_ . Just) atomIcon
                             , liftM (Endo . set sourceId_ . toNullable) atomId
                             , liftM (Endo . over sourceLinks_ . cons) atomLink
                             , liftM (Endo . set sourceLogo_ . Just) atomLogo
                             , liftM (Endo . set sourceRights_ . Just) (atomText "rights")
                             , liftM (Endo . set sourceSubtitle_ . Just) (atomText "subtitle")
                             , liftM (Endo . set sourceTitle_ . Just) (atomText "title")
                             , liftM (Endo . set sourceUpdated_ . Just) (tagDate "updated")
                             , unknownTag
                             ]

-- | Parse an @atom:generator@ element.
-- Example:
--
-- > <generator uri="/myblog.php" version="1.0">
-- >   Example Toolkit
-- > </generator>
atomGenerator :: (MonadCatch m) => ConduitParser Event m AtomGenerator
atomGenerator = tagName' "generator" generatorAttrs $ \(uri, version) -> AtomGenerator uri version <$> (asNonNull =<< textContent)
  where generatorAttrs = (,) <$> optional (attr "uri" asURI) <*> (textAttr "version" <|> pure mempty) <* ignoreAttrs

-- | Parse an @atom:link@ element.
-- Examples:
--
-- > <link rel="self" href="/feed" />
--
-- > <link rel="alternate" href="/blog/1234"/>
atomLink :: (MonadCatch m) => ConduitParser Event m AtomLink
atomLink = tagName' "link" linkAttrs $ \(href, rel, ltype, lang, title, length') ->
  return $ AtomLink href rel ltype lang title length'
  where linkAttrs = (,,,,,) <$> attr "href" asURI
                            <*> (textAttr "rel" <|> pure mempty)
                            <*> (textAttr "type" <|> pure mempty)
                            <*> (textAttr "hreflang" <|> pure mempty)
                            <*> (textAttr "title" <|> pure mempty)
                            <*> (textAttr "length" <|> pure mempty)
                            <* ignoreAttrs

-- | Parse an @atom:category@ element.
-- Example:
--
-- > <category term="sports"/>
atomCategory :: (MonadCatch m) => ConduitParser Event m AtomCategory
atomCategory = tagName "category" categoryAttrs $ \(t, s, l) -> do
  term <- asNonNull t
  return $ AtomCategory term s l
  where categoryAttrs = (,,) <$> textAttr "term"
                             <*> (textAttr "scheme" <|> pure mempty)
                             <*> (textAttr "label" <|> pure mempty)
                             <* ignoreAttrs

-- | Parse an Atom person construct.
-- Example:
--
-- > <author>
-- >  <name>John Doe</name>
-- >   <email>JohnDoe@example.com</email>
-- >   <uri>http://example.com/~johndoe</uri>
-- > </author>
atomPerson :: (MonadCatch m) => Text -> ConduitParser Event m AtomPerson
atomPerson name = named ("Atom person construct <" <> name <> ">") $ tagIgnoreAttrs name $ do
  builders <- many $ choice atomPersonBuilders
  case (appEndo $ mconcat builders) (Nothing, "", Nothing) of
    (Just n, e, u) -> return $ AtomPerson n e u
    _ -> unexpected "Missing person name."
  where atomPersonBuilders :: (MonadCatch m) => [ConduitParser Event m (Endo (Maybe (NonNull Text), Text, Maybe URI))]
        atomPersonBuilders = [ liftM (Endo . set _1 . Just) personName
                             , liftM (Endo . set _2) personEmail
                             , liftM (Endo . set _3 . Just) personURI
                             , unknownTag
                             ]

        personName :: MonadCatch m => ConduitParser Event m (NonNull Text)
        personName = tagIgnoreAttrs "name" (content asNonNull)

        personEmail :: MonadCatch m => ConduitParser Event m Text
        personEmail = tagIgnoreAttrs "email" textContent

        personURI :: MonadCatch m => ConduitParser Event m URI
        personURI = tagIgnoreAttrs "uri" $ content asURI

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
  where handler (Just "xhtml") = AtomXHTMLText <$> tagIgnoreAttrs "div" xhtmlContent
        handler (Just "html") = AtomPlainText TypeHTML <$> textContent
        handler _ = AtomPlainText TypeText <$> textContent
        xhtmlContent :: MonadCatch m => ConduitParser Event m Text
        xhtmlContent = mconcat <$> many (textContent <|> anyTag (\name attrs -> renderTag name attrs <$> xhtmlContent))
        renderTag name attrs content = "<" <> nameLocalName name <> renderAttrs attrs <> ">" <> content <> "</" <> nameLocalName name <> ">"
        renderAttrs [] = ""
        renderAttrs ((name, content):t) = " " <> nameLocalName name <> "=\"" <> mconcat (renderContent <$> content) <> "\"" <> renderAttrs t
        renderContent (ContentText t) = t
        renderContent (ContentEntity t) = t


data AtomException = InvalidURI Text
                   | MissingEntryTitle
                   | MissingEntryUpdated
                   | NullElement
                   | EmptyList

deriving instance Eq AtomException
instance Show AtomException where
  show (InvalidURI t) = "Invalid URI: " ++ unpack t
  show MissingEntryTitle = "Missing entry title."
  show MissingEntryUpdated = "Missing entry's last update."
  show NullElement = "Null element."
  show EmptyList = "Empty list."
instance Exception AtomException


asURI :: (MonadThrow m) => Text -> m URI
asURI t = maybe (throwM $ InvalidURI t) return . parseURIReference $ unpack t

asNonNull :: (MonoFoldable a, MonadThrow m) => a -> m (NonNull a)
asNonNull = maybe (throwM NullElement) return . fromNullable

-- | Like 'tagName' but ignores the namespace.
tagName' :: (MonadCatch m) => Text -> AttrParser a -> (a -> ConduitParser Event m b) -> ConduitParser Event m b
tagName' t = tagPredicate (\n -> nameLocalName n == t)

-- | Tag which content is a date-time that follows RFC 3339 format.
tagDate :: (MonadCatch m) => Text -> ConduitParser Event m UTCTime
tagDate name = tagIgnoreAttrs name $ content (fmap zonedTimeToUTC . parseTimeRFC3339)

-- | Like 'tagName'' but ignores all attributes.
tagIgnoreAttrs :: (MonadCatch m) => Text -> ConduitParser Event m a -> ConduitParser Event m a
tagIgnoreAttrs name handler = tagName' name ignoreAttrs $ const handler


atomId :: (MonadCatch m) => ConduitParser Event m (NonNull Text)
atomId = tagIgnoreAttrs "id" $ content asNonNull

atomIcon, atomLogo :: (MonadCatch m) => ConduitParser Event m URI
atomIcon = tagIgnoreAttrs "icon" $ content asURI
atomLogo = tagIgnoreAttrs "logo" $ content asURI

unknownTag :: (MonadCatch m) => ConduitParser Event m (Endo a)
unknownTag = anyTag $ \_ _ -> many (void unknownTag <|> void textContent) >> return mempty


class (Eq a) => Dummy a where
  dummy :: a
  checkDummy :: a -> Bool
  checkDummy = (== dummy)

instance Dummy UTCTime where
  dummy = UTCTime (toEnum 0) (secondsToDiffTime 0)
instance Dummy Text where
  dummy = " "
instance Dummy AtomText where
  dummy = AtomPlainText TypeText dummy
instance Dummy (NonNull Text) where
  dummy = nonNull dummy
instance Dummy AtomSource where
  dummy = AtomSource mzero mzero mzero mzero mzero mempty mzero mzero mzero mzero mzero mzero
instance Dummy AtomEntry where
  dummy = AtomEntry mzero mzero mzero mzero dummy mzero mzero mzero mzero mzero dummy dummy
  checkDummy (AtomEntry _ _ _ _ i _ _ _ _ _ t u) = checkDummy i && checkDummy t && checkDummy u
instance Dummy AtomFeed where
  dummy = AtomFeed mzero mzero mzero mzero mzero mzero dummy mzero mzero mzero mzero dummy dummy
  checkDummy (AtomFeed _ _ _ _ _ _ i _ _ _ _ t u) = checkDummy i && checkDummy t && checkDummy u
