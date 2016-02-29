{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE StandaloneDeriving     #-}
-- | Atom is an XML-based Web content and metadata syndication format.
--
-- Example:
--
-- > <?xml version="1.0" encoding="utf-8"?>
-- > <feed xmlns="http://www.w3.org/2005/Atom">
-- >
-- >   <title>Example Feed</title>
-- >   <link href="http://example.org/"/>
-- >   <updated>2003-12-13T18:30:02Z</updated>
-- >   <author>
-- >     <name>John Doe</name>
-- >   </author>
-- >   <id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>
-- >
-- >   <entry>
-- >     <title>Atom-Powered Robots Run Amok</title>
-- >     <link href="http://example.org/2003/12/13/atom03"/>
-- >     <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
-- >     <updated>2003-12-13T18:30:02Z</updated>
-- >     <summary>Some text.</summary>
-- >   </entry>
-- >
-- > </feed>
module Text.Atom.Types where

-- {{{ Imports
import           Data.NonNull
import           Data.Text           hiding (map)
import           Data.Time.Clock
import           Data.Time.LocalTime ()

import           GHC.Generics

import           URI.ByteString
-- }}}


data AtomURI = forall a . AtomURI (URIRef a)

withAtomURI :: (forall a . URIRef a -> b) -> AtomURI -> b
withAtomURI f (AtomURI a) = f a


instance Eq AtomURI where
  AtomURI a@URI{} == AtomURI b@URI{} = a == b
  AtomURI a@RelativeRef{} == AtomURI b@RelativeRef{} = a == b
  _ == _ = False
instance Show AtomURI where
  show (AtomURI a@URI{}) = show a
  show (AtomURI a@RelativeRef{}) = show a


data TextType = TypeText | TypeHTML
  deriving(Eq, Generic, Show)

-- | An atom text construct.
data AtomText = AtomPlainText TextType Text
              | AtomXHTMLText Text

deriving instance Eq AtomText
deriving instance Generic AtomText
deriving instance Show AtomText

-- | An atom person construct.
data AtomPerson = AtomPerson
  { personName  :: NonNull Text
  , personEmail :: Text
  , personUri   :: Maybe AtomURI
  }

deriving instance Eq AtomPerson
deriving instance Generic AtomPerson
deriving instance Show AtomPerson

-- | The @atom:category@ element.
data AtomCategory = AtomCategory
  { categoryTerm   :: NonNull Text
  , categoryScheme :: Text
  , categoryLabel  :: Text
  }

deriving instance Eq AtomCategory
deriving instance Generic AtomCategory
deriving instance Show AtomCategory

-- | The @atom:link@ element.
data AtomLink = AtomLink
  { linkHref   :: AtomURI
  , linkRel    :: Text
  , linkType   :: Text
  , linkLang   :: Text
  , linkTitle  :: Text
  , linkLength :: Text
  }

deriving instance Eq AtomLink
deriving instance Generic AtomLink
deriving instance Show AtomLink

-- | The @atom:generator@ element.
data AtomGenerator = AtomGenerator
  { generatorUri     :: Maybe AtomURI
  , generatorVersion :: Text
  , generatorContent :: NonNull Text
  }

deriving instance Eq AtomGenerator
deriving instance Generic AtomGenerator
deriving instance Show AtomGenerator

-- | The @atom:source@ element.
data AtomSource = AtomSource
  { sourceAuthors      :: [AtomPerson]
  , sourceCategories   :: [AtomCategory]
  , sourceContributors :: [AtomPerson]
  , sourceGenerator    :: Maybe AtomGenerator
  , sourceIcon         :: Maybe AtomURI
  , sourceId           :: Text
  , sourceLinks        :: [AtomLink]
  , sourceLogo         :: Maybe AtomURI
  , sourceRights       :: Maybe AtomText
  , sourceSubtitle     :: Maybe AtomText
  , sourceTitle        :: Maybe AtomText
  , sourceUpdated      :: Maybe UTCTime
  }

deriving instance Eq AtomSource
deriving instance Generic AtomSource
deriving instance Show AtomSource

type Type = Text

-- | The @atom:content@ element.
data AtomContent = AtomContentInlineText TextType Text
                 | AtomContentInlineXHTML Text
                 | AtomContentInlineOther Type Text
                 | AtomContentOutOfLine Type AtomURI

deriving instance Eq AtomContent
deriving instance Generic AtomContent
deriving instance Show AtomContent

-- | The @atom:entry@ element.
data AtomEntry = AtomEntry
  { entryAuthors      :: [AtomPerson]
  , entryCategories   :: [AtomCategory]
  , entryContent      :: Maybe AtomContent
  , entryContributors :: [AtomPerson]
  , entryId           :: NonNull Text
  , entryLinks        :: [AtomLink]
  , entryPublished    :: Maybe UTCTime
  , entryRights       :: Maybe AtomText
  , entrySource       :: Maybe AtomSource
  , entrySummary      :: Maybe AtomText
  , entryTitle        :: AtomText
  , entryUpdated      :: UTCTime
  }

deriving instance Eq AtomEntry
deriving instance Generic AtomEntry
deriving instance Show AtomEntry

-- | The @atom:feed@ element.
data AtomFeed = AtomFeed
  { feedAuthors      :: [AtomPerson]
  , feedCategories   :: [AtomCategory]
  , feedContributors :: [AtomPerson]
  , feedEntries      :: [AtomEntry]
  , feedGenerator    :: Maybe AtomGenerator
  , feedIcon         :: Maybe AtomURI
  , feedId           :: NonNull Text
  , feedLinks        :: [AtomLink]
  , feedLogo         :: Maybe AtomURI
  , feedRights       :: Maybe AtomText
  , feedSubtitle     :: Maybe AtomText
  , feedTitle        :: AtomText
  , feedUpdated      :: UTCTime
  }

deriving instance Eq AtomFeed
deriving instance Generic AtomFeed
deriving instance Show AtomFeed
