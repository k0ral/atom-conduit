{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
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
import           Control.Lens.Combinators

import           Data.NonNull
import           Data.Text                hiding (map)
import           Data.Time.Clock
import           Data.Time.LocalTime      ()

import           GHC.Generics

import           URI.ByteString
-- }}}

data TextType = TypeText | TypeHTML
  deriving(Eq, Generic, Show)

-- | Either a 'URI', or a 'RelativeRef' (as defined by RFC 3986)
data UriReference = UriReferenceUri URI | UriReferenceRelativeRef RelativeRef
  deriving(Eq, Generic, Show)

-- | An atom text construct.
data AtomText = AtomPlainText TextType Text
              | AtomXHTMLText Text

makePrisms ''AtomText

deriving instance Eq AtomText
deriving instance Generic AtomText
deriving instance Show AtomText

-- | An atom person construct.
data AtomPerson = AtomPerson
  { _personName_  :: NonNull Text
  , _personEmail_ :: Text
  , _personUri_   :: Maybe UriReference
  }

makeLenses ''AtomPerson

deriving instance Eq AtomPerson
deriving instance Generic AtomPerson
deriving instance Show AtomPerson

-- | The @atom:category@ element.
data AtomCategory = AtomCategory
  { _categoryTerm_   :: NonNull Text
  , _categoryScheme_ :: Text
  , _categoryLabel_  :: Text
  }

makeLenses ''AtomCategory

deriving instance Eq AtomCategory
deriving instance Generic AtomCategory
deriving instance Show AtomCategory

-- | The @atom:link@ element.
data AtomLink = AtomLink
  { _linkHref_   :: UriReference
  , _linkRel_    :: Text
  , _linkType_   :: Text
  , _linkLang_   :: Text
  , _linkTitle_  :: Text
  , _linkLength_ :: Text
  }

makeLenses ''AtomLink

deriving instance Eq AtomLink
deriving instance Generic AtomLink
deriving instance Show AtomLink

-- | The @atom:generator@ element.
data AtomGenerator = AtomGenerator
  { _generatorUri_     :: Maybe UriReference
  , _generatorVersion_ :: Text
  , _generatorContent_ :: NonNull Text
  }

makeLenses ''AtomGenerator

deriving instance Eq AtomGenerator
deriving instance Generic AtomGenerator
deriving instance Show AtomGenerator

-- | The @atom:source@ element.
data AtomSource = AtomSource
  { _sourceAuthors_      :: [AtomPerson]
  , _sourceCategories_   :: [AtomCategory]
  , _sourceContributors_ :: [AtomPerson]
  , _sourceGenerator_    :: Maybe AtomGenerator
  , _sourceIcon_         :: Maybe UriReference
  , _sourceId_           :: Text
  , _sourceLinks_        :: [AtomLink]
  , _sourceLogo_         :: Maybe UriReference
  , _sourceRights_       :: Maybe AtomText
  , _sourceSubtitle_     :: Maybe AtomText
  , _sourceTitle_        :: Maybe AtomText
  , _sourceUpdated_      :: Maybe UTCTime
  }

makeLenses ''AtomSource

deriving instance Eq AtomSource
deriving instance Generic AtomSource
deriving instance Show AtomSource

type Type = Text

-- | The @atom:content@ element.
data AtomContent = AtomContentInlineText TextType Text
                 | AtomContentInlineXHTML Text
                 | AtomContentInlineOther Type Text
                 | AtomContentOutOfLine Type UriReference

deriving instance Eq AtomContent
deriving instance Generic AtomContent
deriving instance Show AtomContent

-- | The @atom:entry@ element.
data AtomEntry = AtomEntry
  { _entryAuthors_      :: [AtomPerson]
  , _entryCategories_   :: [AtomCategory]
  , _entryContent_      :: Maybe AtomContent
  , _entryContributors_ :: [AtomPerson]
  , _entryId_           :: NonNull Text
  , _entryLinks_        :: [AtomLink]
  , _entryPublished_    :: Maybe UTCTime
  , _entryRights_       :: Maybe AtomText
  , _entrySource_       :: Maybe AtomSource
  , _entrySummary_      :: Maybe AtomText
  , _entryTitle_        :: AtomText
  , _entryUpdated_      :: UTCTime
  }

makeLenses ''AtomEntry

deriving instance Eq AtomEntry
deriving instance Generic AtomEntry
deriving instance Show AtomEntry

-- | The @atom:feed@ element.
data AtomFeed = AtomFeed
  { _feedAuthors_      :: [AtomPerson]
  , _feedCategories_   :: [AtomCategory]
  , _feedContributors_ :: [AtomPerson]
  , _feedEntries_      :: [AtomEntry]
  , _feedGenerator_    :: Maybe AtomGenerator
  , _feedIcon_         :: Maybe UriReference
  , _feedId_           :: NonNull Text
  , _feedLinks_        :: [AtomLink]
  , _feedLogo_         :: Maybe UriReference
  , _feedRights_       :: Maybe AtomText
  , _feedSubtitle_     :: Maybe AtomText
  , _feedTitle_        :: AtomText
  , _feedUpdated_      :: UTCTime
  }

makeLenses ''AtomFeed

deriving instance Eq AtomFeed
deriving instance Generic AtomFeed
deriving instance Show AtomFeed
