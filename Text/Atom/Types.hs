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
import           Control.Lens.TH

import           Data.NonNull
import           Data.Text           hiding (map)
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.XML.Types

import           GHC.Generics

import           Network.URI
-- }}}

data TextType = TypeText | TypeHTML deriving(Eq, Generic, Show)

-- | An atom text construct.
declarePrisms [d|
  data AtomText = AtomPlainText { atomPlainTextType_ :: TextType, atomPlainTextText_ :: Text }
                  | AtomXHTMLText { atomXHTMLTextDiv_ :: Text }
  |]

deriving instance Eq AtomText
deriving instance Generic AtomText
deriving instance Show AtomText

-- | An atom person construct.
declareLenses [d|
  data AtomPerson = AtomPerson
    { personName_  :: NonNull Text
    , personEmail_ :: Text
    , personUri_   :: Maybe URI
    }
  |]

deriving instance Eq AtomPerson
deriving instance Generic AtomPerson
deriving instance Show AtomPerson

-- | The @atom:category@ element.
declareLenses [d|
  data AtomCategory = AtomCategory
    { categoryTerm_   :: NonNull Text
    , categoryScheme_ :: Text
    , categoryLabel_  :: Text
    }
  |]

deriving instance Eq AtomCategory
deriving instance Generic AtomCategory
deriving instance Show AtomCategory

-- | The @atom:link@ element.
declareLenses [d|
  data AtomLink = AtomLink
    { linkHref_ :: URI
    , linkRel_ :: Text
    , linkType_ :: Text
    , linkLang_ :: Text
    , linkTitle_ :: Text
    , linkLength_ :: Text
    }
  |]

deriving instance Eq AtomLink
deriving instance Generic AtomLink
deriving instance Show AtomLink

-- | The @atom:generator@ element.
declareLenses [d|
  data AtomGenerator = AtomGenerator
    { generatorUri_ :: Maybe URI
    , generatorVersion_ :: Text
    , generatorContent_ :: NonNull Text
    }
  |]

deriving instance Eq AtomGenerator
deriving instance Generic AtomGenerator
deriving instance Show AtomGenerator

-- | The @atom:source@ element.
declareLenses [d|
  data AtomSource = AtomSource
    { sourceAuthors_ :: [AtomPerson]
    , sourceCategories_ :: [AtomCategory]
    , sourceContributors_ :: [AtomPerson]
    , sourceGenerator_ :: Maybe AtomGenerator
    , sourceIcon_ :: Maybe URI
    , sourceId_ :: Text
    , sourceLinks_ :: [AtomLink]
    , sourceLogo_ :: Maybe URI
    , sourceRights_ :: Maybe AtomText
    , sourceSubtitle_ :: Maybe AtomText
    , sourceTitle_ :: Maybe AtomText
    , sourceUpdated_ :: Maybe UTCTime
    }
  |]

deriving instance Eq AtomSource
deriving instance Generic AtomSource
deriving instance Show AtomSource

-- | The @atom:content@ element.
declareLenses [d|
  data AtomContent
    = AtomContentInlineText { atomContentInlineTextType_ :: TextType, atomContentInlineTextText_ :: Text }
    | AtomContentInlineXHTML { atomContentInlineXHTMLDiv_ :: Text }
    | AtomContentInlineOther { atomContentInlineOtherType_ :: Text, atomContentInlineOtherText_ :: Text }
    | AtomContentOutOfLine { atomContentOutOfLineType_ :: Text, atomContentOutOfLineUri_ :: URI }
  |]

deriving instance Eq AtomContent
deriving instance Generic AtomContent
deriving instance Show AtomContent

-- | The @atom:entry@ element.
declareLenses [d|
  data AtomEntry = AtomEntry
    { entryAuthors_ :: [AtomPerson]
    , entryCategories_ :: [AtomCategory]
    , entryContent_ :: Maybe AtomContent
    , entryContributors_ :: [AtomPerson]
    , entryId_ :: NonNull Text
    , entryLinks_ :: [AtomLink]
    , entryPublished_ :: Maybe UTCTime
    , entryRights_ :: Maybe AtomText
    , entrySource_ :: Maybe AtomSource
    , entrySummary_ :: Maybe AtomText
    , entryTitle_ :: AtomText
    , entryUpdated_ :: UTCTime
    }
  |]

deriving instance Eq AtomEntry
deriving instance Generic AtomEntry
deriving instance Show AtomEntry

-- | The @atom:feed@ element.
declareLenses [d|
  data AtomFeed = AtomFeed
    { feedAuthors_ :: [AtomPerson]
    , feedCategories_ :: [AtomCategory]
    , feedContributors_ :: [AtomPerson]
    , feedEntries_ :: [AtomEntry]
    , feedGenerator_ :: Maybe AtomGenerator
    , feedIcon_ :: Maybe URI
    , feedId_ :: NonNull Text
    , feedLinks_ :: [AtomLink]
    , feedLogo_ :: Maybe URI
    , feedRights_ :: Maybe AtomText
    , feedSubtitle_ :: Maybe AtomText
    , feedTitle_ :: AtomText
    , feedUpdated_ :: UTCTime
    }
  |]

deriving instance Eq AtomFeed
deriving instance Generic AtomFeed
deriving instance Show AtomFeed
