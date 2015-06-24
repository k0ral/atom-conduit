{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Lens.Getter
import           Control.Monad
import           Control.Monad.Catch.Pure
import           Control.Monad.Trans.Resource

import           Data.Char
import           Data.Conduit
import           Data.Conduit.List
import           Data.Conduit.Parser
import           Data.Conduit.Parser.XML      as XML
import           Data.Default
import           Data.Functor.Identity
import           Data.MinLen
import           Data.Monoid
import           Data.MonoTraversable
import           Data.NonNull
import           Data.Text                    as Text
import           Data.Time.Clock
import           Data.XML.Types

import           Network.URI

import qualified Language.Haskell.HLint       as HLint (hlint)
import           Test.QuickCheck.Instances
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Text.Atom.Conduit.Parse      as Parser
import           Text.Atom.Conduit.Render     as Renderer
import           Text.Atom.Types
import           Text.Parser.Combinators


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ unitTests
  , properties
  , hlint
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ linkCase
  , personCase
  , generatorCase
  , sourceCase
  , textConstructCase
  , simpleCase
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ inverseAtomTextProperty
  , inverseAtomPersonProperty
  , inverseAtomCategoryProperty
  , inverseAtomLinkProperty
  , inverseAtomGeneratorProperty
  , inverseAtomSourceProperty
  , inverseAtomContentProperty
  , inverseAtomEntryProperty
  -- , inverseAtomFeedProperty
  ]

linkCase :: TestTree
linkCase = testCase "Link element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser atomLink
  show (result ^. linkHref_) @?= "/feed"
  (result ^. linkRel_) @?= "self"
  where input = ["<link rel=\"self\" href=\"/feed\" />"]

personCase :: TestTree
personCase = testCase "Person construct" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser (atomPerson "author")
  toNullable (result ^. personName_) @?= "John Doe"
  result ^. personEmail_ @?= "JohnDoe@example.com"
  show <$> (result ^. personUri_) @?= Just "http://example.com/~johndoe"
  where input =
          [ "<author>"
          , "<name>John Doe</name>"
          , "<email>JohnDoe@example.com</email>"
          , "<uri>http://example.com/~johndoe</uri>"
          , "</author>"
          ]

generatorCase :: TestTree
generatorCase = testCase "Generator element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser atomGenerator
  show <$> (result ^. generatorUri_) @?= Just "/myblog.php"
  (result ^. generatorVersion_) @?= "1.0"
  toNullable (result ^. generatorContent_) @?= "Example Toolkit"
  where input =
          [ "<generator uri=\"/myblog.php\" version=\"1.0\">"
          , "Example Toolkit"
          , "</generator>"
          ]

sourceCase :: TestTree
sourceCase = testCase "Source element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser atomSource
  (result ^. sourceId_) @?= "http://example.org/"
  (result ^. sourceTitle_) @?= Just (AtomPlainText TypeText "Fourty-Two")
  show <$> (result ^. sourceUpdated_) @?= Just "2003-12-13 18:30:02 UTC"
  (result ^. sourceRights_) @?= Just (AtomPlainText TypeText "© 2005 Example, Inc.")
  where input =
          [ "<source>"
          , "<id>http://example.org/</id>"
          , "<title>Fourty-Two</title>"
          , "<updated>2003-12-13T18:30:02Z</updated>"
          , "<rights>© 2005 Example, Inc.</rights>"
          , "</source>"
          ]

textConstructCase :: TestTree
textConstructCase = testCase "Text construct" $ do
  (a, b, c) <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser ((,,) <$> atomText "title1" <*> atomText "title2" <*> atomText "title3")
  a @?= AtomPlainText TypeText "AT&T bought by SBC!"
  b @?= AtomPlainText TypeHTML "AT&amp;T bought <b>by SBC</b>!"
  c @?= AtomXHTMLText "AT&T bought <b>by SBC</b>!"
  where input =
          [ "<title1 type=\"text\">AT&amp;T bought by SBC!</title1>"
          , "<title2 type=\"html\">"
          , "AT&amp;amp;T bought &lt;b&gt;by SBC&lt;/b&gt;!"
          , "</title2>"
          , "<title3 type=\"xhtml\">"
          , "<div xmlns=\"http://www.w3.org/1999/xhtml\">"
          , "AT&amp;T bought <b>by SBC</b>!"
          , "</div>"
          , "</title3>"
          ]

simpleCase :: TestTree
simpleCase = testCase "Simple case" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser atomFeed
  return ()
  where input =
          [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
          , "<feed xmlns=\"http://www.w3.org/2005/Atom\">"
          , "<title>Example Feed</title>"
          , "<link href=\"http://example.org/\"/>"
          , "<updated>2003-12-13T18:30:02Z</updated>"
          , "<author>"
          , "<name>John Doe</name>"
          , "</author>"
          , "<id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>"
          , "<entry>"
          , "<title>Atom-Powered Robots Run Amok</title>"
          , "<link href=\"http://example.org/2003/12/13/atom03\"/>"
          , "<id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>"
          , "<updated>2003-12-13T18:30:02Z</updated>"
          , "<summary>Some text.</summary>"
          , "</entry>"
          , "</feed>"
          ]


hlint :: TestTree
hlint = testCase "HLint check" $ do
  result <- HLint.hlint [ "test/", "Text/" ]
  Prelude.null result @?= True


inverseAtomTextProperty :: TestTree
inverseAtomTextProperty = testProperty "parse . render = id (AtomText)" $ \i -> either (const False) (i ==) (runIdentity . runCatchT . runConduit $ renderAtomText "test" i =$= runConduitParser (atomText "test"))

inverseAtomPersonProperty :: TestTree
inverseAtomPersonProperty = testProperty "parse . render = id (AtomPerson)" $ \i -> either (const False) (i ==) (runIdentity . runCatchT . runConduit $ renderAtomPerson "test" i =$= runConduitParser (atomPerson "test"))

inverseAtomCategoryProperty :: TestTree
inverseAtomCategoryProperty = testProperty "parse . render = id (AtomCategory)" $ \i -> either (const False) (i ==) (runIdentity . runCatchT . runConduit $ renderAtomCategory i =$= runConduitParser atomCategory)

inverseAtomLinkProperty :: TestTree
inverseAtomLinkProperty = testProperty "parse . render = id (AtomLink)" $ \i -> either (const False) (i ==) (runIdentity . runCatchT . runConduit $ renderAtomLink i =$= runConduitParser atomLink)

inverseAtomGeneratorProperty :: TestTree
inverseAtomGeneratorProperty = testProperty "parse . render = id (AtomGenerator)" $ \i -> either (const False) (i ==) (runIdentity . runCatchT . runConduit $ renderAtomGenerator i =$= runConduitParser atomGenerator)

inverseAtomSourceProperty :: TestTree
inverseAtomSourceProperty = testProperty "parse . render = id (AtomSource)" $ \i -> either (const False) (i ==) (runIdentity . runCatchT . runConduit $ renderAtomSource i =$= runConduitParser atomSource)

inverseAtomContentProperty :: TestTree
inverseAtomContentProperty = testProperty "parse . render = id (AtomContent)" $ \i -> either (const False) (i ==) (runIdentity . runCatchT . runConduit $ renderAtomContent i =$= runConduitParser atomContent)

inverseAtomFeedProperty :: TestTree
inverseAtomFeedProperty = testProperty "parse . render = id (AtomFeed)" $ \i -> either (const False) (i ==) (runIdentity . runCatchT . runConduit $ renderAtomFeed i =$= runConduitParser atomFeed)

inverseAtomEntryProperty :: TestTree
inverseAtomEntryProperty = testProperty "parse . render = id (AtomEntry)" $ \i -> either (const False) (i ==) (runIdentity . runCatchT . runConduit $ renderAtomEntry i =$= runConduitParser atomEntry)


alphaNum = oneof [choose('a', 'z'), suchThat arbitrary isDigit]

instance (MonoFoldable a, Arbitrary a) => Arbitrary (MinLen (Succ Zero) a) where
  arbitrary = nonNull <$> arbitrary `suchThat` (not . onull)

instance Arbitrary URIAuth where
  arbitrary = do
    userInfo <- oneof [return "", (++ "@") <$> listOf1 alphaNum]
    regName <- listOf1 alphaNum
    port <- oneof [return "", (":" ++) . show <$> choose(1 :: Int, 65535)]
    return $ URIAuth userInfo regName port

instance Arbitrary URI where
  arbitrary = do
    scheme <- (++ ":") <$> listOf1 (choose('a', 'z'))
    path <- ("/" ++) <$> listOf1 alphaNum
    query <- oneof [return "", ("?" ++) <$> listOf1 alphaNum]
    fragment <- oneof [return "", ("#" ++) <$> listOf1 alphaNum]
    authority <- arbitrary
    return $ URI scheme authority path query fragment

instance Arbitrary TextType where
  arbitrary = elements [TypeText, TypeHTML]
  -- shrink = genericShrink

instance Arbitrary AtomText where
  arbitrary = oneof
    [ AtomPlainText <$> arbitrary <*> arbitrary
    , AtomXHTMLText <$> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary AtomPerson where
  arbitrary = AtomPerson <$> arbitrary <*> arbitrary <*> arbitrary
  -- shrink = genericShrink

instance Arbitrary AtomCategory where
  arbitrary = AtomCategory <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AtomLink where
  arbitrary = AtomLink <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AtomGenerator where
  arbitrary = AtomGenerator <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary AtomSource where
  arbitrary = do
    updated <- oneof [return Nothing, Just <$> genUtcTime]
    AtomSource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> pure updated
  -- shrink = genericShrink

instance Arbitrary AtomContent where
  arbitrary = oneof
    [ AtomContentInlineText <$> arbitrary <*> arbitrary
    , AtomContentInlineXHTML <$> arbitrary
    , AtomContentInlineOther <$> arbitrary <*> arbitrary
    , AtomContentOutOfLine <$> arbitrary <*> arbitrary
    ]

instance Arbitrary AtomEntry where
  arbitrary = do
    published <- oneof [return Nothing, Just <$> genUtcTime]
    AtomEntry <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> pure published <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> genUtcTime

instance Arbitrary AtomFeed where
  arbitrary = AtomFeed <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> genUtcTime

-- | Generates 'UTCTime' with rounded seconds.
genUtcTime = do
  (UTCTime d s) <- arbitrary
  return $ UTCTime d (fromIntegral (round s :: Int))
