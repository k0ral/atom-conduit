{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
import           Blaze.ByteString.Builder     (toByteString)
import           Conduit                      (foldC, yieldMany)
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Char
import           Data.Conduit
import           Data.Default
import           Data.Functor.Identity
import           Data.Monoid
import           Data.MonoTraversable
import           Data.NonNull
import           Data.Text                    as Text
import           Data.Text.Encoding           as Text
import           Data.Time.Clock
import           Data.Void
import           Data.XML.Types
import           Lens.Simple
import           Test.QuickCheck.Instances
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Text.Atom.Conduit.Parse      as Parser
import           Text.Atom.Conduit.Render     as Renderer
import           Text.Atom.Lens
import           Text.Atom.Types
import           Text.Parser.Combinators
import Text.XML.Stream.Render        (renderBuilder)
import qualified Text.XML.Stream.Parse        as XML
import           URI.ByteString

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ unitTests
  , properties
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
  [ roundtripProperty "AtomText" (renderAtomText "test") (atomText "test")
  , roundtripProperty "AtomPerson" (renderAtomPerson "test") (atomPerson "test")
  , roundtripProperty "AtomCategory" renderAtomCategory atomCategory
  , roundtripProperty "AtomLink" renderAtomLink atomLink
  , roundtripProperty "AtomGenerator" renderAtomGenerator atomGenerator
  , roundtripProperty "AtomSource" renderAtomSource atomSource
  , roundtripProperty "AtomContent" renderAtomContent atomContent
  -- , roundtripProperty "AtomEntry" renderAtomEntry atomEntry
  -- , roundtripProperty "AtomFeed" renderAtomFeed atomFeed
  ]

linkCase :: TestTree
linkCase = testCase "Link element" $ do
  result <- runResourceT . runConduit $ yieldMany input =$= XML.parseText' def =$= XML.force "Invalid <link>" atomLink
  result ^. linkHrefL @?= AtomURI (RelativeRef Nothing "/feed" (Query []) Nothing)
  (result ^. linkRelL) @?= "self"
  where input = ["<link xmlns=\"http://www.w3.org/2005/Atom\" rel=\"self\" href=\"/feed\" />"]

personCase :: TestTree
personCase = testCase "Person construct" $ do
  result <- runResourceT . runConduit $ yieldMany input =$= XML.parseText' def =$= XML.force "Invalid <author>" (atomPerson "author")
  toNullable (result ^. personNameL) @?= "John Doe"
  result ^. personEmailL @?= "JohnDoe@example.com"
  result ^. personUriL @?= Just (AtomURI $ URI (Scheme "http") (Just $ Authority Nothing (Host "example.com") Nothing) "/~johndoe" (Query []) Nothing)
  where input =
          [ "<author xmlns=\"http://www.w3.org/2005/Atom\">"
          , "<name>John Doe</name>"
          , "<email>JohnDoe@example.com</email>"
          , "<uri>http://example.com/~johndoe</uri>"
          , "</author>"
          ]

generatorCase :: TestTree
generatorCase = testCase "Generator element" $ do
  result <- runResourceT . runConduit $ yieldMany input =$= XML.parseText' def =$= XML.force "Invalid <generator>" atomGenerator
  result ^. generatorUriL @?= Just (AtomURI $ RelativeRef Nothing "/myblog.php" (Query []) Nothing)
  (result ^. generatorVersionL) @?= "1.0"
  toNullable (result ^. generatorContentL) @?= "Example Toolkit"
  where input =
          [ "<generator xmlns=\"http://www.w3.org/2005/Atom\" uri=\"/myblog.php\" version=\"1.0\">"
          , "Example Toolkit"
          , "</generator>"
          ]

sourceCase :: TestTree
sourceCase = testCase "Source element" $ do
  result <- runResourceT . runConduit $ yieldMany input =$= XML.parseText' def =$= XML.force "Invalid <source>" atomSource
  (result ^. sourceIdL) @?= "http://example.org/"
  (result ^. sourceTitleL) @?= Just (AtomPlainText TypeText "Fourty-Two")
  show <$> (result ^. sourceUpdatedL) @?= Just "2003-12-13 18:30:02 UTC"
  (result ^. sourceRightsL) @?= Just (AtomPlainText TypeText "© 2005 Example, Inc.")
  where input =
          [ "<source xmlns=\"http://www.w3.org/2005/Atom\">"
          , "<id>http://example.org/</id>"
          , "<title>Fourty-Two</title>"
          , "<updated>2003-12-13T18:30:02Z</updated>"
          , "<rights>© 2005 Example, Inc.</rights>"
          , "</source>"
          ]

textConstructCase :: TestTree
textConstructCase = testCase "Text construct" $ do
  a:b:c:_ <- runResourceT . runConduit $ yieldMany input =$= XML.parseText' def =$= XML.many (atomText "title")
  a @?= AtomPlainText TypeText "AT&T bought by SBC!"
  b @?= AtomPlainText TypeHTML "AT&amp;T bought <b>by SBC</b>!"
  c @?= AtomXHTMLText "AT&amp;T bought <b xmlns=\"http://www.w3.org/1999/xhtml\"><em>by SBC</em></b>&lt;!"
  where input =
          [ "<title xmlns=\"http://www.w3.org/2005/Atom\" type=\"text\">AT&amp;T bought by SBC!</title>"
          , "<title xmlns=\"http://www.w3.org/2005/Atom\" type=\"html\">"
          , "AT&amp;amp;T bought &lt;b&gt;by SBC&lt;/b&gt;!"
          , "</title>"
          , "<title xmlns=\"http://www.w3.org/2005/Atom\" type=\"xhtml\">"
          , "<div xmlns=\"http://www.w3.org/1999/xhtml\">"
          , "AT&amp;T bought <b><em>by SBC</em></b>&lt;!"
          , "</div>"
          , "</title>"
          ]

simpleCase :: TestTree
simpleCase = testCase "Simple case" $ do
  result <- runResourceT . runConduit $ yieldMany input =$= XML.parseText' def =$= XML.force "Invalid <feed>" atomFeed
  return ()
  where input =
          [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
          , "<feed xmlns=\"http://www.w3.org/2005/Atom\">"
          , "<title type=\"text\">&lt;em&gt;Example&lt;/em&gt; Feed</title>"
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


roundtripProperty :: Eq a => Arbitrary a => Show a
                  => TestName -> (a -> Source Maybe Event) -> ConduitM Event Void Maybe (Maybe a) -> TestTree
roundtripProperty name render parse = testProperty ("parse . render = id (" <> name <> ")") $ do
  input <- arbitrary
  let intermediate = fmap (decodeUtf8 . toByteString) $ runConduit $ render input =$= renderBuilder def =$= foldC
      output = join $ runConduit $ render input =$= parse
  return $ counterexample (show input <> " | " <> show intermediate <> " | " <> show output) $ Just input == output


letter = choose ('a', 'z')
digit = arbitrary `suchThat` isDigit
alphaNum = oneof [letter, digit]

instance (MonoFoldable a, Arbitrary a) => Arbitrary (NonNull a) where
  arbitrary = impureNonNull <$> arbitrary `suchThat` (not . onull)

instance Arbitrary Scheme where
  arbitrary = do
    a <- letter
    b <- listOf $ oneof [letter, digit, pure '+', pure '-', pure '.']
    return $ Scheme $ encodeUtf8 $ pack (a:b)

instance Arbitrary Authority where
  arbitrary = Authority <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary UserInfo where
  arbitrary = UserInfo <$> (encodeUtf8 . pack <$> listOf1 alphaNum)
                       <*> (encodeUtf8 . pack <$> listOf1 alphaNum)

instance Arbitrary Host where
  arbitrary = Host <$> (encodeUtf8 . pack <$> listOf1 alphaNum)

instance Arbitrary Port where
  arbitrary = Port <$> (getPositive <$> arbitrary)

instance Arbitrary Query where
  arbitrary = Query <$> listOf ((,) <$> (encodeUtf8 . pack <$> listOf1 alphaNum) <*> (encodeUtf8 . pack <$> listOf1 alphaNum))

instance Arbitrary URI where
  arbitrary = URI <$> arbitrary
                  <*> arbitrary
                  <*> (encodeUtf8 . pack . ('/' :) <$> listOf1 alphaNum)
                  <*> arbitrary
                  <*> oneof [pure Nothing, Just <$> (encodeUtf8 . pack <$> listOf1 alphaNum)]

instance Arbitrary AtomURI where
  arbitrary = oneof [AtomURI <$> (arbitrary :: Gen (URIRef Absolute)), AtomURI <$> (arbitrary :: Gen (URIRef Relative))]

instance Arbitrary RelativeRef where
  arbitrary = RelativeRef <$> arbitrary
                          <*> (encodeUtf8 . pack . ('/' :) <$> listOf1 alphaNum)
                          <*> arbitrary
                          <*> oneof [pure Nothing, Just <$> (encodeUtf8 . pack <$> listOf1 alphaNum)]

instance Arbitrary TextType where
  arbitrary = elements [TypeText, TypeHTML]

instance Arbitrary AtomText where
  arbitrary = oneof
    [ AtomPlainText <$> arbitrary <*> (pack <$> listOf1 alphaNum)
    , AtomXHTMLText <$> (pack <$> listOf1 alphaNum)
    ]
  shrink = genericShrink

instance Arbitrary AtomPerson where
  arbitrary = AtomPerson <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AtomCategory where
  arbitrary = AtomCategory <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AtomLink where
  arbitrary = AtomLink <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AtomGenerator where
  arbitrary = do
    Just content <- fromNullable . pack <$> listOf1 alphaNum
    AtomGenerator <$> arbitrary <*> arbitrary <*> pure content
  shrink = genericShrink

instance Arbitrary AtomSource where
  arbitrary = do
    updated <- oneof [return Nothing, Just <$> genUtcTime]
    AtomSource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> pure updated

instance Arbitrary AtomContent where
  arbitrary = oneof
    [ AtomContentInlineText <$> arbitrary <*> arbitrary
    , AtomContentInlineXHTML <$> (pack <$> listOf1 alphaNum)
    , AtomContentInlineOther <$> arbitrary <*> arbitrary
    , AtomContentOutOfLine <$> arbitrary <*> arbitrary
    ]
  shrink = genericShrink

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
