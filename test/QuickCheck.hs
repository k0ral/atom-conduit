{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Data.Char
import           Data.Conduit
import           Data.Text                    as Text
import           Data.Text.Encoding           as Text
import           Data.Time.Clock
import           Data.Void
import           Data.XML.Types
import           Prelude ()
import           Prelude.Compat
import           Refined
import           Test.QuickCheck.Instances
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Text.Atom.Conduit.Parse      as Parser
import           Text.Atom.Conduit.Render     as Renderer
import           Text.Atom.Types
import           URI.ByteString

main :: IO ()
main = defaultMain $ testGroup "Properties"
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


roundtripProperty :: Eq a => Arbitrary a => Show a
                  => TestName -> (a -> ConduitT () Event Maybe ()) -> ConduitM Event Void Maybe (Maybe a) -> TestTree
roundtripProperty name render parse = testProperty ("parse . render = id (" <> name <> ")") $ do
  input <- arbitrary
  let output = join $ runConduit $ render input .| parse
  return $ Just input == output


letter = choose ('a', 'z')
digit = arbitrary `suchThat` isDigit
alphaNum = oneof [letter, digit]

instance Arbitrary (Refined (Not Null) Text) where
  arbitrary = do
    ~(Right text) <- refine <$> arbitrary `suchThat` (not . Text.null)
    return text

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
    ~(Right content) <- refine . pack <$> listOf1 alphaNum
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
