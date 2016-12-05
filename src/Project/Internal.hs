{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Project.Internal ( exampleFixedText
                        , exampleOverFlowProtection
                        , exampleInvalidChar
                        , exampleUnderFlowProtection
                        , fixedTextFromString
                        , fixedTextFromText
                        , fixedTextToString
                        , fixedTextToText
                        , unsafeFixedTextFromText
                        , FixedText(..)
                        , FixedTextError (..)
                        , FixedTextErrors) where

import Data.Text (pack,Text)
import qualified Data.Text as Text
import Data.Proxy (Proxy(..))
import Data.Aeson
import GHC.TypeLits (Nat,natVal,KnownNat,Symbol,KnownSymbol,symbolVal)
import Data.Semigroup
import Text.Regex.Lens
import Text.Regex.Base
import Text.Regex.Posix
import Control.Lens



-- | Set of things that can go wrong with Fixed Text construction
data FixedTextError = FixedTextErrorMin
                     | FixedTextErrorRegex String String
                     | FixedTextErrorMax
  deriving (Show,Eq,Ord)

type FixedTextErrors = [FixedTextError]

-- | Text array with max size and min size and character set
newtype  FixedText (lengthMax :: Nat)
                   (lengthMin :: Nat)
                   (regex     :: Symbol) 
           = FixedText { _unFixedText :: Text}
  deriving (Show,Ord,Eq)

-- | JSON instances for config files
instance (KnownNat max, KnownNat min, KnownSymbol regex) => FromJSON (FixedText (max::Nat) (min :: Nat) (regex :: Symbol)) where
     parseJSON (String txt) = either (fail.show) pure $ fixedTextFromText txt
     parseJSON _             = fail $ "Expecting FixedText max: " <> (show.natVal)    (Proxy :: Proxy max)   <>
                                                          "min: " <> (show.natVal)    (Proxy :: Proxy min)   <>
                                                        "regex: " <> (show.symbolVal) (Proxy :: Proxy regex) 



instance  ToJSON (FixedText (max::Nat) (min :: Nat) (regex :: Symbol)) where
  toJSON (FixedText txt) = toJSON txt


-- | String version of the Smart Constructor
fixedTextFromString :: forall max min regex . 
  ( KnownNat    max
  , KnownNat    min
  , KnownSymbol regex) => String -> Either FixedTextErrors (FixedText max min regex)
fixedTextFromString str = final
  where
    max'          = fromIntegral $ natVal (Proxy :: Proxy max)
    min'          = fromIntegral $ natVal (Proxy :: Proxy min)    
    isTooLittle   = length str < min'
    regexStr      = symbolVal (Proxy :: Proxy regex)
    trimmedString = take max' str
    notRegex      = notValidRegex regexStr trimmedString
    final
      | isTooLittle = Left   [FixedTextErrorMin]
      | notRegex    = Left [(FixedTextErrorRegex regexStr trimmedString)]
      | otherwise   = (Right . FixedText .   pack) trimmedString  


-- | Text version of the Smart Constructor
fixedTextFromText :: forall max min regex . 
  ( KnownNat    max
  , KnownNat    min
  , KnownSymbol regex) => Text -> Either FixedTextErrors (FixedText max min regex)
fixedTextFromText txt = final
  where
    max'          = fromIntegral $ natVal (Proxy :: Proxy max)
    min'          = fromIntegral $ natVal (Proxy :: Proxy min)    
    isTooLittle   = Text.length txt < min'
    regexStr      = symbolVal (Proxy :: Proxy regex)
    trimmedText   = Text.take max' txt
    notRegex      = notValidRegex regexStr (Text.unpack trimmedText)
    final
      | isTooLittle = Left   [FixedTextErrorMin]
      | notRegex    = Left [(FixedTextErrorRegex regexStr (Text.unpack trimmedText))]
      | otherwise   = (Right . FixedText  ) trimmedText  


-- | Use with caution but it is nice especially when building
--   String literals hard
--  use msg to localize the error message
unsafeFixedTextFromText :: forall max min regex . 
  ( KnownNat    max
  , KnownNat    min
  , KnownSymbol regex) => String -> Text -> (FixedText max min regex)
unsafeFixedTextFromText msg txt = performUnsafePart
  where
   performUnsafePart = (either (error.(msg <>).show) id ) eitherFixedText
   eitherFixedText   = fixedTextFromText txt



fixedTextToText :: FixedText max min regex -> Text
fixedTextToText (FixedText txt) = txt



fixedTextToString :: FixedText max min regex -> String
fixedTextToString (FixedText txt) = Text.unpack txt



notValidRegex :: String -> String -> Bool
notValidRegex regexStr txt =  regexPart /= txt
  where
    regexPart     = txt ^. regex compiledRegex . matchedString
    compiledRegex :: Regex
    compiledRegex = makeRegex regexStr



-- | A few examples to make sure everything works right...
-- Working input
exampleFixedText  :: Either FixedTextErrors (FixedText 30 0 "[[:alnum:]]")
exampleFixedText = fixedTextFromString "exampleText1234" 


-- | Cut off too much input.
exampleOverFlowProtection :: Either FixedTextErrors (FixedText 10 1 "[[:alnum:]]")
exampleOverFlowProtection = fixedTextFromString "exampleText1234" 


-- | Reject if below min input
exampleUnderFlowProtection :: Either FixedTextErrors (FixedText 200 20 "[[:alnum:]]")
exampleUnderFlowProtection = fixedTextFromString "exampleText1234"


-- | Reject if invalid char
exampleInvalidChar :: Either FixedTextErrors (FixedText 30 1 "[[:digit:]]")
exampleInvalidChar = fixedTextFromString "exampleNotAllDigits"


-- | Instances to define
--   Monoid instance with 0 min
-- No FixedText besides one that has a minimum size of zero
-- should be a Monoid.
instance ( KnownNat max
         , KnownSymbol regex) => 
 Monoid (FixedText (max::Nat) (0::Nat) (regex::Symbol)) where
  mempty                      = FixedText ""
  mappend s1@(FixedText str1)
             (FixedText str2) = either (const s1)
                                        id
                                        (fixedTextFromText (str1 <> str2))



instance ( KnownNat max
         , KnownNat min
         , KnownSymbol regex) =>  Semigroup (FixedText (max::Nat) (min::Nat) (regex::Symbol)) where
  (<>) s1@(FixedText str1)
                 (FixedText str2) = either (const s1)
                                            id
                                            (fixedTextFromText (str1 <> str2))







