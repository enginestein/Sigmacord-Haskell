{-# LANGUAGE DeriveDataTypeable #-}


module Sigmacord.Internal.Types.Color where


import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Data.Aeson
import Data.Data
import Control.Applicative (Alternative((<|>)))
import Data.Bits (Bits((.&.)))


import Sigmacord.Internal.Types.Prelude (InternalSigmacordEnum(..))







data SigmacordColor
  = 
    SigmacordColorRGB Integer Integer Integer
  | SigmacordColorDefault
  | SigmacordColorAqua
  | SigmacordColorDarkAqua
  | SigmacordColorGreen
  | SigmacordColorDarkGreen
  | SigmacordColorBlue
  | SigmacordColorDarkBlue
  | SigmacordColorPurple
  | SigmacordColorDarkPurple
  | SigmacordColorLuminousVividPink
  | SigmacordColorDarkVividPink
  | SigmacordColorGold
  | SigmacordColorDarkGold
  | SigmacordColorOrange
  | SigmacordColorDarkOrange
  | SigmacordColorRed
  | SigmacordColorDarkRed
  | SigmacordColorGray
  | SigmacordColorDarkGray
  | SigmacordColorDarkerGray
  | SigmacordColorLightGray
  | SigmacordColorNavy
  | SigmacordColorDarkNavy
  | SigmacordColorYellow
  | SigmacordColorSigmacordWhite
  | SigmacordColorSigmacordBlurple
  | SigmacordColorSigmacordGrayple
  | SigmacordColorSigmacordDarkButNotBlack
  | SigmacordColorSigmacordNotQuiteBlack
  | SigmacordColorSigmacordGreen
  | SigmacordColorSigmacordYellow
  | SigmacordColorSigmacordFuschia
  | SigmacordColorSigmacordRed
  | SigmacordColorSigmacordBlack
  deriving (Show, Read, Eq, Ord, Data)



hexToRGB :: String -> Maybe (Integer, Integer, Integer)
hexToRGB hex = do
  let h = map toLower hex
  r <- take2 h >>= toDec
  g <- drop2 h >>= take2 >>= toDec
  b <- drop2 h >>= drop2 >>= toDec
  return (r, g, b)
  where
    take2 (a:b:_) = Just [a, b]
    take2 _ = Nothing
    drop2 (_ : _ : as) = Just as
    drop2 _ = Nothing
    toDec :: String -> Maybe Integer
    toDec [s, u] = do
      a <- charToDec s
      b <- charToDec u
      return $ a * 16 + b
    toDec _ = Nothing
    charToDec :: Char -> Maybe Integer
    charToDec 'a' = Just 10
    charToDec 'b' = Just 11
    charToDec 'c' = Just 12
    charToDec 'd' = Just 13
    charToDec 'e' = Just 14
    charToDec 'f' = Just 15
    charToDec c = readMaybe [c]



hexToSigmacordColor :: String -> SigmacordColor
hexToSigmacordColor hex =
  let (r, g, b) = fromMaybe (0, 0, 0) $ hexToRGB hex
   in SigmacordColorRGB r g b


colorToInternal :: SigmacordColor -> Integer

colorToInternal (SigmacordColorRGB r g b) = (r * 256 + g) * 256 + b
colorToInternal SigmacordColorDefault = 0
colorToInternal SigmacordColorAqua = 1752220
colorToInternal SigmacordColorDarkAqua = 1146986
colorToInternal SigmacordColorGreen = 3066993
colorToInternal SigmacordColorDarkGreen = 2067276
colorToInternal SigmacordColorBlue = 3447003
colorToInternal SigmacordColorDarkBlue = 2123412
colorToInternal SigmacordColorPurple = 10181046
colorToInternal SigmacordColorDarkPurple = 7419530
colorToInternal SigmacordColorLuminousVividPink = 15277667
colorToInternal SigmacordColorDarkVividPink = 11342935
colorToInternal SigmacordColorGold = 15844367
colorToInternal SigmacordColorDarkGold = 12745742
colorToInternal SigmacordColorOrange = 15105570
colorToInternal SigmacordColorDarkOrange = 11027200
colorToInternal SigmacordColorRed = 15158332
colorToInternal SigmacordColorDarkRed = 10038562
colorToInternal SigmacordColorGray = 9807270
colorToInternal SigmacordColorDarkGray = 9936031
colorToInternal SigmacordColorDarkerGray = 8359053
colorToInternal SigmacordColorLightGray = 12370112
colorToInternal SigmacordColorNavy = 3426654
colorToInternal SigmacordColorDarkNavy = 2899536
colorToInternal SigmacordColorYellow = 16776960
colorToInternal SigmacordColorSigmacordWhite = 16777215
colorToInternal SigmacordColorSigmacordBlurple = 5793266
colorToInternal SigmacordColorSigmacordGrayple = 10070709
colorToInternal SigmacordColorSigmacordDarkButNotBlack = 2895667
colorToInternal SigmacordColorSigmacordNotQuiteBlack = 2303786
colorToInternal SigmacordColorSigmacordGreen = 5763719
colorToInternal SigmacordColorSigmacordYellow = 16705372
colorToInternal SigmacordColorSigmacordFuschia = 15418782
colorToInternal SigmacordColorSigmacordRed = 15548997
colorToInternal SigmacordColorSigmacordBlack = 16777215


convertToRGB :: Integer -> SigmacordColor
convertToRGB i = SigmacordColorRGB (div i (256 * 256) .&. 255) (div i 256 .&. 255) (i .&. 255)

instance InternalSigmacordEnum SigmacordColor where
  SigmacordTypeStartValue = SigmacordColorDefault
  fromSigmacordType = fromIntegral . colorToInternal
  SigmacordTypeTable = map (\d -> (fromSigmacordType d, d)) (makeTable SigmacordTypeStartValue)
    where
      makeTable :: Data b => b -> [b]
      makeTable t = map (fromConstrB (fromConstr (toConstr (0 :: Int)))) (dataTypeConstrs $ dataTypeOf t)

instance ToJSON SigmacordColor where
  toJSON = toJSON . fromSigmacordType

instance FromJSON SigmacordColor where
  parseJSON =
    withScientific
      "SigmacordColor"
      ( \v ->
          SigmacordTypeParseJSON "SigmacordColor" (Number v)
            <|> ( case maybeInt v >>= Just . convertToRGB of
                    Nothing -> fail $ "could not parse Sigmacord color: " ++ show v
                    Just d -> return d
                )
      )
    where
      maybeInt i
        | fromIntegral (round i) == i = Just $ round i
        | otherwise = Nothing
