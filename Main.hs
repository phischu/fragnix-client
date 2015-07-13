module Main where

import Reflex
import Reflex.Dom
import Data.Map as Map
import Safe (readMay)
import Control.Applicative ((<$>),(<*>))

main = mainWidget (el "div" (do
  nx <- numberInput
  d <- dropdown "+" (constDyn ops)
  ny <- numberInput
  text " = "
  result <- combineDyn (\x y -> (+) <$> x <*> y) nx ny
  resultString <- mapDyn show result
  dynText resultString))

ops = Map.fromList [("+", "+"), ("-", "-")]

numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
numberInput = do
  n <- input' "number" "0" never (constDyn Map.empty)
  mapDyn readMay (_textInput_value n)




