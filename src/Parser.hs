module Parser where

import ReadP
import Data.Char(isLetter, isNumber)

var :: ReadP String
var = many alphaNum
  alphaNum = satisfy isLetter +++ satisfy isNumber
