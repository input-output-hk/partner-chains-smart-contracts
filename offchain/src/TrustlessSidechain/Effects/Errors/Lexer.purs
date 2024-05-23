module TrustlessSidechain.Effects.Errors.Lexer where

import Prelude

import Data.List (List, many)
import Parsing (Parser)
import Parsing.String.Basic (oneOf)

spaces ∷ Parser String (List Char)
spaces = many $ oneOf [ ' ', '\n', '\r' ]

token ∷ ∀ p. Parser String p → Parser String p
token p = spaces *> p
