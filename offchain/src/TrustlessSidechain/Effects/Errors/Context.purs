module TrustlessSidechain.Effects.Errors.Context where

import Contract.Prelude ((<>))
import Data.Maybe (Maybe(Just, Nothing))

data ErrorContext = ErrorContext ErrorContextType String
data ErrorContextType = Wallet | Transaction | Log

ppErrorContext ∷ Maybe ErrorContext → String
ppErrorContext ctx =
  case ctx of
    Just (ErrorContext c msg) →
      ( case c of
          Wallet → "A wallet error occurred: "
          Transaction → "A transaction error occurred: "
          Log → "A log error occurred: "
      ) <> "in the context of " <> msg
    Nothing → "An error occurred when running CTL base monad: "
