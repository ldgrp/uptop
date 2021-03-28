module Up.Model.Token where

data Token = Token String
  deriving (Eq)

tokenToString :: Token -> String
tokenToString (Token s) = "Bearer " <> s
