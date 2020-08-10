module Up.Model.Token where

data Token = Token String 
    deriving (Eq)

instance Show Token where
    show (Token tok) = "Bearer " <> tok

