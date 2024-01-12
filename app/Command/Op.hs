module Command.Op where

data OperationCodes = NoOp
                    | CancelOp String

instance Show OperationCodes where
    show NoOp = "No operation done."
    show (CancelOp e) = "Cancelled operation due to: " ++ e ++ "."