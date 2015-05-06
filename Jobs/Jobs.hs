
module Jobs where

type Owner = String
type Station = String
type Ware = String

data Contract =
    CourierContract
      { cc_issuer :: Owner
      , cc_duration :: Int
      , cc_paymentType :: PaymentType
      }
  | KillerContract
      { kc_issuer :: Owner
      , kc_target :: Owner
      }
  deriving ()

data PaymentType =
  PT_Fixed Int
