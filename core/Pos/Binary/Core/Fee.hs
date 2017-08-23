
-- | Binary instances for transaction fee data.

module Pos.Binary.Core.Fee () where

import           Universum

import           Data.Fixed       (Nano)

import           Pos.Binary.Class (Bi (..), decode, decodeCborDataItem,
                                   decodeCborDataItemTag, encode, encodeCborDataItem,
                                   encodeListLen, enforceSize, serialize')
import           Pos.Core.Fee     (Coeff (..), TxFeePolicy (..), TxSizeLinear (..))

instance Bi Coeff where
  encode (Coeff n) = encode n
  decode = Coeff <$> decode @Nano

instance Bi TxSizeLinear where
  encode (TxSizeLinear a b) = encodeListLen 2 <> encode a <> encode b
  decode = do
    enforceSize "TxSizeLinear" 2
    !a <- decode @Coeff
    !b <- decode @Coeff
    return $ TxSizeLinear a b

instance Bi TxFeePolicy where
  encode policy = case policy of
    TxFeePolicyTxSizeLinear txSizeLinear -> encodeListLen 2 <> encode (0 :: Word8)
                                                            <> encodeCborDataItem (serialize' txSizeLinear)
    TxFeePolicyUnknown word8 bs          -> encodeListLen 2 <> encode word8
                                                            <> encodeCborDataItem bs
  decode = do
    enforceSize "TxFeePolicy" 2
    tag <- decode @Word8
    case tag of
      0 -> TxFeePolicyTxSizeLinear <$> decodeCborDataItem
      _ -> TxFeePolicyUnknown tag  <$> (decodeCborDataItemTag *> decode)
