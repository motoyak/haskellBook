{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module UnitMagnitude where

data Bytes =
    B
  | KB
  | MB
  | GB
  | TB

instance (b ~ Double) => Num (Bytes -> b) where
  fromInteger i B =
    fromInteger i
  fromInteger i KB =
    fromInteger $ i * 1024
  fromInteger i MB =
    fromInteger $ i * 1024 * 1024
  fromInteger i GB =
    fromInteger $ i * 1024 * 1024 * 1024
  fromInteger i TB =
    fromInteger $ i * 1024 * 1024 * 1024 * 1024


instance (b ~ Double) => Fractional (Bytes -> b) where
  fromRational r B =
    fromRational r
  fromRational r KB =
    fromRational $ r * 1024
  fromRational r MB =
    fromRational $ r * 1024 * 1024
    
data BytesI =
    Bi
  | KBi
  | MBi
  | GBi
  | TBi

instance (b ~ Integer) => Num (BytesI -> b) where
  fromInteger i Bi =
    i
  fromInteger i KBi =
    i * 1024
  fromInteger i MBi =
    i * 1024 * 1024
  fromInteger i GBi =
    i * 1024 * 1024
  fromInteger i TBi =
    i * 1024 * 1024