{-# LANGUAGE OverloadedStrings #-}

module Builder where

import Data.ByteString.Builder

infixr 6 <+>
(<+>) :: Builder -> Builder -> Builder
a <+> b = a <> " " <> b

scoreboardOperation :: (Builder, Builder) -> Builder -> (Builder, Builder) -> Builder
scoreboardOperation (p1, s1) op (p2, s2) = "scoreboard players operation" <+> p1 <+> s1 <+> op <+> p2 <+> s2 <> "\n"

scoreboardSet :: (Builder, Builder) -> Int -> Builder
scoreboardSet (p, s) val = "scoreboard players set" <+> p <+> s <+> intDec val <> "\n"