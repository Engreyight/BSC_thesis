{-# LANGUAGE OverloadedStrings #-}

module Builder where

import Data.ByteString.Builder

infixr 6 <+>
(<+>) :: Builder -> Builder -> Builder
a <+> b = a <> " " <> b

scoreboardOperation :: Builder -> (Builder, Builder) -> (Builder, Builder) -> Builder
scoreboardOperation op (p1, s1) (p2, s2) = "scoreboard players operation" <+> p1 <+> s1 <+> op <+> p2 <+> s2 <> "\n"

scoreboardSet :: Int -> Builder
scoreboardSet val = "scoreboard players set registers imm" <+> intDec val <> "\n"