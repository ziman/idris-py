module IRTS.Erasure (erase) where

import IRTS.Defunctionalise
import Idris.Core.TT (Name(..))

erase :: [(Name, DDecl)] -> [(Name, DDecl)]
erase decls = decls
