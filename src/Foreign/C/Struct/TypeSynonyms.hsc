module Foreign.C.Struct.TypeSynonyms where

import Foreign.Ptr

type PtrVoid = Ptr ()
type PtrFloat = Ptr #{type float}

type ListFloat = [#{type float}]
