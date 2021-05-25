# c-struct

foo.h

```
#ifndef _FOO_H
#define _FOO_H

typedef struct { int x; int y; } Foo;

#endif
```

foo.c

```
#include <stdlib.h>
#include <stdio.h>
#include "foo.h"

Foo *
foo_copy(Foo *src)
{
	Foo *p = malloc(sizeof(Foo));
	p -> x = src -> x;
	p -> y = src -> y;
	return p;
}

void
foo_free(Foo *p)
{
	free(p);
}

void
foo_print(Foo *f)
{
	printf("Foo: x = %d, y = %d\n", f -> x, f -> y);
}

void
foo_scale(Foo *f, int s)
{
	f -> x = f -> x * s;
	f -> y = f -> y * s;
}
```

Foo.hsc

```
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foo where

import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Foreign.C.Types (CInt(..))
import Foreign.C.Struct (struct)

#include "foo.h"

struct "Foo" #{size Foo}
	[	("x", ''CInt, [| #{peek Foo, x} |], [| #{poke Foo, x} |]),
		("y", ''CInt, [| #{peek Foo, y} |], [| #{poke Foo, y} |]) ]
	[''Show, ''Read, ''Eq, ''Ord, ''Bounded]

fooPrint :: Foo -> IO ()
fooPrint (Foo_ f) = withForeignPtr f c_foo_print

foreign import ccall "foo_print" c_foo_print :: Ptr Foo -> IO ()
```
