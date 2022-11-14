-- | Foo interface implementation
module App.DI.Foo
  ( initFoo
  ) where

import Server.ListTag (Foo(..))

initFoo :: Foo
initFoo = Foo
  { validTag = const $ pure True
  }
