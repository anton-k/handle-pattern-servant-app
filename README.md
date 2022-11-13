# Haskell Handler pattern and servant example app

Show case for a small application written with servant and [Reader-pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/).
It is an example on how to make Dependency Injection in Haskell with Reader-pattern.

Application allows to save messages with tags. 
User can create a new message and then user can query it by id
or by tag.

API methods:

```
POST: api/v1/save:
   input: JSON text of the message and list of tags
   output: id of the message 

GET: api/v1/get/message/{message-id}
  input: id of the message
  output: message or error  

GET: api/v1/list/tag/{tag} 
  input: tag 
  output: list of messages that belong to the tag

POST: api/v1/toggle-logs
  toggles the logs (active or silent)
```

Applications shows how to  create interfaces for mocks and real instances.
Also we show how to use interfaces that depend on run-time data 
and how we can split the top-level interface to smaller ones dedicated to concrete methods.
On save message is augmented with current time stamp which is queried over external service.
We use `getCurrentTime` for mock but it serves as an example of external dependency.

### Application user guide

See makefile for available actions for installation and testing the service.
The app can be biuld with stack. The GHC extension list is kept lightweight
but we rely on modern compiler GHC 9.2 for nice record-dot syntax.

We can 

* build the app:

  ```
  make build
  ```

* run the executable:

  ```
  make run
  ```

  It will start the server on default port.

* trigger API-routes over `curl`:

  ```
  make message='{"message": "waiting for the summer", "tags": ["random"] }' post-save
  make id=0 get-id
  make tag=random list-tag
  make toggle-logs
  ```

The rest of the article is tutorial explaining the application of Reader pattern
in Haskell to build web-apps.

### The application structure

The library `src/` defines types, interfaces, server and handlers

 * `Types` - types of the domain
 * `App` - reader pattern monad
 * `Api` - API for the app
 * `Server` - servant server and main envirnment (state) of the service
 * `DI.[Log | Time | Setup]` - interfaces for the app and common functions
 * `Server.[Save | GetMessage | ListTag | ToggleLog]` - handlers of the API-routes

Executable `app/` implements interfaces initialises service state and launchaes the app.

 * `Main` - init and launch server
 * `Config` - read server configs from command line arguments
 * `App.DI.[DB | Log | Time | Setup]` - implement interfaces
 * `App.State` - mutable state of the app
 * `App.DI.Db.MockDb` - mock db, should be in separate package but kept here for simplicity

## Introduction

In this example and tutorial we will learn how to build flexible
web-apps with the help of reader pattern. We will mention some key-factors
of the web-development domain, discuss the problems and solutions
and look at how to combine Readr pattern with servant.
While implementing a small app.

What we will learn:

* How to organise application with ReaderT, keeping our mtl-stack shallow and simple.

* with Reader-approach we can decopule implementation from the servant-server details

* we can keep interface for external services separate to mock them for testing
  or swap implementations

* to be flexible we propose API-route or user action first design for interfaces.
   It's better to build interfaces not from the point of view of the actual
   external service but from the user perspective of the app. From what our
   app wants form that external service in the given API-route.

* How to we  can scrap the reader pattern boilerplate with new GHC's features

* the web-app domain is ocean wave not a solid gorund to build castles.
   so we need to build with presence of uncertainty and unexpected changes in mind.
   Which coresponds badly with mathematical thinking. 
   We need to use more flexible solutions.

* How to hide mutable state with interfaces

In this app we use the combo of Reader pattern with Handler pattern for DI's
But our version of Handler pattern is reversed in terms of where interfaces originate.
In original Handler pattern we wrap external services with concrete interfaces.
So interface is driven by external dependency. But I'd like to stress the point
of user or app driven interfaces. We build small interfaces that are dedicated
to concrete part of the app and use it localy. And on level of the executable 
we use concrete implemntation. 

This approach is inspired by the book [Domain Modeling Made Functional](https://pragprog.com/titles/swdddf/domain-modeling-made-functional/) by
Scott Walschin (it uses F#). In this book it's well described how to build small 
and focused interfaces. I'd like to thank the [Scott Walschin](https://fsharpforfunandprofit.com/) 
for providing this simple yet powerful technique to mitigate complexity. 
In this tutorial I'd like to adapt it to Haskell and building web apps with Reader pattern.

So let's dive in.

## Reader patterm

The [Reader pattern](https://github.com/anton-k/reader-pattern-servant-app) is one
of the standard ways to design web-applications in Haskell. 
The main idea is that we use a Reader monad to pass environment to 
different parts of the app. Here is the main type of the application (see `App` module):

```haskell
newtype App env a = App (ReaderT env IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader env
    , MonadIO
    , MonadThrow
    , MonadCatch
    )

runApp :: App env a -> env -> IO (Either ApiError a)
runApp (App a) env = runExceptT $ runReaderT a env
```

Our main monad `App` is `ReaderT` over plain `IO`.
We keep our mutable state in `TVar` inside the `env`. In different parts of the app
we can read the `env` parts with reade'sr function `asks` and here is the main idea of it.

The pros of Reader-pattern:

* easy to implement and reason about
* very fast and efficient
* light-weight on dependencies and extensions
* offers uniform way to design apps and track mutable state

### Depndency injection for Haskell

With reader pattern we can make lightweight dependency injection by putting
interfaces into environment:

```haskell
data Env = Env
  { db  :: Db
  , log :: Log
  }
```

where `Db` and `Log` are interfaces:

```haskell
data Log = Log
  { logInfo  :: Text -> IO ()
  , logError :: Text -> IO ()
  , logDebug :: Text -> IO ()
  }

data Db = Db 
  { getMessage  :: MessageId -> IO (Maybe Message)
  , saveMessage :: Message -> IO MessageId
  }
```

This way we keep concrete implementation decoupled from the application
and we can create mocks for testing or play with different implementation.

Notice that `DB`-interface is stated in terms of the domain of the task. 
We would like to avoid `DB`-driver details as much as possible. 
Everyting that is needed for interface realisation we hide into closures.
For example for Postgress we need connection. We can initialise interface
and hide the connection info into closure:

```haskell
initDb :: Url -> IO Db
initDb url = do
  conn <- Postgress.getConnection url
  pure $ Db 
    { getMessage  = Postgress.getMessage conn
    , saveMessage = Postgress.saveMessage conn
    }
```

where `Postgress.getMessage` is concrete implementation with 
convertion to DB-types and SQL queries.

With this trick we can hide many things. For example we can call external 
service as a http-client and hide all connection info under simple
interface with concrete types and `IO` and `Either` for error messages.

### Mutable internal state

In Reader pattern we keep internal mutable state of the server inside `TVar`s
in the environment of the reader. And update it with usual STM routines.
We need `TVar` or similiar for concurrent access.

For example if our service has configs that can be updated by service admins
over methods without restarting of the server:

```haskell
data Env = Env 
  { config :: TVar Config   -- ^ mutable configs
  , ...
  }
```

And inside the method we can use `asks`  and `STM`-functions to modify it:

```haskell
handleSetConfig :: Config -> App Env ()
handleSetConfig cfg = do
  configVar <- asks (.config)  
  liftIO $ updateConfig configVar cfg

updateConfig :: TVar Config -> Config -> IO ()
updateConfig var newConfig = 
  atomically $ writeTVar' var (\env -> { config = newConfig })
```

also it's good idea to make newtype wrappers for those parts of the state
and isolate it's logic:

```haskell
module Server.Env.Config 
  ( Config
  , ConfigVar

  , newConfigVar
  , updateConfig
  , readConfig
  ) where

import ...

data Config = Config { ... }

newtype ConfigVar = ConfigVar (TVar Config)

updateConfig :: ConfigVar -> Config -> IO ()
readConfg :: ConfigVar -> IO Config
```

### How to make reusable functions

There is a trick to make reusable functions that work across readers
with different environments. In the 
[original article](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/) 
it looks like this:

```haskell
class HasLog env where
  getLog :: env -> Log

class HasDb env where
  getDb :: env -> Db
```

And we can write reusable code with it:

```haskell
info :: HasLog env => Text -> App env ()
info message = do
  Log{..} <- asks getLog
  liftIO $ logInfo message
```

With it we can encode our dependencies in contexts and make cool reusable functions
that work on subsets of our interfaces:

```haskell
setWithLog :: (ToJSON a, HasLog env, HasDb a) => req -> App env ()
setWithLog req = do
  Log{..} <- asks getLog
  Db{..}  <- asks getDb

  liftIO $ do
    logInfo (mappend "Request: " $ jsonToText $ toJSON req)
    saveFoo (toFoo req)
```

where `saveFoo` is some `DB`-function and `toFoo`, `jsonToText` are proper convertors.
So we have nice `setWithLog` function that we can use across many 
readers for which we can get logging and dDB ibterface form the environment.

### Scrap your Reader boiler plate

Declaring classes and instances is sort of inevitable boiler-plate 
of the reader pattern. Routine goes like this:

Define an interface and `Has`-class for it:

```haskell
data Log = Log { ... }

class HasLog env where
  getLog :: env -> Log
```

Use it to define generic wrappers for the functions,
also we often use lifting with `MonadIO` to make our interface
methods more composable and easy to use in the setting of the Reader-based functions:

```haskell
info :: haslog env => text -> App env ()
info = ... -- see example above

debug :: haslog env => text -> App env ()
debug = ... -- see example above
```

And for eny reader that uses this inerface we define an instance:

```haskell
data SaveEnv = SaveEnv 
  { log :: Log
  , ... other stuff ...
  }

instance HasLog SaveEnv where
  getLog env = env.log
```

But it turns out that we can make GHC to derive it all for us with record class `HasField`:

```haskell
import GHC.Records (HasField(..))

type HasLog  env = HasField "log" env Log
```

and all environments that keep the `Log`-interface in the field `log` 
are automatically can be used. We redefine our functions with this approach:

```haskell
type HasLog  env = HasField "log" env Log

info :: HasLog env => Text -> App env ()
info message = do
  Log{..} <- asks (.log)
  liftIO $ logInfo message
```

It relies on GHC 9.2 extensions:

* `OverloadedRecordDot` to derive `getField` for us 
* `ConstraintKinds` to declare class constraint synonyms

So with this we don't need to write any boiler-plate instances
that are inherent to Reader-pattern. 
We can only define nice class constraint synonyms and
use proper naming on using interfaces and we are done.

### Using it with Servant

In the servant we need to wrap resonce to Servant.Handler monad.
The main idea of the approach is to use servant only on top-level Server-method
and inside handlers we should work only in terms of reader `App` monad.

We can adapt `App` monad to Servant needs with this function

```haskell
toHandler :: env -> App env resp -> Servant.Handler resp
toHandler e handler = Handler $ ExceptT $ try $ runApp handler e
```

Note that we need to use `try` to capture servant server exceptions.
And we can define converters for more arguments:

```haskell
toHandler1 :: env -> (req -> App env resp) -> req -> Servant.Handler resp
toHandler1 env handle a = toHandler env (handle a)

toHandler2 :: env -> (a -> b -> App env resp) -> a -> b -> Servant.Handler resp
toHandler2 env handle a b = toHandler env (handle a b)
```

In the servant definition we use it like this:

```haskell
server :: Env -> Server Api
server env =
       toHandler1 saveEnv Save.handle
  :<|> toHandler1 getMessageEnv GetMessage.handle
  :<|> toHandler1 listTagEnv ListTag.handle
  :<|> toHandler  toggleLogEnv ToggleLog.handle
  where
    -- local env's for methods
    saveEnv    = 
    getMessageEnv =
    listTagEnv =
    toggleLogEnv =
```

and the cool thing is that in all local handlers we don't have to think about servant
anymore. They should work in tems of our reader monad `App`.

#### Custom messages

For simplicity we use plain `Text` error messages but in real app 
we should define more fine grained type for ApiError that we can convert 
to servant errors.

Also as we work with plain `IO`-monad we need to convert our
exceptions to servant ones so that they can be handled properly.
For that we use custom `throwApi` function:

```haskell
import Control.Monad.Catch    (MonadThrow(..), MonadCatch(..))
import Data.ByteString.Lazy   qualified as BL
import Data.Text.Encoding     qualified as Text
...

throwApi :: ApiError -> App env a
throwApi = throwM . toServantError
  where
    toServantError (ApiError err) = err400 { errBody = BL.fromStrict $ Text.encodeUtf8 err }
```

### Implement interfaces outside of the library

In this example we keep all interfaces separate from the implementation.
And separation is on package level. Implementation is defined in the executable `app`
and interfaces are declared in the library `src`. 

This way we can faciliate top-down approach and work in terms of interfaces that
are yet to be implemented. So if we zoom in building of the library:

```haskell
stack build handler-proto:lib
```
We are not tied to concrete implementation and can quickly invent new interfaces
and try them out. 

### Interface that depend on mutable state

What if interface dependes on mutable state? 
In this example it's illustrated by `Log` interface.
By task definition we can call a route to toggle logs. 
With it we can switch of redundant logging or turn them on again.

In real application there might be need for hot config update without
relaunch of the app or we can depend on some other stuff that is subject to change.

We can package interface with a mutalbe state and adapt interface on the fly.
For example let's consider our case:

```haskell
data Log = Log
  { logInfo  :: Text -> IO ()
  , logDebug :: Text -> IO ()
  , logError :: Text -> IO ()
  }

data LogVar = LogVar
  { isVerboseLog :: TVar Bool
  , log          :: Log
  }
```

We have nice `Log` interface but it depends on mutable variable
that switches logging facility. We can package them together
and provide interface to read the logger:

```haskell
type HasLog  env = HasField "log" env LogVar

askLog :: HasLog env => App env Log
askLog = liftIO . readLogVar =<< asks (.log)

readLogVar :: LogVar -> IO Log
readLogVar = ... 
```

so we have `askLog` that reads the log interface. But it reads
it with a twist that if logging is set to `True` it returns the original
logger but if it's set to `False` it returns the silent logger that does nothing:

```haskell
silentLog :: Log
silentLog = Log
  { logInfo = silent
  , logDebug = silent
  , logError = silent
  }
  where
    silent = const $ pure ()
```

and here is optional read log definition:

```haskell
readLogVar :: LogVar -> IO Log
readLogVar var = do
  isVerbose <- readTVarIO var.isVerboseLog
  pure $ if isVerbose
    then var.log
    else silentLog
```

and this way we can conditionally use interface that depend on mutable state.
We package it to interface:

```haskell
type HasLog  env = HasField "log" env LogVar

-- | Read current log
askLog :: HasLog env => App env Log


-- | Toggle logs
toggleLog :: HasLog env => App env ()
```

Another option to implement it is to use verbosity var
on the stage of logger initialisation:

```haskell
data Env = Env
  { isVerbose :: TVar Bool
  , log       :: Log
  }
```

and during logger initialisation we can pass this variable:

```haskell
initLog :: TVar Bool -> IO Log
initLog isVerboseVar = ... use verbosity info in the methods
```

It hides this connection details behind interface. and leaves us with 
nice interface. But for this approach we should pass verbosity to
all logging functions.

### Taking interface approach to extreme

I've said that `Env` is for keeping interfaces and mutable state of the
service. The cool thing that we can make interfaces depend on mutable 
state and they might look like they are just pure interfaces (up to the point of `IO`-inside them).

It's interesting can we hide all mutation and use only interfaces?
Actually we can. This is an open question should we do it or not
but in this case our app looks like a composition of interfaces every one of which 
can be mocked or used with laternative implementation.

Let's turn back to our stetful logger example. With one approach we can
keep interfaces look stateless but pass mutable state on initialisation stage.
and our solution lookes like this on `Env`-level:

```haskell
data Env = Env
  { isVerbose :: TVar Bool
  , log       :: Log
  }

initLog :: TVar Bool -> IO Log
initLog =
```

The function `initLog` hides dependency on mutable interface in the logger.
and we have to face the reality of `isVerbose` mutable `TVar`-state.
But what if it was also an interface? 

It can be done this way:

```haskell
---------------------------------------
-- in the library code

data Env = Env
  { setup :: Setup
  , log   :: Log
  }

-- | Interface for tweaking configs
data Setup = Setup
  { toggleLogs  :: IO ()
  , swithcToFoo :: FooConfig -> IO () -- ^ other config tweaks of the app
  , useBar      :: BarConfig -> IO ()
  }

---------------------------------------
-- in the executable code

initEnv :: IO Env
initEnv = do
  verboseVar <- newVerboseVar
  setup <- initSetup verboseVar
  log <- initLog verboseVar
  pure $ Env setup log
```

Note that `Setup` is not a data structure it's an interface
to trigger changes in the configs of the system.
And we share the link between logger and config only inside the executable `app`.
On the level of the library they look decoupled.

This way we are not forced to chose TVar between some other method of
sharing mutable state. It's all hided from the library.
By the `Env` we only see the list of available actions 
that can be performed on the app in terms of interfaces.

### Hiding mutable variables with interfaces

For the previous example instead of passing `TVar Bool` directly
to initialization functions we can create a wrapper that hides
away internal details of that mutalbe variable. We do that in the module `app/App/State.hs`:

```haskell
-- | Application mutable state
module App.State
  ( VerboseVar
  , newVerboseVar
  , isVerbose
  , toggleVerbose
  ) where

import Control.Concurrent.STM

newtype VerboseVar = VerboseVar (TVar Bool)

newVerboseVar :: IO VerboseVar
newVerboseVar = VerboseVar <$> newTVarIO True

isVerbose :: VerboseVar -> IO Bool
isVerbose (VerboseVar var) = readTVarIO var

toggleVerbose :: VerboseVar -> IO ()
toggleVerbose (VerboseVar var) = atomically $ modifyTVar' var not
```

We define a newtype wrapper for the mutable variable that controls
verbosity of the logs and provide several functions with which we can read that variable and set it up.

And signatures for our initialization functions become more self-explanatory:

```haskell
initLog   :: VerboseVar -> IO Log
initSetup :: VerboseVar -> Setup
```

### Flexibility of record-style interfaces

I'd like to metion how easy it's to adapt our interfaces. As they are
expressed as plain functions in the records. 
Let's consider logging example. We need to define the logging context dedicated
to specific route. for example we need to prefix the logs with the name of the route.

We can adapt the whole logging interface by plugging the function:

```haskell
mapLog :: (Text -> Text) -> Log -> Log
mapLog go logger = Log
  { logInfo = logger.logInfo . go
  , logDebug = logger.logDebug . go
  , logError = logger.logError . go
  }

addLogContext :: Text -> Log -> Log
addLogContext contextMesage =
  mapLog (mappend (contextMesage <> ": "))
```

In this example we use sort of logging middleware that inserts
text-processing function prior to user call. We can transform the whole 
interface with it. And we can use it in the code by passing the logger to concrete API-route:

```haskell
    saveEnv =
      Save.Env
        { db = env.db.save
        , time = env.time
        , log = addLogContext "api.save" env.log
        }

    listTagEnv =
      ListTag.Env
        { db = env.db.listTag
        , log = addLogContext "api.list-tag" env.log
        }
```
Here we transform the common logger defined in top-level envirnment state
of the service reader and pass it to the local loggers. And all local loggers
will have this modified logging built into it.


### Keep your environments and interfaces small

I'd like to avoid having one big `Env` type that declares all possible
interfaces and mutable state of the server. Instead of this it's much better
to have local small environments and interfaces that are dedicated to concrete part
of the app. 

This example app is tiny. But for full-blown application one single Env can become
a source of compilation-time pain very quickly.
Because if every handler will depend on it every additional feature will
force **recompile everyting** scenario. And with time it would be very hard to
be able to reason about gigantic `Env`. This will lead to reduce our time to market
as app is strongly coupled on one type of `Env` and there wil be many interdependencies
and compilation time will be bad.

Instead of this I prefer to keep `Env` dedicated to methods.
Let's take a look at the interface of the `ListTag` API-method.
In the task it returns the message by `Tag`. Here is complete definition:

```haskell
-- | Get by tag handler
module Server.ListTag
  ( Env(..)
  , Db(..)
  , handle
  ) where

import DI.Log
import Types

data Env = Env
  { db  :: Db
  , log :: Log
  }

data Db = Db
  { listTag :: Tag -> IO [Message]
  }

-----------------------------------------
-- Handler

handle :: Tag -> App Env [Message]
handle tag = do
  Db{..}  <- asks (.db)
  Log{..} <- askLog

  liftIO $ do
    logInfo $ "get by tag call: " <> display tag
    listTag tag
```

Let's take it apart. It has it's own `Env`:

```haskell
data Env = Env
  { db  :: Db
  , log :: Log
  }
```

and we can see that `DB`-interface is also local to the method:

```haskell
data Db = Db
  { listTag :: Tag -> IO [Message]
  }
```

And the handler is defined in terms of local reader:

```haskell
handle :: Tag -> App Env [Message]
```

By imports we can see that it depends on common `Types` and common logger interface:

```haskell
import DI.Log
import Types
```

so with this approach we don't rely on Servant or on big one-for-all `Env`.
we keep it small, simple and local to the method. 
It would be painless to make a microservice out of it.

But how we use it in the service? In service we have that big `Env`.
It contains all enironments for the methods:

```haskell
-- | Service environment by methods
data Env = Env
  { save        :: Save.Env
  , getMessage  :: GetMessage.Env
  , listTag     :: ListTag.Env
  , toggleLogs  :: ToggleLog.Env
  }

server :: Env -> Server Api
server env =
       onRequest1 env.save Save.handle
  :<|> onRequest1 env.listTag GetMessage.handle
```

Here we instantiate concrete environment for the API-method.
This way by local environment definition we can see which dependencies are used.
For example the `ToggleLog` method uses only `Log`-service 
and modification of `DB` or `Time` interface will not affect it:

```haskell
module Server.ToggleLog where

data Env = Env
  { log   :: Log
  , setup :: Setup
  }

handle :: App Env ()
```

Let's consider some benefits of this approach.

#### Interfaces are driven by the API-methods

With this approach we define interfaces in terms of the method domain and
we use only that much from the external dependency as we need to implement
by the user action. 

This can save us alot of trouble by trying to define beatiful and shiny
DB-interface that will fit every needs. With single DB-interface to rule them all
it can lead to disaster of bloated interfaces
that are hard to modify and reason about. And they usually trigger recompilation of
the whole project.

#### The price of change

I argue that writing methods in this style we can keep our
changes local.

Let's consider two types of changes:

* adding new method to existing interface
* adding new type of external depndency

##### Add method to existing interface

For example if we want to add `validTag` to our DB-interface
for the API-route `ListTag`. We can add it to the local DB-interface:

```haskell
data Db = Db
  { listTag  :: Tag -> IO [Message]
  , validTag :: Tag -> IO Bool
  }
```

And with this approach we re-compile only two moduels (this one and server that puts it all together)
and we have no errors on the level of the library. But we will have missing field in the DI-implementation.
Which is easy to define with mock or we can compile on the library level with 

```haskell
stack build handler-proto:lib
```

for a while and keep implementing our feature in terms of interfaces.

##### Add new external depndency

Ok let's imagine that `validTag` is provided not by `DB` but by some http-client `Foo`.
We have two options here to consider:

* is it well defined and settled interface like `Log`?
* is it hard to define interface with many features like `DB`?

If it's well defined then we can declare it under `DI`-umnrella and just import
it to the handler:

```haskell
module Server.ListTag where

import DI.Foo

data Env = Env
  { db  :: Db
  , log :: Log
  , foo :: Foo    -- ^ new interface here
  }
```

and in handler we can use it in the same way:

```haskell
handle :: Tag -> App Env [Message]
handle tag = do
  Db{..}  <- asks (.db)
  Log{..} <- askLog

  Foo{..} <- askFoo  
  -- use validTag from Foo 
```

Note that main service `Env` does not change at all with this change.
It only changes if we add a new API-route.

Also we pass it to the local env for `ListTag` to make it compile.
Again we get no errors on library level and we recompile only two modules if 
`Foo` is already defined in `DI`.

In the second option if we decide that this is hard to settle down and vague interface
like `Db` one. We create local version of the `Foo` and keep it inside 
the handler module:

```haskell
module Server.ListTag where

data Env = Env
  { db  :: Db
  , log :: Log
  , foo :: Foo -- ^ new interface here
  }

data Foo = Foo
  { validTag  :: Tag -> IO Bool 
  }
```

And that's it. We also recompile only two modules and get no errors on the library level.

##### Coding through the uncertainty

In web-applications domains are very flexible and features are incoherent at best
and come to life and death as fast as the market wants them.
And nothing can be done about that. Our domain is ocean wave and it's hard to build
castles on top of it. 

But we are Haskellers. We are mathy people. We like beatiful solid Math interfaces.

Forget it. This approach can lead to disaster in the web-application domain.
The interfaces starting solid and cool quickly become incoherent and bloated.

So instead of building rock solid, beautiful interfaces I propose to build local
small interfaces that are easy to introduce and throw away if they are not needed.
It makes us more flexible and easy to adapt to the changes.

#### Downsides of the approach

Let's consider some downsides.

##### Interface duplication

One downside that comes to mind is code duplication. Say we work as a team on 
new features. And our project uses small interfaces as this post advertised
and we can build stuff in isolation and we are happy with tht.

But say what if sub-team A  working on route A 
wants some interfaces that are local to the route B that team B works on.
What should we do?

Should we introduce cross dependencies or try to isolate or regroup interfaces in the `DI`?
This is an open question. If we take this approach to extereme we should allow the code to
be duplicated. So the same local DB-method used in both cases should stay inside local version.

and in the `DI` implementation stage we will just use the same low-level function 
to instantiate it. 

This code duplication I think is a price that worth it. as we still don't get artificial coupling.
Because as I stated our domain is lalways in flux. as it evolbves what looked the
same on Monday might become not so the same on Friday next month. 
and with this coupling introudced we will bring the unwonted change to the interface that
does not really need it.  But as coupling was codified we will forget that we need to 
keep them separate and we will bring stronger bound that will prevent changes from
being local and flexible.

I think that this is where software engeneering stops to look like a Science and
starts to look like like an Art. There is no right answer to this.
We should balance it as we grow and our app grows. 
Some interfaces can be reused and we might want to solidify them to not to copy over and over.
But some are real deamons of change and it would be hard to keep them at bay.
And we should use them localised per method.

So it becomes the matter of taste and intuition. But starting small with local
ones I think it pays off and great decision for web-application building.
As it's much more flexible approach.

### Service configs

Recommended way to organise service settings is with config file
that is easy to read for humans (for example YAML or TOML formats).
We can parse the YAML with `yaml` library and parse CLI-arguments with
`optparse-applicative` library. The code example is in the `app/Config.hs`.
For our app we can see the available options with:

```
stack run -- --help
```

### Scaling up

So we have defined our small app. But story does not end there.
We have to implement new and new API-routes and features and app becomes not so small.
How to keep it small nonetheless?

I think there is no answer to this. We have to balance on the waves.
But in this section I'd like to mention osme further steps.

For simplicity I kept all API definition in the single module `Api`.
In real case we can split it also to modules as we did it with handlers.
This is a proper place not only for servant API definition but also for all 
transport types that are used for response and requests. We should keep it 
separate from domain types.

Also we can go down to microservice route and split the app 
by groups of logically related methods to separate services.
On this stage our method with local interfaces can pay off well.
As we already have separated environments we can define separate 
apps with local env's becoming top-level ones.

But keep in mind the hidden dependencies of mutable state on the app init level.
It can also become tangled. I advise instead of direct usage of TVar's to wrap
them to newtypes and create the modules that provide meaningful interface
for them so that `TVar` details are hidden. This way it's easier to decouple things
or see which one depends on which.

### Conclusion

We have discused a Reader pattern and how to use it to build flexible
web-apps with servant that are easy to change and keep development with the wave.

Let's mention the points:

* we can organise application with ReaderT, keeping our mtl-stack shallow and simple.

* with Reader-approach we can decopule implementation from the servant details

* we can use local environments for API-routes and assemble them in the last stage on level
   of the server definition or we can swap it to microservice design.

* local interfaces that are driven by the API-routes give more flexible solutions
   that are local to the compiling routine and more easy to think about at price of possible
  duplication.

* we can scrap the reader pattern boilerplate with GHC's `HasFields` instances
   that are autogenerated for us.

* the web-app domain is ocean wave not a solid gorund to build castles.
   so we need to build with presence of uncertainty and unexpected changes in mind.
   Which coresponds badly with mathematical thinking. 
   We need to use more flexible solutions.

* Separation of interface definition and implementation on package level.
  Use executable (or separate package) for implementation and inside the
  library think in terms of open interfaces that are yet to be dfined.

* Mutable state can be hided behind interfaces completely. 
  We can link  internal depndencies in the initialisation step
  if interfaces want to communicate with each other.

Happy web-apps building with Haskell!
