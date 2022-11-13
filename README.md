# Haskell Reversed Handler pattern and servant example app

Show case for a small application written with servant and Reversed Handler pattern.
It is an example on how to make Dependency Injection in Haskell with Handler-pattern.

The main idea of reversed handler pattern is that we build interfaces for external services
not driven by services themselves but by the methods we use in concrete API-routes.
In the original article on [Handler pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html)
we describe interfaces to DB or Logging based on the natural API of the library.
But I argue that it's much more beneficial to create small interfaces dedicated to concrete
task of the API-route. This way our interfaces ar more flexible and local regarding to change
of the code. Also it might be interesting to check the same project implemented in [Reader pattern](https://github.com/anton-k/reader-pattern-servant-app).


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

The rest of the article is tutorial explaining the application of Handler pattern
in Haskell to build web-apps.

### The application structure

The library `src/` defines types, interfaces, server and handlers

 * `Types` - types of the domain
 * `Error` - custom server errors
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
web-apps with the help of handler pattern. We will mention some key-factors
of the web-development domain, discuss the problems and solutions
and look at how to combine Handler pattern with servant.
While implementing a small app.

What we will learn:

* How to organise application with collection of interfaces 

* we can keep interface for external services separate to mock them for testing
  or swap implementations

* to be flexible we propose API-route or user action first design for interfaces.
   It's better to build interfaces not from the point of view of the actual
   external service but from the user perspective of the app. From what our
   app wants form that external service in the given API-route.

* the web-app domain is ocean wave not a solid gorund to build castles.
   so we need to build with presence of uncertainty and unexpected changes in mind.
   Which coresponds badly with mathematical thinking. 
   We need to use more flexible solutions.

* How to hide mutable state with interfaces

In this app we use the Handler pattern for DI's
But our version of Handler pattern is reversed in terms of where interfaces originate.
In original Handler pattern we wrap external services with concrete interfaces.
So interface is driven by external dependency. But I'd like to stress the point
of user or app driven interfaces. We build small interfaces that are dedicated
to concrete part of the app and use it localy. And on level of the executable 
we use concrete implemntation. 

### Links

This approach is inspired by the book [Domain Modeling Made Functional](https://pragprog.com/titles/swdddf/domain-modeling-made-functional/) by
Scott Walschin (it uses F#). In this book it's well described how to build small 
and focused interfaces. I'd like to thank the [Scott Walschin](https://fsharpforfunandprofit.com/) 
for providing this simple yet powerful technique to mitigate complexity. 
In this tutorial I'd like to adapt it to Haskell and building web apps with Reader pattern.

### History of project

The history of this project is fun road of simplifiaction of the Reader pattern to the bare essentials
which evetually lead me to the question: Do we really need Reader in the first place?

So here is my progress:

* `type App rnv a = ReaderT env (ExceptT IO) a`
* `newtype App rnv a = App (ReaderT env (ExceptT IO) a) deriving newtype (...)`
* `newtype App rnv a = App (ReaderT env IO a)`
* split env to local per API-route interfaces
* find out that mutable state can be hidden with interfaces also
* but if everything is described in interfaces, do we really need ReaderT at all?
* handler pattern: `interfaces -> request -> IO response`

During rewrite of the library code the library code size shrinked from 266 to 210 LOC.
I've remove all deps on mtl, exceptions, liftIO and etc. from the code. 

This is an open question to me. But I'm inclined to think that no, we don't need the reader 
for our next web-application.

So let's dive in.

## Handler patterm

The Handler pattern is a very simple way to make dependency injection (DI) in Haskell.
We express interfaces as plain records of functions and pass them around as arguments.
So we notice that DI in Haskell is just a currying. 

Why do we go with records and not with type classes? Because they are more flexible.
we can store them as a value, pass to the function, transform with the function, keep in collection
and even in mutable refs and they are not tied to particlular type that is instance of the type class.

Using records can be somewhat cumbersome with polymorphic funcitions as generic parameters
should become parameters of the intterface. But it turns out that this generic behavior is
rarely needed in web-apps so we can stick with plain `IO` and handling errors with `Either` or `Maybe`.

There is alternative way to pass interfaces with Reader-pattern. 
Reader pattern incapsulates the collection of interfaces into environment.
But this approach can lead to frustration that we need to have uniform collection
of interfaces for all functions. and also we have some tiny performance  overhead.

In contrast with currying we have no performance penalty and we can pass different 
flavours interfaces on the spot. This comes at the price of being self-repetetive
as we don't use direct calls to external dependencies but call them over interface.
This is why we have DI-in the first place. 

Pros of the Handler-pattern 

* easy to implement and reason about (as simple as applying argument to the function)
* very fast and efficient
* light-weight on dependencies and extensions

### Depndency injection for Haskell

Let's start with the notion of interface as a record.
Let's look at the examples. Here is the logger interface:

```haskell
data Log = Log
  { logInfo  :: Text -> IO ()
  , logError :: Text -> IO ()
  , logDebug :: Text -> IO ()
  }
```

Here is an interface for the persistent storage:

```haskell
data Db = Db 
  { getMessage  :: MessageId -> IO (Maybe Message)
  , saveMessage :: Message -> IO MessageId
  }
```

and instead of directly calling those functions we pass them 
around to functions that need them:

```
doSomethingWithStorage :: Db -> Message -> IO ()
doSomethingWithStorage Db{..} msg = ... use interface ...
```

we can use `RecordWildCards` extension to bring all functions of the interface in
scope of the function. This our `import` for interfaces.

also interfaces can be organised in groups:

```haskell
data Env = Env
  { db  :: Db
  , log :: Log
  }
```

so if we want to do something with storage and log results as we go
we can use this combined interface:

```haskell
logAndStore :: Env -> a -> IO b
logAndStore (Env Db{..} Log{..}) a = ...
```

We can also have collection of interfaces. Imagine that we have a
list of competing http-services that can be identified by name and all
of them can be wrapped in the sme interface. 

We can create a map of interfaces:

```haskell
type Services = Map ServiceName ServiceInterface
```

We can do some fun with it like querying a method concurrently and returning
which ever returns first. Or iterating over all of them and returning result in the list.
This all stems from the benefit of having interfaces as plain values.

For the web application we will pass the interface record to the handler of the API-route
as first argument:

```haskell
handler :: Interface -> ApiRequest -> IO ApiResponce
```

### Mutable internal state as interface

Originaly I used extensively the Reader-pattern to keep the track of internal state in `TVars`.
But it's interesting to note that with some discipline mutable variable management can also 
be organised in interfaces.

In our app we have mutable shared state of logger verbosity we can update it by calling a API-method
`toggle-logs`. It turns out that instead of using TVar directly we can pass it 
to interface initialization functions and internaly if they rely on IO and usually they do
as we express external services with interfaces. They can read those variables
and behave according to changes.

So instead of doing this:

```haskell
data Env = Env
  { isVerbose :: TVar Bool
  , log       :: Log
  }
```

We can pass the `TVar` to the initialization of the logger:


```haskell
initLog :: TVar Bool -> IO Log
initLog =
```

The function `initLog` hides dependency on mutable interface in the logger.
and we have to face the reality of `isVerbose` mutable `TVar`-state.
But what if it was also an interface? 

We can provide an interface tweak the logger state:

```haskell
-- | Interface for tweaking configs
data Setup = Setup
  { toggleLogs  :: IO ()
  }
```

And in in the executable code we can initialise `TVar` and
pass it to interface constructors for `Log` and `Setup`


```haskell
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

Keeping mutable variables visible to the user is more flexible
approach as it allows for mutual-recursive dependencies.
But for keeping them in interfaces graph should be acyclic.
If your application does not need cyclic deps of interfaces.
And I'm sure you dont want that to happen we can turn mutables
to interfaces and this matches nicely with Handler-pattern as everything
becomes just a collection of interfaces.

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

You can look at the code how it's organaised in the implementation of `Log` and `Setup`
in the `app/App/DI` modules.

### Using it with Servant

In the servant we need to wrap responce to Servant.Handler monad.
The main idea of the approach is to use servant only on top-level Server-method
and inside handlers we should work only in terms of the `IO` and interfaces
that are passed as arguments to the handlers.

The cool thing about servant that it supports building handlers 
not only with builtin monad `Servant.Handler` but also with other
monads that can be converted to it.

So we can build server with plain `IO`:

```haskell
-- | Service interfaces by methods
data Env = Env
  { save        :: Save.Env
  , getMessage  :: GetMessage.Env
  , listTag     :: ListTag.Env
  , toggleLogs  :: ToggleLog.Env
  }

-- | Servant server for the app
server :: Env -> ServerT Api IO
server env =
       Save.handle env.save
  :<|> GetMessage.handle env.getMessage
  :<|> ListTag.handle env.listTag
  :<|> ToggleLog.handle env.toggleLogs
```

And in the `app` to launch the server we use `serveWithContextT` with our
custom lifting of `IO` to `Handler` monad:

```haskell
import Control.Exception (try)
import Control.Monad.Except (ExceptT(..))
...

run config.port $ serveWithContextT (Proxy :: Proxy Api) EmptyContext toHandler $ server env
  where
    toHandler :: IO resp -> Servant.Handler resp
    toHandler  = Handler . ExceptT . try
```

#### Custom error messages

For simplicity we use plain `Text` error messages but in real app 
we should define more fine grained type for ApiError that we can convert 
to servant errors.

Also as we work with plain `IO`-monad we need to convert our
exceptions to servant ones so that they can be handled properly.
For that we use custom `throwApi` function:

```haskell
import Control.Exception      (throwIO)
import Data.ByteString.Lazy   qualified as BL
import Data.Text.Encoding     qualified as Text
...

throwApi :: ApiError -> IO a
throwApi = throwIO . toServantError
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


### Keep your interfaces small

I'd like to avoid having one big `Env` record type that declares all possible
interfaces of the server. Instead of this it's much better
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

handle :: Env -> Tag -> IO [Message]
handle (Env Db{..} Log{..}) tag = do
  logInfo $ "list tag call: " <> display tag
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
handle :: Env -> Tag -> IO [Message]
```

By imports we can see that it depends on common `Types` and common logger interface:

```haskell
import DI.Log
import Types
```

so with this approach we don't rely on Servant or on big one-for-all `Env`.
we keep it small, simple and local to the method. 
It would be painless to make a microservice out of it.

But how we use it in the service? In the service we have that big `Env`.
It contains all enironments for the methods:

```haskell
-- | Service environment by methods
data Env = Env
  { save        :: Save.Env
  , getMessage  :: GetMessage.Env
  , listTag     :: ListTag.Env
  , toggleLogs  :: ToggleLog.Env
  }

server :: Env -> ServerT Api IO
server env =
       Save.handle env.save
  :<|> GetMessage.handle env.listTag
```

Here we instantiate concrete set of interfaces for the API-method.
This way by local environment definition we can see which dependencies are used.
For example the `ToggleLog` method uses only `Log` and `Setup` services
and modification of `DB` or `Time` interface will not affect it:

```haskell
module Server.ToggleLog where

data Env = Env
  { log   :: Log
  , setup :: Setup
  }

handle :: Env -> IO ()
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
handle :: Env -> Tag -> IO [Message]
handle (Env Db{..} Log{..} Foo{..}) tag = do
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

We have discused a Handler pattern and how to use it to build flexible
web-apps with servant that are easy to change and keep development with the wave.

Let's mention the points:

* we can organise application with interfaces as records

* with this approach we can decopule implementation from the servant details

* we can use local environments ior groups of interfaces 
    for API-routes and assemble them in the last stage on level
   of the server definition or we can swap it to microservice design.

* local interfaces that are driven by the API-routes give more flexible solutions
   that are local to the compiling routine and more easy to think about at price of possible
  duplication.

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
