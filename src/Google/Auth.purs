module Google.Auth where

import Prelude

import Control.Monad.Aff (Aff, Canceler(Canceler), cancel, makeAff, makeAff', runAff)
import Control.Monad.Aff.Console (log) as A
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error, error, throwException)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Timer (TIMER, clearInterval, setInterval, setTimeout)
import Control.Monad.Except (runExcept, throwError)
import Control.MonadPlus (guard)
import Control.Promise (Promise, toAff)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Event.EventTypes (click)
import DOM.HTML.Types (htmlDocumentToDocument, htmlDocumentToNonElementParentNode, htmlElementToNode)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElement)
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (appendChild)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), elementToEventTarget, elementToNode)
import Data.Either (either)
import Data.Foreign (F, Foreign, ForeignError(..), isUndefined, toForeign)
import Data.Foreign.Index (readProp)
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable)
import Debug.Trace (traceAnyA)
import Network.HTTP.Affjax (AJAX)
import Unsafe.Coerce (unsafeCoerce)

-- TODO poll and cancelWhen are extremely messy. Apparently Aff allows you to succeed or fail multiple times. This feels like an error to me, but I'm working around it in those two functions

-- TODO a bunch of the handling of the return types is extremely unsafe, like getAuthResponse
-- We should probably use Generic decoding to make this and other functions here safer

testClientId = ClientId "494993385239-vqv6bqt5hi2pjgq87bsod41m8h9qum0t.apps.googleusercontent.com"

main = runAff
         (log <<< show)
         attachHandler
         (prepare {timeout: 5000, clientId: testClientId}) 

attachHandler ga = do
  b <- getButton >>= maybe (throwException (error "No button")) pure
  addEventListener click (eventListener \_ -> go ga) false (elementToEventTarget b)

getButton :: forall t75. Eff ( dom :: DOM | t75) (Maybe Element)
getButton = window >>=
       document >>=
       htmlDocumentToNonElementParentNode >>>
       getElementById (ElementId "go")

go :: forall e. GoogleAuth -> Eff (console :: CONSOLE, dom :: DOM, timer :: TIMER, ajax :: AJAX | e) Unit
go ga = void $ runAff 
       (log <<< show) 
       (\user -> do
           traceAnyA user
           traceAnyA (user # getBasicProfile # getEmail))
       (signIn ga)

type LoginProps = { timeout :: Int
                  , clientId :: ClientId
                  }


-- | This procedure will:
-- | * import the google platform js bundle
-- | * wait for it to be downloaded (for as long as the specified timeout)
-- | * Load the "auth2" part of the google platform
-- | * Initialize the Google Auth2 object with the provided clientId
-- | Any technical errors are thrown into the Aff
-- | This all happens in the background: the user sees nothing yet
prepare :: forall e. LoginProps -> Aff (dom :: DOM, timer :: TIMER, ajax :: AJAX, console :: CONSOLE, ref :: REF | e) GoogleAuth
prepare props = do
  -- add src element for sdk to document
  doc <- liftEff (window >>= document)
  body <- liftEff (body doc) >>= maybe (throwError $ error "No body found") pure
  scr <- liftEff $ createElement "script" (htmlDocumentToDocument doc)
  liftEff $ setAttribute "src" "https://apis.google.com/js/platform.js" scr
  _ <- liftEff $ appendChild (elementToNode scr) (htmlElementToNode body)

  A.log "before polling"
  -- poll for window.gapi = Google Platform global object
  forn <- cancelWhen (\cancel -> void $ setTimeout props.timeout cancel)
                     (error "Couldn't find gapi for too long")
                     (poll 50 lookForGapi)
  A.log "after polling"

  -- Decode the "gapi" object
  gapi :: Gapi <- either (head >>> show >>> error >>> throwError) pure (runExcept $ readGapi forn)


  -- Load auth2 from the google platform
  lgapi <- loadAuth2 gapi

  -- Initialize the Gapi object with your application's key
  liftEff (initAuth lgapi props.clientId) >>= toAff
  where
    lookForGapi = (window <#> toForeign
                   >>= readProp "gapi"
                   >>> runExcept
                   >>> either
                         (\_ -> pure Nothing)
                         (\forn -> pure $ if isUndefined forn then Nothing else Just forn))
    
-- | Guide the user through the "Sign in with Google" workflow
signIn :: forall e. GoogleAuth -> Aff (dom :: DOM, ajax :: AJAX | e) GoogleUser
signIn = signInImpl >>> liftEff >=> toAff

foreign import data Gapi :: Type
foreign import data LoadedGapi :: Type
foreign import data GoogleAuth :: Type
foreign import data GoogleUser :: Type
foreign import data GoogleBasicProfile :: Type
type GoogleAuthResponse =
  { access_token :: String
  , id_token :: IdToken
  , login_hint :: String
  , scope :: String
  , expires_in :: String
  , first_issued_at :: String
  , expires_at :: String
  }
newtype ClientId = ClientId String
newtype IdToken = IdToken String

foreign import loadAuth2Impl :: forall e.
  Gapi
  -> (Error -> Eff (ajax :: AJAX | e) Unit)
  -> (LoadedGapi -> Eff (ajax :: AJAX | e) Unit)
  -> Eff (ajax :: AJAX | e) Unit
foreign import initAuth :: forall e.
                           LoadedGapi
                           -> ClientId
                           -> Eff (dom :: DOM | e) (Promise GoogleAuth)
foreign import signInImpl :: forall e. GoogleAuth -> Eff (dom :: DOM, ajax :: AJAX | e) (Promise GoogleUser)
foreign import getBasicProfile :: GoogleUser -> GoogleBasicProfile
foreign import getEmail :: GoogleBasicProfile -> Nullable String -- Nullable??
foreign import getAuthResponse :: GoogleUser -> GoogleAuthResponse

loadAuth2 :: forall e. Gapi -> Aff (ajax :: AJAX | e) LoadedGapi
loadAuth2 = loadAuth2Impl >>> makeAff

readGapi :: Foreign -> F Gapi
readGapi gapi =
   either
       (\_ -> throwError (pure $ ForeignError "gapi global object not recognized"))
       (\_ -> pure (cast gapi))
       (runExcept $ readProp "_pl" gapi)
    where
      cast :: Foreign -> Gapi
      cast = unsafeCoerce

-- | Probably belongs in an Aff package somewhere
-- | Runs the eff computation every <interval> times
-- | Once the eff computation returns a Just, it's returned inside the Aff
-- | Cancel the Aff to stop polling, eg: when the polling takes too long
poll :: forall e a.
        Int
        -> Eff (timer :: TIMER, ref :: REF | e) (Maybe a)
        -> Aff (timer :: TIMER, ref :: REF | e) a
poll interval runEff = do
  makeAff' (\errCb succCb -> do 
    intervalRef <- newRef Nothing
    iid <- setInterval
             interval
             (runEff >>= maybe (pure unit)
                               (\res -> readRef intervalRef
                                        >>= maybe (succCb res) -- Should never happen
                                                  (\id -> clearInterval id *> succCb res)))
    writeRef intervalRef (Just iid)
    pure $ Canceler (\_ -> liftEff (clearInterval iid) *> pure true))

-- | Probably belongs in an Aff package somewhere, after the API is improved
-- | Provide a cancelling function, an error, and an Aff, and we'll return an Aff
-- | that is errored when the cancellation happens
cancelWhen :: forall e a. (Eff (ref :: REF | e) Unit -> Eff (ref :: REF | e) Unit) -> Error -> Aff (ref :: REF | e) a -> Aff (ref :: REF | e) a
cancelWhen cancellingFunc err aff = do
  done <- liftEff $ newRef false -- Seems I can both succeed AND fail the Aff... So I keep this ref to only succeed or fail once. Feels extremely messy and an error in Aff
  makeAff \fail succeed -> void do
    let guardOnce eff = (readRef done) >>= \isDone -> if isDone then pure unit else eff
    canceller <- runAff (\e -> guardOnce $ writeRef done true *> fail e)
                        (\a -> guardOnce $ writeRef done true *> succeed a)
                        aff
    cancellingFunc (do guardOnce $ void $ runAff fail (\_ -> fail err) (cancel canceller err))
