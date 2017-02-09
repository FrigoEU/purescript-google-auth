module Google.Auth where

import Prelude
import Control.Monad.Aff (Aff, attempt, launchAff, makeAff, runAff)
import Control.Monad.Aff.AVar (AVAR, AVar, killVar, makeVar, putVar, takeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, throw)
import Control.Monad.Eff.Ref (newRef, writeRef)
import Control.Monad.Eff.Timer (TIMER, clearInterval, clearTimeout, setInterval, setTimeout)
import Control.Monad.Except (runExcept, throwError)
import Control.Promise (Promise, toAff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlDocumentToDocument, htmlElementToNode)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElement)
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (elementToNode)
import Data.Either (Either(..), either, isRight)
import Data.Foreign (F, Foreign, ForeignError(..), isUndefined, toForeign)
import Data.Foreign.Index (prop)
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe)
import Debug.Trace (traceAnyA)
import Network.HTTP.Affjax (AJAX)
import Unsafe.Coerce (unsafeCoerce)

main :: forall e. Eff (console :: CONSOLE, dom :: DOM, err :: EXCEPTION, avar :: AVAR, timer :: TIMER, ajax :: AJAX | e) Unit
main = void $ runAff (log <<< show) (\user -> do
  traceAnyA user
  traceAnyA (user # getBasicProfile # getEmail)) (login 5000 >>= signIn)

foreign import data Gapi :: *
foreign import data LoadedGapi :: *
foreign import data GoogleAuth :: *
foreign import data GoogleUser :: *
foreign import data GoogleBasicProfile :: *
newtype ClientId = ClientId String

foreign import loadAuth2Impl :: forall e.
  Gapi
  -> (Error -> Eff (ajax :: AJAX | e) Unit)
  -> (LoadedGapi -> Eff (ajax :: AJAX | e) Unit)
  -> Eff (ajax :: AJAX | e) Unit
foreign import init :: forall e.
                       LoadedGapi
                       -> ClientId
                       -> Eff (dom :: DOM | e) (Promise GoogleAuth) -- TODO effect
foreign import signInImpl :: forall e. GoogleAuth -> Eff (dom :: DOM, ajax :: AJAX | e) (Promise GoogleUser)
foreign import getBasicProfile :: GoogleUser -> GoogleBasicProfile -- TODO deel kan misschien met Foreign module gedaan worden ipv met FFI
foreign import getEmail :: GoogleBasicProfile -> Nullable String -- Nullable??

loadAuth2 :: forall e. Gapi -> Aff (ajax :: AJAX | e) LoadedGapi
loadAuth2 gapi = makeAff $ loadAuth2Impl gapi

signIn :: forall e. GoogleAuth -> Aff (dom :: DOM, ajax :: AJAX | e) GoogleUser
signIn = signInImpl >>> liftEff >=> toAff

login :: forall e. Int -> Aff (dom :: DOM, err :: EXCEPTION, avar :: AVAR, timer :: TIMER, ajax :: AJAX | e) GoogleAuth
login timeout = do
  -- add src element for sdk to document
  liftEff $ do
    doc <- window >>= document
    body <- body doc <#> toMaybe >>= maybe (throw "No body found") pure
    scr <- createElement "script" (htmlDocumentToDocument doc)
    setAttribute "src" "https://apis.google.com/js/platform.js" scr
    appendChild (elementToNode scr) (htmlElementToNode body)

    -- poll for window.gapi
  forn <- poll (window <#> toForeign >>= prop "gapi"
                >>> runExcept
                >>> either
                      (\_ -> pure Nothing)
                      (\forn -> pure $ if isUndefined forn then Nothing else Just forn)) 50 5000

  gapi :: Gapi <- liftEff $ either (head >>> show >>> throw) pure (runExcept $ readGapi forn)
  lgapi <- loadAuth2 gapi
  liftEff (init lgapi (ClientId "494993385239-vqv6bqt5hi2pjgq87bsod41m8h9qum0t.apps.googleusercontent.com")) >>= toAff

poll :: forall e a.
        Eff (err :: EXCEPTION, avar :: AVAR, timer :: TIMER | e) (Maybe a)
        -> Int
        -> Int
        -> Aff (err :: EXCEPTION, avar :: AVAR, timer :: TIMER | e) a
poll eff interval timeout = do
  var :: AVar a <- makeVar
  iid <- liftEff $ setInterval interval do
    liftEff $ eff >>= maybe (pure unit) (putVar var >>> launchAff >>> void)
  tid <- liftEff $ setTimeout timeout do
    clearInterval iid
    void $ launchAff $ killVar var (error "Timeout reached, polling for too long")
  a <- takeVar var
  liftEff $ clearTimeout tid
  liftEff $ clearInterval iid
  pure a


readGapi :: Foreign -> F Gapi
readGapi gapi =
   either
       (\_ -> throwError (pure $ ForeignError "gapi global object not recognized"))
       (\_ -> pure (cast gapi))
       (runExcept $ prop "_pl" gapi)
    where
      cast :: Foreign -> Gapi
      cast = unsafeCoerce
