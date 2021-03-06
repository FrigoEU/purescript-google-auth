module Node.Google.Auth where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (Error)
import Google.Auth (ClientId, IdToken)


foreign import data GOOGLEAUTH :: Effect
foreign import data OAuth2 :: Type
foreign import data LoginTicket :: Type
foreign import data Payload :: Type
foreign import getOAuth2 :: ClientId -> OAuth2
foreign import getPayload :: LoginTicket -> Payload
-- TODO hoe haal je vanuit Payload emailadres?
foreign import verifyIdTokenImpl ::
  forall e.
  IdToken
  -> ClientId
  -> (Error -> Eff (googleauth :: GOOGLEAUTH | e) Unit)
  -> (LoginTicket -> Eff (googleauth :: GOOGLEAUTH | e) Unit)
  -> Eff (googleauth :: GOOGLEAUTH | e) Unit

verifyIdToken :: forall e.
  IdToken -> ClientId -> Aff (googleauth :: GOOGLEAUTH | e) LoginTicket
verifyIdToken i c = makeAff (verifyIdTokenImpl i c)
