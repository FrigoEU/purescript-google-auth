exports.getOAuth2 = function(clientId){
  var auth = new require("google-auth-library")();
  var client = new auth.OAuth2(clientId, "", "");
  return client;
};

exports.verifyIdTokenImpl = function(idToken){
  return function(clientId){
    return function(onErr){
      return function(onSucc){
        return function(){
          client.verifyIdToken(idToken, clientId, function(e, login){
            if (e){
              onErr(e);
            } else {
              onSucc(login);
            }
          });
        };
      };
    };
  };
};

exports.getPayload = function(loginTicket){
  return loginTicket.getPayload();
};
