exports.loadAuth2Impl = function(gap){
  return function(onerr){
    return function(onSucc){
      return function(){
        try{
          gap.load("auth2", function(){
            onSucc(gap)();
          });
        } catch (e){
          onerr(e)();
        }
      };
    };
  };
};

exports.initAuth = function(gap){
  return function(clientId){
    return function(){
      return gap.auth2.init({client_id: clientId});
    };
  };
};
exports.signInImpl = function(gauth){
  return function(){
    return gauth.signIn();
  };
};
exports.getAuthResponse = function(gu){
  return gu.getAuthResponse();
};
exports.getBasicProfile = function(gu){
  return gu.getBasicProfile();
};
exports.getEmail = function(gbp){
  return gbp.getEmail();
};
