exports.loadAuth2Impl = function(gap){
  debugger;
  return function(onerr){
    return function(onSucc){
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

exports.init = function(gap){
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
exports.getBasicProfile = function(gu){
  debugger;
  return gu.getBasicProfile();
};
exports.getEmail = function(gbp){
  debugger;
  return gbp.getEmail();
};
