function handleCredentialResponse(response) {
  const responsePayload = jwt_decode(response.credential);
  Shiny.onInputChange("g.email", responsePayload.email);
  Shiny.onInputChange("g.name", responsePayload.name);
  
  
  var link = document.getElementById('g_id_onload');
  link.style.display = 'none'; //or
  link.style.visibility = 'hidden';
  link = document.getElementById('g_id_signin');
  link.style.display = 'none'; //or
  link.style.visibility = 'hidden';
}

//window.onload = function () {

   
   /*
  google.accounts.id.initialize({
    client_id: '789616587258-lt9ji16j9u7jp998itd5kivgq249t0v3.apps.googleusercontent.com',
    callback: handleCredentialResponse
  });
  google.accounts.id.prompt();
  */
  /*
  tokenClient = google.accounts.oauth2.initTokenClient({
      client_id: "789616587258-lt9ji16j9u7jp998itd5kivgq249t0v3.apps.googleusercontent.com",
      scope: 'https://www.googleapis.com/auth/userinfo.profile',
      callback: (tokenResponse) => {
          console.log("aaaa", tokenResponse)
      },
  });
  */
  //tokenClient.requestAccessToken();
  
//};

