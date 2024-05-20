//////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// WebPage functions ////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

// After heads loaded, run the script
function runHeadScripts() {

  // Dex hunter
  const dexHunterApp = React.createElement(
    dexhunterSwap,
    { "autofocus": "false", "orderTypes": ["SWAP", "LIMIT"], "colors": { "background": "#0E0F12", "containers": "#191B23", "subText": "#88919E", "mainText": "#FFFFFF", "buttonText": "#FFFFFF", "accent": "#007DFF" }, "theme": "dark", "width": 450, "partnerCode": "encoins61646472317138753272683575756436797a6d687130646537767437703072767170666164776e656533746a6e7a32746c347263743671743033776a63326c667779716e6435346777666465793530733733343265336a6c366b78777777346b717a6661683278da39a3ee5e6b4b0d3255bfef95601890afd80709", "partnerName": "Encoins" }
  );

  const dexHunterNode = ReactDOM.createRoot(
    document.getElementById('dexhunter-root')
  );

  dexHunterNode.render(dexHunterApp);

  // Webflow
  WebFont.load({
    google: {
      families: ["Poppins:regular,italic,500,500italic,600,600italic,700,700italic,800,800italic", "Mulish:regular,500", "Inter:regular,500,700"]
    }
  });
  !function (o, c) {
    var n = c.documentElement, t = " w-mod-";
    n.className += t + "js", ("ontouchstart" in o || o.DocumentTouch && c instanceof DocumentTouch) && (n.className += t + "touch")
  }(window, document);
};

function scrollIntoView(elemId) {
  const elem = document.getElementById(elemId);
  elem.scrollIntoView();
}