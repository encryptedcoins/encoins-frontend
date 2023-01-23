//////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// WebPage functions ////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

function runHeadScripts() {
  // Loading modules
  loader.load();

  // Webflow
  WebFont.load({
    google: {
      families: ["Poppins:regular,italic,500,500italic,600,600italic,700,700italic,800,800italic","Mulish:regular,500","Inter:regular,500,700"]
    }
  });
  !function(o,c) {
    var n = c.documentElement, t = " w-mod-";
    n.className += t+"js", ("ontouchstart" in o || o.DocumentTouch && c instanceof DocumentTouch) && (n.className += t + "touch")
  }(window,document);
};

function scrollIntoView(elemId) {
  const elem = document.getElementById(elemId);
  elem.scrollIntoView();
}