//////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// WebPage functions ////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

function runHeadScripts() {
  // Loading modules
  loader.load();

  // Webflow
  WebFont.load(
    {
      google: {
        families:
          ['Droid Serif:400,400italic,700,700italic',
            'Corben:regular', 'Fenix:regular']
      }
    });
  !function (o, c) {
    var n = c.documentElement, t = ' w-mod-';
    n.className += t + 'js';
    if ('ontouchstart' in o || o.DocumentTouch && c instanceof DocumentTouch) {
      n.className += t + 'touch';
    }
  }(window, document);
};
