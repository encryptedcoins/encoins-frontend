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

function copyElemContent(elId) {
  var el = document.getElementById(elId);
  if (el != null && navigator && navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(el.innerText);
  }
};

function saveTextFile(txt) {
  var element = document.createElement('a');
  element.setAttribute('href', 'data:application/octet-stream,' + encodeURIComponent(txt));
  element.setAttribute('download', 'key.txt');
  element.style.display = 'none';
  document.body.appendChild(element);
  element.click();
  document.body.removeChild(element);
};

function setElementText(elId, val) {
  var el = document.getElementById(elId);
  if (el != null) {
    el.innerTextvalue = val;
  };
};

function setInputValue(elId, val) {
  var el = document.getElementById(elId);
  if (el != null) {
    el.value = val;
    var eChange = new Event('change');
    var eInput = new Event('input');
    el.dispatchEvent(eChange);
    el.dispatchEvent(eInput);
  };
};

//////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// App functions //////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

async function walletAPI(walletName) {
  switch (walletName) {
    case "nami":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.nami !== 'undefined'))
        return window.cardano.nami.enable();
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.nami is not found"); });
    case "eternl":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.eternl !== 'undefined'))
        return window.cardano.eternl.enable();
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.eternl is not found"); });
    default:
      return new Promise(() => { throw new Error("Wallet's not identified"); });
  }
}

function metaWallet(method) {
  return (walletName/*, ...args*/) => {
    return walletAPI(walletName).then((api) => { return api[method](/*...args*/) });
  }
}

function walletEnable(walletName, resId) {
  const w = walletAPI(walletName);
  w.then(() => { setInputValue(resId, "true"); }, () => { setInputValue(resId, "error_walletAPI"); });
};

let walletBalance = metaWallet("getBalance");
let walletNetworkId = metaWallet("getNetworkId");

// let walletUtxos = metaWallet("getUtxos");
function walletUtxos(walletName, amount, paginate) {
  return walletAPI(walletName).then((api) => { return api.getUtxos(amount, paginate) });
}

let walletCollateral = metaWallet("getCollateral");
let walletUnusedAddresses = metaWallet("getUnusedAddresses");
let walletChangeAddress = metaWallet("getChangeAddress");
let getRewardAddresses = metaWallet("getRewardAddresses");

function walletSignTx(walletName, tx, partialSign) {
  return walletAPI(walletName).then((api) => { return api.signTx(tx, partialSign) });
}

function walletSubmitTx(walletName, tx) {
  return walletAPI(walletName).then((api) => { return api.signTx(tx) });
}

function metaWalletView(method) {
  return (walletName, resId) => {
    return metaWallet(method)(walletName)
      .then((res) => { setInputValue(resId, res); }
        , () => { setInputValue(resId, "error_walletAPI"); });
  }
}

let walletAddress = metaWalletView("getChangeAddress");

function walletAddressBech32(walletName, resId) {
  loader.load().then(() => {
    const CardanoWasm = loader.Cardano;
    walletChangeAddress(walletName).then((res) => {
      const address = CardanoWasm.Address.from_bytes(fromHexString(res)).to_bech32();
      setInputValue(resId, address);
    }, () => { setInputValue(resId, "error_walletAPI"); });
  });
};

function walletAddressBech32ToBytes(addressBech32, resId) {
  loader.load().then(() => {
    const CardanoWasm = loader.Cardano;
    const address = toHexString(CardanoWasm.Address.from_bech32(addressBech32).to_bytes());
    setInputValue(resId, address);
  });
};

async function encoinsTx(walletName, partialTx, red, resId) {
  // loading CardanoWasm
  await loader.load();
  const CardanoWasm = loader.Cardano;

  //loading wallet
  const api = await walletAPI(walletName);

  // loading wallet's change address
  const changeAddressHex = await api.getChangeAddress();
  const changeAddress = CardanoWasm.Address.from_bytes(fromHexString(changeAddressHex));

  // loading wallet's utxos
  const valueToSpend = CardanoWasm.Value.new(CardanoWasm.BigNum.from_str("10000000"));
  const utxos = await api.getUtxos(toHexString(valueToSpend.to_bytes()), undefined);

  console.log("Transaction to sign:");
  console.log(partialTx);

  const signedTx = await api.signTx(partialTx);
  console.log(signedTx);
};

// Convert a hex string to a byte array
function fromHexString(hex) {
  for (var bytes = [], c = 0; c < hex.length; c += 2)
    bytes.push(parseInt(hex.substr(c, 2), 16));
  return bytes;
}

function toHexString(byteArray) {
  return Array.from(byteArray, function (byte) {
    return ('0' + (byte & 0xFF).toString(16)).slice(-2);
  }).join('')
}