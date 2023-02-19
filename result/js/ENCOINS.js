//////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// WebPage functions ////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

function copyText(txt) {
  if (navigator && navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(txt);
  }
};

function copyElemContent(elId) {
  var el = document.getElementById(elId);
  if (el != null) {
    navigator.clipboard.writeText(copyText(el.innerText));
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

function saveJSON(key, val) {
  localStorage.setItem(key, val);
}

function loadJSON(key, resId) {
  const val = localStorage.getItem(key);
  setInputValue(resId, val);
}

//////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// App functions //////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

function setWalletNone() {
  setInputValue("walletNameElement", "None");
  setInputValue("changeAddressBech32Element", "");
  setInputValue("pubKeyHashElement", "");
  setInputValue("stakeKeyHashElement", "");
  setInputValue("utxosElement", "[]");
}

async function walletAPI(walletName) {
  switch (walletName) {
    case "nami":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.nami !== 'undefined'))
      {
        console.log("Wallet: Nami");
        return window.cardano.nami.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.nami is not found"); });
    case "eternl":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.eternl !== 'undefined'))
      {
        console.log("Wallet: Eternl");
        return window.cardano.eternl.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.eternl is not found"); });
    case "flint":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.flint !== 'undefined'))
      {
        console.log("Wallet: Flint");
        return window.cardano.flint.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.flint is not found"); });
    case "nufi":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.nufi !== 'undefined'))
      {
        console.log("Wallet: NuFi");
        return window.cardano.nufi.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.nufi is not found"); });
    case "gerowallet":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.gerowallet !== 'undefined'))
      {
        console.log("Wallet: Gero");
        return window.cardano.gerowallet.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.gerowallet is not found"); });
    case "begin":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.begin !== 'undefined'))
      {
        console.log("Wallet: Begin");
        return window.cardano.begin.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.begin is not found"); });
    case "typhon":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.typhon !== 'undefined'))
      {
        console.log("Wallet: Typhon");
        return window.cardano.typhon.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.typhon is not found"); });
    case "lace":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.lace !== 'undefined'))
      {
        console.log("Wallet: Lace");
        return window.cardano.lace.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.lace is not found"); });
    default:
      return new Promise(() => { setWalletNone(); console.log("Wallet: None"); });
  }
}

async function walletLoad(walletName)
{
  console.log("begin walletLoad");
  await loader.load();
  const CardanoWasm = loader.Cardano;
  try {
    const api = await walletAPI(walletName);
    
    setInputValue("walletNameElement", walletName);

    // const networkId           = await api.getNetworkId();
    // setInputValue(networkIdElement, networkId);

    // const balance             = await api.getBalance();
    // setInputValue(balanceElement, balance);

    const changeAddress       = CardanoWasm.Address.from_bytes(fromHexString(await api.getChangeAddress()));
    const changeAddressBech32 = changeAddress.to_bech32();
    const baseAddress         = CardanoWasm.BaseAddress.from_address(changeAddress);
    changeAddress.free();
    const pubKeyHashCred = baseAddress.payment_cred();
    const stakeKeyHashCred = baseAddress.stake_cred();
    baseAddress.free();
    const pubKeyHash = pubKeyHashCred.to_keyhash();
    const stakeKeyHash = stakeKeyHashCred.to_keyhash();
    pubKeyHashCred.free();
    stakeKeyHashCred.free();
    const pubKeyHashHex          = toHexString(pubKeyHash.to_bytes());
    const stakeKeyHashHex        = toHexString(stakeKeyHash.to_bytes());
    pubKeyHash.free();
    stakeKeyHash.free();
    setInputValue("changeAddressBech32Element", changeAddressBech32);
    setInputValue("pubKeyHashElement", pubKeyHashHex);
    setInputValue("stakeKeyHashElement", stakeKeyHashHex);
    
    // const collateral          = await api.experimental.getCollateral();
    // setInputValue(collateralElement, collateral);

    const utxos               = await api.getUtxos();
    const utxosJSON           = [];
    for (i = 0; i<utxos.length; i++)
    {
      const utxo = CardanoWasm.TransactionUnspentOutput.from_bytes(fromHexString(utxos[i]));
      utxosJSON.push(utxo.to_json());
      utxo.free();
    }
    setInputValue("utxosElement", "[" + utxosJSON.join(', ') + "]");

    // const unusedAddresses     = await api.getUnusedAddresses();
    // setInputValue(unusedAddressesElement, unusedAddresses);

    // const rewardAddresses     = await api.getRewardAddresses();
    // setInputValue(rewardAddressesElement, rewardAddresses);
    console.log("end walletLoad");
  } catch (e) {
    setWalletNone();
    setInputValue("walletErrorElement", "No access to the wallet.");
    return;
  }
}

async function walletSignTx(walletName, partialTxHex)
{
  // loading CardanoWasm
  await loader.load();
  const CardanoWasm = loader.Cardano;

  try {
    //loading wallet
    const api = await walletAPI(walletName);

    console.log("Transaction to sign:");
    console.log(partialTxHex);

    const walletSignatureWitnessSetBytes = await api.signTx(partialTxHex, true);
    const walletSignatureWitnessSet = CardanoWasm.TransactionWitnessSet.from_bytes(fromHexString(walletSignatureWitnessSetBytes));
    const walletSignatures = walletSignatureWitnessSet.vkeys();
    walletSignatureWitnessSet.free();

    const resultJSON = [];
    for (i=0; i<walletSignatures.len(); i++)
    {
      const walletSignature = walletSignatures.get(i);
      const vkey = walletSignature.vkey();
      const public_key = vkey.public_key();
      const signature = walletSignature.signature();
      resultJSON.push('{ \"vkey\": \"' + public_key.to_hex() + 
        '\", \"signature\": \"' + signature.to_hex() + '\" }');
      walletSignature.free();
      vkey.free();
      public_key.free();
      signature.free();
    }
    walletSignatures.free();

    setInputValue("walletSignatureElement", "[" + resultJSON.join(', ') + "]");
    console.log("[" + resultJSON.join(', ') + "]");
  } catch (e) {
    console.log(e);
    setInputValue("walletErrorElement", "Transaction declined.");
    return;
  }
};

async function walletSubmitTx(walletName, txHex)
{
  const api = await walletAPI(walletName);
  return api.signTx(txHex);
}

// Convert a hex string to a byte array
function fromHexString(hex)
{
  for (var bytes = [], c = 0; c < hex.length; c += 2)
    bytes.push(parseInt(hex.substr(c, 2), 16));
  return bytes;
}

// Convert a byte array to a hex string
function toHexString(byteArray)
{
  return Array.from(byteArray, function (byte) {
    return ('0' + (byte & 0xFF).toString(16)).slice(-2);
  }).join('')
}

// takes a hex string 'str' and puts the hash as a hex string into an element 'resId'
async function sha2_256(str, resId)
{
  const hashBuffer = await crypto.subtle.digest('SHA-256', new Uint8Array(fromHexString(str)));
  const hashArray = Array.from(new Uint8Array(hashBuffer));
  console.log("inside sha");
  setInputValue(resId, toHexString(hashArray));
}

// NOTE: this is for testing purposes
// signs a hex string 'msg' with Ed25519 private key 'prvKey'
async function ed25519Sign(prvKey, msg, resId)
{
  const sig = await window.nobleEd25519.sign(msg, prvKey);
  setInputValue(resId, toHexString(sig));
}