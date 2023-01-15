//////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// WebPage functions ////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

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

async function walletLoad(walletName, networkIdElement, balanceElement, changeAddressElement, changeAddressBech32Element,
  pubKeyHashElement, stakeKeyHashElement, collateralElement, utxosElement, unusedAddressesElement, rewardAddressesElement)
{
  console.log("begin walletLoad");
  await loader.load();
  const CardanoWasm = loader.Cardano;
  const api         = await walletAPI(walletName);

  const networkId           = await api.getNetworkId();
  setInputValue(networkIdElement, networkId);

  const balance             = await api.getBalance();
  setInputValue(balanceElement, balance);

  const changeAddress       = await api.getChangeAddress();
  const changeAddressBech32 = CardanoWasm.Address.from_bytes(fromHexString(changeAddress)).to_bech32();
  const baseAddress         = CardanoWasm.BaseAddress.from_address(CardanoWasm.Address.from_bech32(changeAddressBech32));
  const pubKeyHash          = toHexString(baseAddress.payment_cred().to_keyhash().to_bytes());
  const stakeKeyHash        = toHexString(baseAddress.stake_cred().to_keyhash().to_bytes());
  setInputValue(changeAddressElement, changeAddress);
  setInputValue(changeAddressBech32Element, changeAddressBech32);
  setInputValue(pubKeyHashElement, pubKeyHash);
  setInputValue(stakeKeyHashElement, stakeKeyHash);
  
  const collateral          = await api.experimental.getCollateral();
  setInputValue(collateralElement, collateral);

  const utxos               = await api.getUtxos();
  const utxosJSON           = [];
  for (i = 0; i<utxos.length; i++)
  {
    utxosJSON.push(CardanoWasm.TransactionUnspentOutput.from_bytes(fromHexString(utxos[i])).to_json());
  }
  setInputValue(utxosElement, "[" + utxosJSON.join(', ') + "]");

  const unusedAddresses     = await api.getUnusedAddresses();
  setInputValue(unusedAddressesElement, unusedAddresses);

  const rewardAddresses     = await api.getRewardAddresses();
  setInputValue(rewardAddressesElement, rewardAddresses);
  console.log("end walletLoad");
}

async function walletSignTx(walletName, partialTxHex, walletSignatureElement)
{
  // loading CardanoWasm
  await loader.load();
  const CardanoWasm = loader.Cardano;

  //loading wallet
  const api = await walletAPI(walletName);

  console.log("Transaction to sign:");
  console.log(partialTxHex);

  const walletSignatureWitnessSetBytes = await api.signTx(partialTxHex, true);
  const walletSignature = CardanoWasm.TransactionWitnessSet.from_bytes(fromHexString(walletSignatureWitnessSetBytes)).vkeys().get(0).to_json();

  setInputValue(walletSignatureElement, walletSignature);
  console.log(walletSignature);
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
  setInputValue(resId, toHexString(hashArray));
}

// NOTE: this is for testing purposes
// signs a hex string 'msg' with Ed25519 private key 'prvKey'
async function ed25519Sign(prvKey, msg, resId)
{
  const sig = await window.nobleEd25519.sign(msg, prvKey);
  setInputValue(resId, toHexString(sig));
}