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

function saveTextFile(fname, txt) {
  var element = document.createElement('a');
  element.setAttribute('href', 'data:application/octet-stream,' + encodeURIComponent(txt));
  element.setAttribute('download', fname);
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

function saveJSON(key, val, encr, pass) {
  var val1 = val;
  if (encr) {
    val1 = CryptoJS.AES.encrypt(val, pass).toString();
  }
  localStorage.setItem(key, val1);
}

function loadJSON(key, resId, decr, pass) {
  const val = localStorage.getItem(key);
  var res = val;
  if (decr) {
    res = CryptoJS.AES.decrypt(val, pass).toString(CryptoJS.enc.Utf8);
  }
  setInputValue(resId, res);
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
        return window.cardano.nami.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.nami is not found"); });
    case "eternl":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.eternl !== 'undefined'))
      {
        return window.cardano.eternl.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.eternl is not found"); });
    case "flint":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.flint !== 'undefined'))
      {
        return window.cardano.flint.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.flint is not found"); });
    case "nufi":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.nufi !== 'undefined'))
      {
        return window.cardano.nufi.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.nufi is not found"); });
    case "gerowallet":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.gerowallet !== 'undefined'))
      {
        return window.cardano.gerowallet.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.gerowallet is not found"); });
    case "begin":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.begin !== 'undefined'))
      {
        return window.cardano.begin.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.begin is not found"); });
    case "begin-nightly":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano["begin-nightly"] !== 'undefined'))
      {
        return window.cardano["begin-nightly"].enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.begin-nightly is not found"); });
    case "typhon":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.typhon !== 'undefined'))
      {
        return window.cardano.typhon.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.typhon is not found"); });
    case "lace":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.lace !== 'undefined'))
      {
        return window.cardano.lace.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.lace is not found"); });
    default:
      return new Promise(() => { setWalletNone(); console.log(walletName); console.log("Wallet: None"); });
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

    const networkId           = await api.getNetworkId();
    setInputValue("networkIdElement", networkId);

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
    var stakeKeyHash = stakeKeyHashCred.to_keyhash();
    if (stakeKeyHash === undefined) {
      stakeKeyHash = "";
    }
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
    console.log(e.message);
    setWalletNone();
    if (e.message.includes("no account set")) { setInputValue("walletErrorElement", "No account set in Eternl wallet."); }
    else { setInputValue("walletErrorElement", "No access to the wallet."); }
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
  setInputValue(resId, toHexString(hashArray));
}

async function ed25519Sign(prvKey, msg, resId)
{
  const sig = await window.nobleEd25519.sign(msg, prvKey);
  setInputValue(resId, toHexString(sig));
}

function setElementStyle(elId, prop, val) {
  var el = document.getElementById(elId);
  if (el != null) {
    el.style.setProperty(prop,val);
  };
};

function pingServer(baseUrl) {
  const request = new XMLHttpRequest();
  request.open('GET', baseUrl + '/ping', false);  // `false` makes the request synchronous
  try {
    request.send(null);
  } catch(e) {
    console.log('ping ' + baseUrl + ' ====> ' + 'failed to send request');
    return false;
  }
  console.log('ping ' + baseUrl + ' ====> ' + request.status);
  return (request.status === 200);
};

function saveHashedTextToStorage(key, val) {
  localStorage.setItem(key, CryptoJS.SHA3(val));
}

function loadHashedPassword(key) {
  const pass = localStorage.getItem(key);
  if (pass == null || pass == CryptoJS.SHA3("")) {
    return "";
  } else {
    return pass;
  }
}

function checkPassword(hash, raw) {
  return (hash == CryptoJS.SHA3(raw));
}

async function addrLoad(addrInput)
{
  await loader.load();
  const CardanoWasm = loader.Cardano;
  try {
    const addrBech32 = CardanoWasm.Address.from_bech32(addrInput);
    const baseAddress = CardanoWasm.BaseAddress.from_address(addrBech32);
    addrBech32.free();
    const pubKeyHashCred = baseAddress.payment_cred();
    const stakeKeyHashCred = baseAddress.stake_cred();
    baseAddress.free();
    var pubKeyHash = pubKeyHashCred.to_keyhash();
    if (pubKeyHash === undefined) {
      pubKeyHash = "";
    }
    var stakeKeyHash = stakeKeyHashCred.to_keyhash();
    if (stakeKeyHash === undefined) {
      stakeKeyHash = "";
    }
    pubKeyHashCred.free();
    stakeKeyHashCred.free();
    const pubKeyHashHex = toHexString(pubKeyHash.to_bytes());
    const stakeKeyHashHex = toHexString(stakeKeyHash.to_bytes());
    pubKeyHash.free();
    stakeKeyHash.free();
    setInputValue("addrPubKeyHashElement", pubKeyHashHex);
    setInputValue("addrStakeKeyHashElement", stakeKeyHashHex);
  } catch (e) {
    console.log(e.message);
    setInputValue("addrPubKeyHashElement", "");
    setInputValue("addrStakeKeyHashElement", "");
    return;
  }
}

//////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// DAO functions //////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

function toUTF8Array(str) {
  var utf8 = [];
  for (var i=0; i < str.length; i++) {
      var charcode = str.charCodeAt(i);
      if (charcode < 0x80) utf8.push(charcode);
      else if (charcode < 0x800) {
          utf8.push(0xc0 | (charcode >> 6),
                    0x80 | (charcode & 0x3f));
      }
      else if (charcode < 0xd800 || charcode >= 0xe000) {
          utf8.push(0xe0 | (charcode >> 12),
                    0x80 | ((charcode>>6) & 0x3f),
                    0x80 | (charcode & 0x3f));
      }
      // surrogate pair
      else {
          i++;
          // UTF-16 encodes 0x10000-0x10FFFF by
          // subtracting 0x10000 and splitting the
          // 20 bits of 0x0-0xFFFFF into two halves
          charcode = 0x10000 + (((charcode & 0x3ff)<<10)
                    | (str.charCodeAt(i) & 0x3ff));
          utf8.push(0xf0 | (charcode >>18),
                    0x80 | ((charcode>>12) & 0x3f),
                    0x80 | ((charcode>>6) & 0x3f),
                    0x80 | (charcode & 0x3f));
      }
  }
  return utf8;
}

async function daoPollVoteTx(n, walletName, answer)
{
  // loading CardanoWasm
  await loader.load();
  const CardanoWasm = loader.Cardano;

  await lucidLoader.load();
  const lucid = await lucidLoader.Lucid.new(
    new lucidLoader.Blockfrost("https://cardano-mainnet.blockfrost.io/api/v0", "mainnetK4sRBCTDwqzK1KRuFxnpuxPbKF4ZQrnl"),
    "Mainnet",
  )

  try {
    //loading wallet
    const api = await walletAPI(walletName);
    lucid.selectWallet(api);

    const changeAddress    = CardanoWasm.Address.from_bytes(fromHexString(await api.getChangeAddress()));
    const baseAddress      = CardanoWasm.BaseAddress.from_address(changeAddress);
    const stakeKeyHashCred = baseAddress.stake_cred();
    const stakeKeyHash     = stakeKeyHashCred.to_keyhash();

    const plc_lst = CardanoWasm.PlutusList.new();
    const tag1 = CardanoWasm.PlutusData.new_bytes(toUTF8Array("ENCOINS"));
    const tag2 = CardanoWasm.PlutusData.new_bytes(toUTF8Array("Poll #" + n));
    const tag3 = CardanoWasm.PlutusData.new_bytes(stakeKeyHash.to_bytes());
    const tag4 = CardanoWasm.PlutusData.new_bytes(toUTF8Array(answer));
    plc_lst.add(tag1);
    plc_lst.add(tag2);
    plc_lst.add(tag3);
    plc_lst.add(tag4);
    const plc_msg = CardanoWasm.PlutusData.new_list(plc_lst);

    const tx = await lucid.newTx()
      .addSignerKey(toHexString(stakeKeyHash.to_bytes()))
      .payToAddressWithData(changeAddress.to_bech32(), { inline: toHexString(plc_msg.to_bytes()) }, { lovelace: 1500000n })
      .complete();

    const signedTx = await tx.sign().complete();

    const txHash = await signedTx.submit();
    console.log(txHash);

    setInputValue("elementPoll" + n, "Thank you for voting! Come back later to see the results.");

    changeAddress.free();
    baseAddress.free();
    stakeKeyHashCred.free();
    stakeKeyHash.free();

    plc_lst.free();
    tag1.free();
    tag2.free();
    tag3.free();
    tag4.free();
    plc_msg.free();
  } catch (e) {
    console.log("Error: " + e.message);
    return;
  }
};