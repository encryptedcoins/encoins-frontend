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

async function saveJSON(key, val, encr, pass) {
  var val1 = val;
  if (encr) {
    val1 = await CryptoNative.encryptAesBase64(pass, val);
  }
  localStorage.setItem(key, val1);
  setInputValue(key, "");
}

async function loadJSON(key, resId, pass, decr) {
  const val = localStorage.getItem(key);
  var res = val;
  try {
    if (val !== null) {
      if (decr) {
        res = await CryptoFallback.decryptAesBase64Fallback(pass, val);
      }
    };
    setInputValue(resId, res);
  } catch (e) {
    console.log("loadJSON: decrypt error: ", e);
    return;
  }
}


function removeKey(key) {
  localStorage.removeItem(key);
  console.log("key", key, "removed");
  return
}

//////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// App functions //////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

function setWalletNone() {
  setInputValue("walletNameElement", "None");
  setInputValue("daoWalletNameNotConnected", "None");
  setInputValue("changeAddressBech32Element", "");
  setInputValue("pubKeyHashElement", "");
  setInputValue("stakeKeyHashElement", "");
  setInputValue("utxosElement", "[]");
}

async function walletAPI(walletName) {
  switch (walletName) {
    case "nami":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.nami !== 'undefined')) {
        return window.cardano.nami.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.nami is not found"); });
    case "eternl":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.eternl !== 'undefined')) {
        return window.cardano.eternl.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.eternl is not found"); });
    case "flint":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.flint !== 'undefined')) {
        return window.cardano.flint.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.flint is not found"); });
    case "nufi":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.nufi !== 'undefined')) {
        return window.cardano.nufi.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.nufi is not found"); });
    case "gerowallet":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.gerowallet !== 'undefined')) {
        return window.cardano.gerowallet.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.gerowallet is not found"); });
    case "begin":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.begin !== 'undefined')) {
        return window.cardano.begin.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.begin is not found"); });
    case "begin-nightly":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano["begin-nightly"] !== 'undefined')) {
        return window.cardano["begin-nightly"].enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.begin-nightly is not found"); });
    case "typhon":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.typhon !== 'undefined')) {
        return window.cardano.typhon.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.typhon is not found"); });
    case "lace":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.lace !== 'undefined')) {
        return window.cardano.lace.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.lace is not found"); });
    case "yoroi":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.yoroi !== 'undefined')) {
        return window.cardano.yoroi.enable();
      }
      else
        return new Promise(() => { throw new Error("window.cardano or window.cardano.yoroi is not found"); });
    default:
      return new Promise(() => { setWalletNone(); console.log(walletName); console.log("Wallet: None"); });
  }
}

async function walletLoad(walletName) {
  console.log("begin walletLoad");
  await loader.load();
  const CardanoWasm = loader.Cardano;
  try {
    const api = await walletAPI(walletName);

    setInputValue("walletNameElement", walletName);
    setInputValue("daoWalletNameNotConnected", walletName);

    const networkId = await api.getNetworkId();
    setInputValue("networkIdElement", networkId);

    // const balance             = await api.getBalance();
    // setInputValue(balanceElement, balance);

    const changeAddress = CardanoWasm.Address.from_bytes(fromHexString(await api.getChangeAddress()));
    const changeAddressBech32 = changeAddress.to_bech32();
    const baseAddress = CardanoWasm.BaseAddress.from_address(changeAddress);
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
    const pubKeyHashHex = toHexString(pubKeyHash.to_bytes());
    const stakeKeyHashHex = toHexString(stakeKeyHash.to_bytes());
    pubKeyHash.free();
    stakeKeyHash.free();
    setInputValue("changeAddressBech32Element", changeAddressBech32);
    setInputValue("pubKeyHashElement", pubKeyHashHex);
    setInputValue("stakeKeyHashElement", stakeKeyHashHex);

    // const collateral          = await api.experimental.getCollateral();
    // setInputValue(collateralElement, collateral);

    const utxos = await api.getUtxos();
    const utxosJSON = [];
    for (i = 0; i < utxos.length; i++) {
      const utxo = CardanoWasm.TransactionUnspentOutput.from_bytes(fromHexString(utxos[i]));
      utxosJSON.push(utxo.to_json());
      utxo.free();
    }
    setInputValue("utxosElement", "[" + utxosJSON.join(', ') + "]");

    // const unusedAddresses     = await api.getUnusedAddresses();
    // setInputValue(unusedAddressesElement, unusedAddresses);

    // const rewardAddresses     = await api.getRewardAddresses();
    // setInputValue(rewardAddressesElement, rewardAddresses);
    setInputValue("EndWalletLoad", "");
    console.log("end walletLoad");
  } catch (e) {
    console.log(e.message);
    setWalletNone();
    if (e.message.includes("no account set")) { setInputValue("walletErrorElement", "No account set in Eternl wallet."); }
    else { setInputValue("walletErrorElement", "No access to the wallet."); }
    return;
  }
}

async function walletSignTx(walletName, partialTxHex) {
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
    for (i = 0; i < walletSignatures.len(); i++) {
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

async function walletSubmitTx(walletName, txHex) {
  const api = await walletAPI(walletName);
  return api.signTx(txHex);
}

// Convert a hex string to a byte array
function fromHexString(hex) {
  for (var bytes = [], c = 0; c < hex.length; c += 2)
    bytes.push(parseInt(hex.substr(c, 2), 16));
  return bytes;
}

// Convert a byte array to a hex string
function toHexString(byteArray) {
  return Array.from(byteArray, function (byte) {
    return ('0' + (byte & 0xFF).toString(16)).slice(-2);
  }).join('')
}

// takes a hex string 'str' and puts the hash as a hex string into an element 'resId'
async function sha2_256(str, resId) {
  const hashBuffer = await crypto.subtle.digest('SHA-256', new Uint8Array(fromHexString(str)));
  const hashArray = Array.from(new Uint8Array(hashBuffer));
  setInputValue(resId, toHexString(hashArray));
}

async function ed25519Sign(prvKey, msg, resId) {
  const sig = await window.nobleEd25519.sign(msg, prvKey);
  setInputValue(resId, toHexString(sig));
}

function setElementStyle(elId, prop, val) {
  var el = document.getElementById(elId);
  if (el != null) {
    el.style.setProperty(prop, val);
  };
};

function pingServer(baseUrl) {
  const request = new XMLHttpRequest();
  request.open('GET', baseUrl + 'ping', false);  // `false` makes the request synchronous
  try {
    request.send(null);
  } catch (e) {
    console.log('ping ' + baseUrl + ' ====> ' + 'failed to send request');
    return false;
  }
  console.log('ping ' + baseUrl + ' ====> ' + request.status);
  return (request.status === 200);
};

function saveHashedTextToStorage(key, hash) {
  localStorage.setItem(key, hash);
}

function loadCacheValue(key) {
  const pass = localStorage.getItem(key);
  if (pass == null) {
    return ""
  } else {
    return pass
  }
}

async function addrLoad(addrInput) {
  await loader.load();
  const CardanoWasm = loader.Cardano;
  try {
    //Start Warm up CardanoWasm
    // TODO: Remove it after moving to cardanp-adresses
    const address = "addr_test1qr8cdsle3chjufssrg9wujvseypyj8fgxj2xg005cclyk0wu66jsm534qu9p759fexv8h2lpsdja54yrzgmzv4z83wmstcscqv";
    CardanoWasm.Address.from_bech32(address);
    //End Warm up CardanoWasm

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
    setInputValue("addrPubKeyHashElement", pubKeyHashHex);
    setInputValue("addrStakeKeyHashElement", stakeKeyHashHex);
    pubKeyHash.free();
    stakeKeyHash.free();
  } catch (e) {
    console.log(e);
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
  for (var i = 0; i < str.length; i++) {
    var charcode = str.charCodeAt(i);
    if (charcode < 0x80) utf8.push(charcode);
    else if (charcode < 0x800) {
      utf8.push(0xc0 | (charcode >> 6),
        0x80 | (charcode & 0x3f));
    }
    else if (charcode < 0xd800 || charcode >= 0xe000) {
      utf8.push(0xe0 | (charcode >> 12),
        0x80 | ((charcode >> 6) & 0x3f),
        0x80 | (charcode & 0x3f));
    }
    // surrogate pair
    else {
      i++;
      // UTF-16 encodes 0x10000-0x10FFFF by
      // subtracting 0x10000 and splitting the
      // 20 bits of 0x0-0xFFFFF into two halves
      charcode = 0x10000 + (((charcode & 0x3ff) << 10)
        | (str.charCodeAt(i) & 0x3ff));
      utf8.push(0xf0 | (charcode >> 18),
        0x80 | ((charcode >> 12) & 0x3f),
        0x80 | ((charcode >> 6) & 0x3f),
        0x80 | (charcode & 0x3f));
    }
  }
  return utf8;
}

async function daoPollVoteTx(n, apiKey, net, walletName, answer, policyId, assetName) {
  // loading CardanoWasm
  await loader.load();
  const CardanoWasm = loader.Cardano;
  const blockfrostAddress = ["https://cardano-", net.toLowerCase(), ".blockfrost.io/api/v0"].join('');
  await lucidLoader.load();
  const lucid = await lucidLoader.Lucid.new(
    new lucidLoader.Blockfrost(blockfrostAddress, apiKey),
    net,
  );

  try {
    //loading wallet
    const api = await walletAPI(walletName);
    lucid.selectWallet(api);

    const changeAddress = CardanoWasm.Address.from_bytes(fromHexString(await api.getChangeAddress()));
    const baseAddress = CardanoWasm.BaseAddress.from_address(changeAddress);
    const stakeKeyHashCred = baseAddress.stake_cred();
    const stakeKeyHash = stakeKeyHashCred.to_keyhash();
    const utxos = await api.getUtxos();

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

    setInputValue("VoteCreateNewTx", "");

    const tx = await lucid.newTx()
      .addSignerKey(toHexString(stakeKeyHash.to_bytes()))
      .payToAddressWithData(changeAddress.to_bech32(),
        { inline: toHexString(plc_msg.to_bytes()) },
        { lovelace: 1500000n, [policyId + assetName]: 1n }
      )
      .complete();

    setInputValue("VoteSignTx", tx);

    const signedTx = await tx.sign().complete();

    setInputValue("VoteSubmitTx", signedTx);

    const txHash = await signedTx.submit();

    console.log(txHash);
    setInputValue("VoteSubmittedTx", signedTx);

    setInputValue("elementPoll" + n, "Thank you for voting! Come back later to see the results.");

    // Check wallets's utxos are changed and then send VoteReadyTx
    await check_utxos_changed("VoteReadyTx", api, utxos, { wait: 1000, retries: 20 })

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
    console.log("Error: " + e.info);
    setInputValue("VoteError", e.info);
    return;
  }
};

async function daoDelegateTx(apiKey, net, walletName, url, policyId, assetName) {
  // loading CardanoWasm
  await loader.load();
  const CardanoWasm = loader.Cardano;

  const blockfrostAddress = ["https://cardano-", net.toLowerCase(), ".blockfrost.io/api/v0"].join('');
  await lucidLoader.load();
  const lucid = await lucidLoader.Lucid.new(
    // TODO: check url below
    new lucidLoader.Blockfrost(blockfrostAddress, apiKey),
    net,
  );

  try {
    //loading wallet
    const api = await walletAPI(walletName);
    lucid.selectWallet(api);


    const changeAddress = CardanoWasm.Address.from_bytes(fromHexString(await api.getChangeAddress()));
    const baseAddress = CardanoWasm.BaseAddress.from_address(changeAddress);
    const stakeKeyHashCred = baseAddress.stake_cred();
    const stakeKeyHash = stakeKeyHashCred.to_keyhash();
    const utxos = await api.getUtxos();

    const plc_lst = CardanoWasm.PlutusList.new();
    const tag1 = CardanoWasm.PlutusData.new_bytes(toUTF8Array("ENCOINS"));
    const tag2 = CardanoWasm.PlutusData.new_bytes(toUTF8Array("Delegate"));
    const tag3 = CardanoWasm.PlutusData.new_bytes(stakeKeyHash.to_bytes());
    const tag4 = CardanoWasm.PlutusData.new_bytes(toUTF8Array(url));
    plc_lst.add(tag1);
    plc_lst.add(tag2);
    plc_lst.add(tag3);
    plc_lst.add(tag4);
    const plc_msg = CardanoWasm.PlutusData.new_list(plc_lst);

    setInputValue("DelegateCreateNewTx", "");

    const tx = await lucid.newTx()
      .addSignerKey(toHexString(stakeKeyHash.to_bytes()))
      .payToAddressWithData(changeAddress.to_bech32(),
        { inline: toHexString(plc_msg.to_bytes()) },
        { lovelace: 1500000n, [policyId + assetName]: 1n }
      )
      .complete();

    setInputValue("DelegateSignTx", tx)

    const signedTx = await tx.sign().complete();

    setInputValue("DelegateSubmitTx", signedTx);

    const txHash = await signedTx.submit();

    setInputValue("DelegateSubmittedTx", txHash);

    // Check wallets's utxos are changed and then send DelegateSuccessTx
    await check_utxos_changed("DelegateSuccessTx", api, utxos, { wait: 1000, retries: 20 })

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
    setInputValue("DelegateReadyTx", "");
  } catch (e) {
    handle_dao_error(e);
    return;
  }
};

async function check_utxos_changed(elementId, api, utxosOld, { wait, retries }) {
  await setTimeout(wait)
  const utxosNew = await api.getUtxos();

  if (utxosOld !== utxosNew) {
    console.log("The utxos of the wallet have been changed")
    return setInputValue(elementId, "");
  }

  if (retries)
    return check_utxos_changed(elementId, api, utxosOld, { wait, retries: --retries })

  throw new Error('Retry attempts exhausted')
}

function handle_dao_error(e) {
  switch (e) {
    case "InputsExhaustedError":
      setInputValue("DelegateError", "Not enough ADA");
      console.log("Error in daoDelegateTx: " + e);
      break;
    default:
      if (e.info === undefined) {
        setInputValue("DelegateError", e);
        console.log("Raw error in daoDelegateTx: " + e);
      } else
        setInputValue("DelegateError", e.info);
      console.log("Error in daoDelegateTx: " + e.info);
      break;
  }
}

// Cloud

const CloudCrypto = {

  // Function to generate a random AES key
  generateCloudKey: async function (resId) {
    const key = await crypto.subtle.generateKey(
      {
        name: "AES-GCM",
        length: 256
      },
      true,
      ["encrypt", "decrypt"]
    );
    const exportedKeyHex = await this.exportAESKey(key);
    setInputValue(resId, exportedKeyHex);
  },

  // Function to encrypt plaintext using AES-GCM
  encryptSecret: async function (hexKey, resId, plaintext) {
    const key = await this.importAESKey(hexKey);
    const iv = crypto.getRandomValues(new Uint8Array(16)); // Generate a random IV
    const encrypted = await crypto.subtle.encrypt(
      {
        name: "AES-GCM",
        iv: iv
      },
      key,
      new TextEncoder().encode(plaintext)
    );
    const encryptedData = { iv: iv, ciphertext: new Uint8Array(encrypted) };
    const encryptedDataHex = this.encryptedObjectToHexString(encryptedData);
    setInputValue(resId, encryptedDataHex)
  },

  // Function to decrypt ciphertext using AES-GCM
  decryptSecret: async function (hexKey, resId, encryptedDataHex) {
    const key = await this.importAESKey(hexKey);
    const decryptedData = this.hexStringToEncryptedObject(encryptedDataHex);
    const decryptedText = new TextDecoder().decode(
      await crypto.subtle.decrypt(
        {
          name: "AES-GCM",
          iv: decryptedData.iv
        },
        key,
        decryptedData.ciphertext
      )
    );
    setInputValue(resId, decryptedText)
  },

  decryptSecretList: async function (hexKey, resId, encryptedDataHexList) {
    const key = await this.importAESKey(hexKey);
    const decryptedTextList = [];

    for (const encryptedDataHex of encryptedDataHexList) {
      try {

        const decryptedData = this.hexStringToEncryptedObject(encryptedDataHex[1]);
        const decryptedText = new TextDecoder().decode(
          await crypto.subtle.decrypt(
            {
              name: "AES-GCM",
              iv: decryptedData.iv
            },
            key,
            decryptedData.ciphertext
          )
        );

        decryptedTextList.push([encryptedDataHex[0], decryptedText]);
      } catch (e) {
        console.log("Token was encrypted with another key.", e)
      }

    }
    const decryptedJSON = JSON.stringify(decryptedTextList);
    setInputValue(resId, decryptedJSON)
  },

  // Function to convert encryption result to hex string
  encryptedObjectToHexString: function (result) {
    const ivHex = Array.from(result.iv)
      .map((byte) => byte.toString(16).padStart(2, "0"))
      .join("");
    const ciphertextHex = Array.from(result.ciphertext)
      .map((byte) => byte.toString(16).padStart(2, "0"))
      .join("");
    return ivHex + ciphertextHex;
  },

  // Function to convert hex string to encryption result object
  hexStringToEncryptedObject: function (hexString) {
    const ivHex = hexString.slice(0, 32);
    const ciphertextHex = hexString.slice(32);
    const iv = new Uint8Array(ivHex.match(/.{1,2}/g).map((byte) => parseInt(byte, 16)));
    const ciphertext = new Uint8Array(ciphertextHex.match(/.{1,2}/g).map((byte) => parseInt(byte, 16)));
    return { iv: iv, ciphertext: ciphertext };
  },

  importAESKey: async function (hexKey) {
    const keyBytes = new Uint8Array(hexKey.match(/.{1,2}/g).map(byte => parseInt(byte, 16)));
    return crypto.subtle.importKey("raw", keyBytes, { name: "AES-GCM" }, true, ["encrypt", "decrypt"]);
  },

  // Function to export a CryptoKey to a hex string
  exportAESKey: async function (cryptoKey) {
    const keyExported = await crypto.subtle.exportKey("raw", cryptoKey);
    const keyBytes = new Uint8Array(keyExported);
    return Array.from(keyBytes)
      .map((byte) => byte.toString(16).padStart(2, '0'))
      .join('');
  }
}


// CryptoNative methods for replacing cryptoJS library and
// using to (en/de)crypt values in local storage
const CryptoNative = {

  getMessageEncoding: function (message) {
    let enc = new TextEncoder();
    return enc.encode(message);
  },

  getKeyMaterial: function (password) {
    let enc = new TextEncoder();
    return crypto.subtle.importKey(
      "raw",
      enc.encode(password),
      { name: "PBKDF2" },
      false,
      ["deriveBits", "deriveKey"]
    );
  },

  getKey: function (keyMaterial, salt) {
    const key = crypto.subtle.deriveKey(
      {
        name: "PBKDF2",
        salt: salt,
        iterations: 100000,
        hash: "SHA-256"
      },
      keyMaterial,
      { "name": "AES-GCM", "length": 256 },
      true,
      ["encrypt", "decrypt"]
    );
    return key
  },

  encryptAesBase64: async function (password, message) {
    const keyMaterial = await this.getKeyMaterial(password);
    const salt = crypto.getRandomValues(new Uint8Array(16));
    const key = await this.getKey(keyMaterial, salt);
    const iv = crypto.getRandomValues(new Uint8Array(12));
    const encoded = this.getMessageEncoding(message);

    const ciphertext = await crypto.subtle.encrypt(
      {
        name: "AES-GCM",
        iv: iv
      },
      key,
      encoded
    );
    const ciphertextBase64 = this.arrayBufferToBase64(salt) + this.arrayBufferToBase64(iv) + this.arrayBufferToBase64(ciphertext);
    return ciphertextBase64
  },

  decryptBase64Aes: async function (password, ciphertextBase64) {
    const salt = this.base64ToArrayBuffer(ciphertextBase64.slice(0, 24));
    const iv = this.base64ToArrayBuffer(ciphertextBase64.slice(24, 40));
    const ciphertext = this.base64ToArrayBuffer(ciphertextBase64.slice(40));

    const keyMaterial = await this.getKeyMaterial(password);
    const key = await this.getKey(keyMaterial, salt);
    const decrypted = await crypto.subtle.decrypt(
      {
        name: "AES-GCM",
        iv: iv
      },
      key,
      ciphertext
    );
    const dec = new TextDecoder();
    const res = dec.decode(decrypted);
    return res
  },

  arrayBufferToBase64: function (buffer) {
    const bytes = new Uint8Array(buffer);
    let binary = '';
    for (let i = 0; i < bytes.length; i++) {
      binary += String.fromCharCode(bytes[i]);
    }
    return btoa(binary);
  },

  base64ToArrayBuffer: function (base64) {
    const binaryString = atob(base64);
    let bytes = new Uint8Array(binaryString.length);
    for (let i = 0; i < binaryString.length; i++) {
      bytes[i] = binaryString.charCodeAt(i);
    }
    return bytes.buffer;
  }
}

// CryptoFallback used to migrate from cryptoJS library
const CryptoFallback = {

  decryptAesBase64Fallback: async function (password, message) {
    try {
      const firstRes = await CryptoNative.decryptBase64Aes(password, message);
      // console.log('firstRes');
      return firstRes
    } catch (e1) {
      console.log('Native decryption failed');
      // console.log('Native error:', e1);
      try {
        const secondRes = await ReplaceCryptoJS.decryptCryptoJSCipherBase64(password, message);
        // console.log('secondRes', secondRes);
        return secondRes
      } catch (e2) {
        console.log('fallback failed', e2);
      }
    }
  }
}

// ReplaceCryptoJS methods used to decrypt values from local storage that
// were encrypted with cryptoJS library
const ReplaceCryptoJS = {

  HEAD_SIZE_DWORD: 2,
  SALT_SIZE_DWORD: 2,

  decryptCryptoJSCipherBase64: async function (
    password,
    cryptoJSCipherBase64,
    { keySizeDWORD = 256 / 32, ivSizeDWORD = 128 / 32, iterations = 1 } = {},
  ) {
    const { salt, ciphertext } = this.parseCryptoJSCipherBase64(cryptoJSCipherBase64);

    const { key, iv } = await this.dangerouslyDeriveParameters(password, salt, keySizeDWORD, ivSizeDWORD, iterations);
    const plaintextArrayBuffer = await crypto.subtle.decrypt({ name: "AES-CBC", iv }, key, ciphertext);

    return new TextDecoder().decode(plaintextArrayBuffer);
  },

  parseCryptoJSCipherBase64: function (cryptoJSCipherBase64) {
    let salt;
    let ciphertext = this.base64ToUint8Array(cryptoJSCipherBase64);

    const [head, body] = this.splitUint8Array(ciphertext, this.HEAD_SIZE_DWORD * 4);

    // This effectively checks if the ciphertext starts with 'Salted__'.
    // Alternatively we could do `atob(cryptoJSCipherBase64.substr(0, 11)) === "Salted__"`.
    const headDataView = new DataView(head.buffer);
    if (headDataView.getInt32(0) === 0x53616c74 && headDataView.getInt32(4) === 0x65645f5f) {
      [salt, ciphertext] = this.splitUint8Array(body, this.SALT_SIZE_DWORD * 4);
    }

    return { ciphertext, salt };
  },

  dangerouslyDeriveParameters: async function (password, salt, keySizeDWORD, ivSizeDWORD, iterations) {
    const passwordUint8Array = new TextEncoder().encode(password);

    const keyPlusIV = this.dangerousEVPKDF(passwordUint8Array, salt, keySizeDWORD + ivSizeDWORD, iterations);
    const [rawKey, iv] = this.splitUint8Array(keyPlusIV, keySizeDWORD * 4);

    const key = await crypto.subtle.importKey("raw", rawKey, "AES-CBC", false, ["decrypt"]);

    return { key, iv };
  },

  dangerousEVPKDF: function (passwordUint8Array, saltUint8Array, keySizeDWORD, iterations) {
    let derivedKey = new Uint8Array();
    let block = new Uint8Array();

    while (derivedKey.byteLength < keySizeDWORD * 4) {
      block = md5.arrayBuffer(this.concatUint8Arrays(block, passwordUint8Array, saltUint8Array));

      for (let i = 1; i < iterations; i++) {
        block = md5.arrayBuffer(block);
      }

      block = new Uint8Array(block);

      derivedKey = this.concatUint8Arrays(derivedKey, block);
    }

    return derivedKey;
  },

  // utils

  concatUint8Arrays: function (...as) {
    const size = as.reduce((size, a) => size + a.length, 0);
    const c = new Uint8Array(size);
    let i = 0;
    for (const a of as) {
      c.set(a, i);
      i += a.length;
    }
    return c;
  },

  splitUint8Array: function (a, i) {
    return [a.subarray(0, i), a.subarray(i, a.length)];
  },

  base64ToUint8Array: base64String => Uint8Array.from(atob(base64String), c => c.charCodeAt(0))

}
