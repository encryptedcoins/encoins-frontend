//////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// WebPage functions ////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

function runHeadScripts() {
  // Loading modules
  loader.load();

  // Webflow
  WebFont.load(
    {google:{families:
      ['Droid Serif:400,400italic,700,700italic',
      'Corben:regular','Fenix:regular']}});
  !function(o,c) {
    var n=c.documentElement, t=' w-mod-';
    n.className+=t+'js';
    if ('ontouchstart'in o||o.DocumentTouch && c instanceof DocumentTouch) {
      n.className+=t+'touch';
    }
  }(window,document);
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

function walletAPI(walletName) {
  switch(walletName) {
    case "nami":
      if ((typeof window.cardano !== 'undefined') || (typeof window.cardano.nami !== 'undefined'))
        return window.cardano.nami.enable();
      break;
    default:
  }
}

function walletEnable(walletName, resId) {
  const w = walletAPI(walletName);
  if (typeof w !== 'undefined')
    w.then(() => { setInputValue(resId, "true"); }, () => { setInputValue(resId, "error_walletAPI"); });
  else setInputValue(resId, "error_walletAPI");
};

function walletAddress(walletName, resId) {
  const w = walletAPI(walletName);
  if (typeof w !== 'undefined')
    w.then((api) => { api.getChangeAddress().then((res) => { setInputValue(resId, res); }); },
      () => { setInputValue(resId, "error_walletAPI"); });
    else setInputValue(resId, "error_walletAPI");
};

function walletAddressBech32(walletName, resId) {
  loader.load().then(() => {
    const CardanoWasm = loader.Cardano;
    const w = walletAPI(walletName);
    if (typeof w !== 'undefined')
      w.then((api) => {
        api.getChangeAddress().then((res) => {
          const address = CardanoWasm.Address.from_bytes(fromHexString(res)).to_bech32();
          setInputValue(resId, address);
        }, () => { setInputValue(resId, "error_walletAPI"); });
      });
      else setInputValue(resId, "error_walletAPI");
  });
};

function walletAddressBech32ToBytes(addressBech32, resId) {
  loader.load().then(() => {
    const CardanoWasm = loader.Cardano;
    const address = toHexString(CardanoWasm.Address.from_bech32(addressBech32).to_bytes());
    setInputValue(resId, address);
  });
};

function encoinsTx(walletName, mp, resId) {
  const w = walletAPI(walletName);
  if (typeof w !== 'undefined')
    w.then((api) => {
      loader.load().then(() => {
        const CardanoWasm = loader.Cardano;

        // instantiate the tx builder with the Cardano protocol parameters
        const linearFee = CardanoWasm.LinearFee.new(CardanoWasm.BigNum.from_str('44'), CardanoWasm.BigNum.from_str('155381'));
        const txBuilderCfg = CardanoWasm.TransactionBuilderConfigBuilder.new()
          .fee_algo(linearFee)
          .pool_deposit(CardanoWasm.BigNum.from_str('500000000'))
          .key_deposit(CardanoWasm.BigNum.from_str('2000000'))
          .max_value_size(5000)
          .max_tx_size(16384)
          .coins_per_utxo_word(CardanoWasm.BigNum.from_str('34482'))
          .build();
        const txBuilder = CardanoWasm.TransactionBuilder.new(txBuilderCfg);

        // creating ADA stake utxo
        const stakeAddressBech32 = "addr1q8297zyzq4kaafaypq599x66vxznytjyfs39gx9endwyksf9x5zdat37w6pt3lzvqkumrpdkyjf8faxek2xkjd59n0csdrsv8e";
        const stakeAddress = CardanoWasm.Address.from_bech32(stakeAddressBech32);
        const stakeValue = CardanoWasm.Value.new(CardanoWasm.BigNum.from_str(mp[1]));
        const stakeUTXO = CardanoWasm.TransactionOutput.new(stakeAddress, stakeValue);
        txBuilder.add_output(stakeUTXO);

        console.log(stakeUTXO);

        // defining minted coins
        const ma = CardanoWasm.MultiAsset.new();
        for (i=0; i < mp[0].length; i++)
        {
          const a = CardanoWasm.Assets.new();
          const encoinsCurSymb = "";
          a.insert(CardanoWasm.AssetName.new(fromHexString(mp[0][i][0])), CardanoWasm.BigNum.from_str('1'));
          ma.insert(CardanoWasm.ScriptHash.from_bytes(fromHexString(encoinsCurSymb)), a);
        }
        valueToPay.set_multiasset(ma);

        // add inputs from the user's wallet
        valueToSpend = valueToPay.checked_add(CardanoWasm.Value.new(CardanoWasm.BigNum.from_str('10000000'))); // adding fee estimate
        api.getUtxos(toHexString(valueToSpend.to_bytes()), undefined).
          then((res) => {
            if (res === null)
            { setInputValue(resId, "error_utxos"); return; }

            for (i=0; i < res.length; i++)
            {
              const utxo = CardanoWasm.TransactionUnspentOutput.from_bytes(fromHexString(res[i]));
              const utxoInput = utxo.input();
              const utxoOut = utxo.output();
              txBuilder.add_input(
                utxoOut.address(),
                utxoInput,
                utxoOut.amount()
              );      
            };
              
            // creating utxo datum
            const datum = CardanoWasm.PlutusData.new_integer(CardanoWasm.BigInt.from_str(dp.dpKey[0])); // here goes the user-generated key

            // add output to the tx
            const scriptAddress = CardanoWasm.Address.from_bech32(dp.dpAddress);
            scriptOutput = CardanoWasm.TransactionOutput.new(scriptAddress, valueToPay);
            scriptOutput.set_data_hash(CardanoWasm.hash_plutus_data(datum));
            txBuilder.add_output(scriptOutput);
              
            // calculate the min fee required and send any change to an address
            api.getChangeAddress().then((res) => {
              const changeAddress = CardanoWasm.Address.from_bytes(fromHexString(res));
              txBuilder.add_change_if_needed(changeAddress);

              // once the transaction is ready, we build it to get the tx body without witnesses
              const txBody = txBuilder.build();
              const txHash = CardanoWasm.hash_transaction(txBody);
              const witnesses = CardanoWasm.TransactionWitnessSet.new();

              // create the finalized transaction with witnesses
              const partialTx = CardanoWasm.Transaction.new(
                txBody,
                witnesses,
                undefined, // transaction metadata
              );
              const partialTx_hex = toHexString(partialTx.to_bytes());

              api.signTx(partialTx_hex).then((res) => {
                const txVkeyWitnesses = CardanoWasm.TransactionWitnessSet.from_bytes(fromHexString(res));
                const readyToSubmit = CardanoWasm.Transaction.new(txBody, txVkeyWitnesses);
                const finalTx = toHexString(readyToSubmit.to_bytes());
                api.submitTx(finalTx).then((res) => {
                  setInputValue(resId, dp.dpKey[1]);
                }, () => { setInputValue(resId, "error_submit"); });
              }, () => { setInputValue(resId, "error_sign"); });
            });
        });
      });
    }, () => { setInputValue(resId, "error_walletAPI"); });
  else setInputValue(resId, "error_walletAPI");
};

// Convert a hex string to a byte array
function fromHexString(hex) {
  for (var bytes = [], c = 0; c < hex.length; c += 2)
      bytes.push(parseInt(hex.substr(c, 2), 16));
  return bytes;
}

function toHexString(byteArray) {
  return Array.from(byteArray, function(byte) {
    return ('0' + (byte & 0xFF).toString(16)).slice(-2);
  }).join('')
}