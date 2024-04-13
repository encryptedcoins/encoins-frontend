const HEAD_SIZE_DWORD = 2;
const SALT_SIZE_DWORD = 2;

async function decryptCryptoJSCipherBase64(
  password,
  cryptoJSCipherBase64,
  { keySizeDWORD = 256 / 32, ivSizeDWORD = 128 / 32, iterations = 1 } = {},
) {
  const { salt, ciphertext } = parseCryptoJSCipherBase64(cryptoJSCipherBase64);

  const { key, iv } = await dangerouslyDeriveParameters(password, salt, keySizeDWORD, ivSizeDWORD, iterations);
  const plaintextArrayBuffer = await crypto.subtle.decrypt({ name: "AES-CBC", iv }, key, ciphertext);

  return new TextDecoder().decode(plaintextArrayBuffer);
}

function parseCryptoJSCipherBase64(cryptoJSCipherBase64) {
  let salt;
  let ciphertext = base64ToUint8Array(cryptoJSCipherBase64);

  const [head, body] = splitUint8Array(ciphertext, HEAD_SIZE_DWORD * 4);

  // This effectively checks if the ciphertext starts with 'Salted__'.
  // Alternatively we could do `atob(cryptoJSCipherBase64.substr(0, 11)) === "Salted__"`.
  const headDataView = new DataView(head.buffer);
  if (headDataView.getInt32(0) === 0x53616c74 && headDataView.getInt32(4) === 0x65645f5f) {
    [salt, ciphertext] = splitUint8Array(body, SALT_SIZE_DWORD * 4);
  }

  return { ciphertext, salt };
}

async function dangerouslyDeriveParameters(password, salt, keySizeDWORD, ivSizeDWORD, iterations) {
  const passwordUint8Array = new TextEncoder().encode(password);

  const keyPlusIV = dangerousEVPKDF(passwordUint8Array, salt, keySizeDWORD + ivSizeDWORD, iterations);
  const [rawKey, iv] = splitUint8Array(keyPlusIV, keySizeDWORD * 4);

  const key = await crypto.subtle.importKey("raw", rawKey, "AES-CBC", false, ["decrypt"]);

  return { key, iv };
}

function dangerousEVPKDF(passwordUint8Array, saltUint8Array, keySizeDWORD, iterations) {
  let derivedKey = new Uint8Array();
  let block = new Uint8Array();

  while (derivedKey.byteLength < keySizeDWORD * 4) {
    block = md5.arrayBuffer(concatUint8Arrays(block, passwordUint8Array, saltUint8Array));

    for (let i = 1; i < iterations; i++) {
      block = md5.arrayBuffer(block);
    }

    block = new Uint8Array(block);

    derivedKey = concatUint8Arrays(derivedKey, block);
  }

  return derivedKey;
}
