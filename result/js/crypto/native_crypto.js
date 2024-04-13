function getMessageEncoding(message) {
  let enc = new TextEncoder();
  return enc.encode(message);
}

function getKeyMaterial(password) {
  let enc = new TextEncoder();
  return crypto.subtle.importKey(
    "raw",
    enc.encode(password),
    { name: "PBKDF2" },
    false,
    ["deriveBits", "deriveKey"]
  );
}

function getKey(keyMaterial, salt) {
  const key = crypto.subtle.deriveKey(
    {
      "name": "PBKDF2",
      salt: salt,
      "iterations": 100000,
      "hash": "SHA-256"
    },
    keyMaterial,
    { "name": "AES-GCM", "length": 256 },
    true,
    ["encrypt", "decrypt"]
  );
  return key
}

async function encryptAesBase64(password, message) {
  let keyMaterial = await getKeyMaterial(password);
  salt = crypto.getRandomValues(new Uint8Array(16));
  let key = await getKey(keyMaterial, salt);
  iv = crypto.getRandomValues(new Uint8Array(12));
  let encoded = getMessageEncoding(message);

  ciphertext = await crypto.subtle.encrypt(
    {
      name: "AES-GCM",
      iv: iv
    },
    key,
    encoded
  );
  return arrayBufferToBase64(ciphertext)
}

async function decryptBase64Aes(password, ciphertext) {
  let keyMaterial = await getKeyMaterial(password);
  let key = await getKey(keyMaterial, salt);
  const decrypted = await crypto.subtle.decrypt(
      {
        name: "AES-GCM",
        iv: iv
      },
      key,
      base64ToArrayBuffer(ciphertext)
  );
  const dec = new TextDecoder();
  const res = dec.decode(decrypted);
  return res
}

function arrayBufferToBase64(buffer) {
  const bytes = new Uint8Array(buffer);
  let binary = '';
  for (let i = 0; i < bytes.length; i++) {
    binary += String.fromCharCode(bytes[i]);
  }
  return btoa(binary);
}

function base64ToArrayBuffer(base64) {
  const binaryString = atob(base64);
  const bytes = new Uint8Array(binaryString.length);
  for (let i = 0; i < binaryString.length; i++) {
    bytes[i] = binaryString.charCodeAt(i);
  }
  return bytes.buffer;
}
