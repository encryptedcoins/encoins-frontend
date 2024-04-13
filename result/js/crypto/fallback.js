async function decryptAesBase64Fallback(password, message) {
  try {
    const firstRes = await decryptBase64Aes(password, message);
    // console.log('firstRes');
    return firstRes
  } catch (e1) {
    console.log('Native decryption failed');
    try {
      const secondRes = await decryptCryptoJSCipherBase64(password, message);
      // console.log('secondRes', secondRes);
      return secondRes
    } catch (e2) {
      console.log('fallback failed', e2);
    }
  }
}
