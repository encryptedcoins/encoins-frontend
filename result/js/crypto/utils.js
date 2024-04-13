function concatUint8Arrays(...as) {
  const size = as.reduce((size, a) => size + a.length, 0);
  const c = new Uint8Array(size);
  let i = 0;
  for (const a of as) {
    c.set(a, i);
    i += a.length;
  }
  return c;
}

function splitUint8Array(a, i) {
  return [a.subarray(0, i), a.subarray(i, a.length)];
}

const base64ToUint8Array = base64String => Uint8Array.from(atob(base64String), c => c.charCodeAt(0));

const typedArrayToBase64 = typedArray => btoa(String.fromCharCode(...new Uint8Array(typedArray.buffer)));

const flipI32 = i32 => {
  return (
    // move byte 3 to byte 0
    ((i32 >> 24) & 0x000000ff) |
    // move byte 2 to byte 1
    ((i32 >> 8) & 0x0000ff00) |
    // move byte 1 to byte 2
    ((i32 << 8) & 0x00ff0000) |
    // move byte 0 to byte 3
    ((i32 << 24) & 0xff000000)
  );
};

const wordArrayToUint8Array = wordArray =>
  new Uint8Array(new Int32Array(wordArray.words.map(flipI32)).buffer).slice(0, wordArray.sigBytes);

const typedArrayToWordArray = typedArray => ({
  words: Array.from(new Int32Array(typedArray.buffer).map(flipI32)),
  sigBytes: typedArray.byteLength,
});
