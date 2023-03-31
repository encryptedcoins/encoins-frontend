window.fingerprintFromAssetName = function fingerprintFromAssetName(currencySymbol, tokenName)
{
  const cip14 = require('@emurgo/cip14-js');
  const AssetFingerprint = cip14.default;
  const assetFingerprint = AssetFingerprint.fromParts(
    Buffer.from(currencySymbol, 'hex'),
    Buffer.from(tokenName, 'hex'),
  );
  return assetFingerprint.fingerprint();
};
