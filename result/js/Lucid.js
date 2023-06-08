class LucidLoader {
  async load() {
    if (this._wasm) return;

    /**
     * @private
     */
    this._wasm = await import(
      "https://unpkg.com/lucid-cardano@0.10.6/web/mod.js"
    );
  }

  get Lucid() {
    return this._wasm.Lucid;
  }

  get Blockfrost() {
    return this._wasm.Blockfrost;
  }
}

const lucidLoader = new LucidLoader();
