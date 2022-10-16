class Loader {
  async load() {
    if (this._wasm) return;

    /**
     * @private
     */
    this._wasm = await import(
      './CSL/cardano_serialization_lib.js'
    );
  }

  get Cardano() {
    return this._wasm;
  }
}

const loader = new Loader();
