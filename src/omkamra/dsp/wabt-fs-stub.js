// WABT's Emscripten bundle reaches for two Node-only things inside its
// ENVIRONMENT_IS_NODE branch, which a browser never takes — it requires "fs" and
// it reads the `process` global — and Shadow resolves both at build time whether
// or not that branch ever runs. An empty object is the smallest honest answer,
// and one stub answers for both.
module.exports = {};
