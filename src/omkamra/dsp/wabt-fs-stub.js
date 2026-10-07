// WABT's Node-only fs branch is unreachable in browser builds. Shadow CLJS
// still needs a module for the conditional CommonJS require to resolve.
module.exports = {};
