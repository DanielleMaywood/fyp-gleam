const wasmCode = await Deno.readFile("./build/dev/webassembly/project_wasm/program.wasm")
// const wasmCode = await Deno.readFile("./program.wasm");
const wasmModule = new WebAssembly.Module(wasmCode);
const wasmInstance = new WebAssembly.Instance(wasmModule);

const toInt = wasmInstance.exports["gleam/Int$to_int"];
const toFloat = wasmInstance.exports["gleam/Float$to_float"];
const toBool = wasmInstance.exports["gleam/Bool$to_bool"];
const main = wasmInstance.exports["project_wasm/main"];

console.log(toInt(main()));
