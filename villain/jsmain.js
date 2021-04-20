const { readFileSync } = require("fs");

const run = async () => {
  const buffer = readFileSync(process.argv[2]);
  const module = await WebAssembly.compile(buffer);
  const instance = await WebAssembly.instantiate(module);
  console.log(instance.exports.sendResult());
  //console.log(String.fromCodePoint(instance.exports.sendResult()));
};

run();
