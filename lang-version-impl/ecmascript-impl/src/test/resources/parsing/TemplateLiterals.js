const plain = `plain text`;
const empty = ``;
const hello = `hello ${name} and ${ {a: 1}.a } end`;
const nested = `outer ${ `inner ${name}` } done`;
const escapes = `price $5, \` tick, \${not} substitution`;
const multi = `first
second ${1 + 2} third`;
const ratio = `10` / 2;
