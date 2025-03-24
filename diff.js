const fs = require('fs');

function parseFile(filename) {
    const content = fs.readFileSync(filename, 'utf8');
    const lines = content.split('\n');
    const opcodes = new Map();

    for (let line of lines) {
        const match = line.match(/^Opcode (\d+): ([A-F0-9]+)/);
        if (match) {
            opcodes.set(parseInt(match[1], 10), match[2]);
        }
    }
    return opcodes;
}

function compareOpcodes(file1, file2) {
    const opcodes1 = parseFile(file1);
    const opcodes2 = parseFile(file2);

    const allIndices = new Set([...opcodes1.keys(), ...opcodes2.keys()]);
    for (let index of allIndices) {
        if (opcodes1.get(index) !== opcodes2.get(index)) {
            console.log(`Difference at Opcode ${index}:`);
            console.log(`  File 1: ${opcodes1.get(index) || 'N/A'}`);
            console.log(`  File 2: ${opcodes2.get(index) || 'N/A'}`);
        }
    }
}

const file1 = process.argv[2];
const file2 = process.argv[3];

if (!file1 || !file2) {
    console.log('Usage: node compare.js <file1> <file2>');
    process.exit(1);
}

compareOpcodes(file1, file2);
