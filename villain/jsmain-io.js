/*
  Bit layout of values

  Values are either:
  - Immediates: end in #b000
  - Pointers

  Immediates are either
  - Integers:   end in  #b00 000
  - Characters: end in #b010 000
  - True:              #b110 000
  - False:           #b1 110 000
  - Eof:            #b10 110 000
  - Void:           #b11 110 000
  - Empty:         #b100 110 000
*/

const imm_shift = 4;
const int_shift = (1 + imm_shift);
const nonint_type_tag = (1 << (int_shift - 1));
const char_shift = (int_shift + 1);
const nonchar_type_tag = ((1 << (char_shift - 1)) | nonint_type_tag);
const val_eof  = ((2 << char_shift) | nonchar_type_tag);

var output = "";
var unloaded_result = "";

function writeBytejs(c) {  
  output = output.concat('', String.fromCodePoint(c >> int_shift));
}

const fs = require('fs')

var peek_flag = 0
const peek_buffer = Buffer.alloc(1)

function readBytejs() {
  const buffer = Buffer.alloc(1)
  if (peek_flag === 0) {
    fs.readSync(process.stdin.fd, buffer, 0, 1)
  } else {
    buffer[0] = peek_buffer[0]
    peek_flag = 0
  }
  if (buffer[0] === 0) {
    return val_eof
  } else {
    return buffer[0] << int_shift
  }
}

function peekBytejs() {
  const buffer = Buffer.alloc(1)
  if (peek_flag === 0) {
    fs.readSync(process.stdin.fd, buffer, 0, 1)
    peek_buffer[0] = buffer[0]
    peek_flag = 1
  } else {
    buffer[0] = peek_buffer[0]
  }
  if (buffer[0] === 0) {
    return val_eof
  } else {
    return buffer[0] << int_shift
  }
}  

function errorjs() {
  const msg = "err"
  console.log(msg)
  console.log(output)
  console.log(unloaded_result)
  process.exit()
}

const { readFileSync } = require("fs");

const run = async () => {
  var importObject = {
    writeBytejs: {
      writeByte: function(arg) {
        writeBytejs(arg);
      }
    },
    readBytejs: {
      readByte: function() {
        return readBytejs();
      }
    },
    peekBytejs: {
      peekByte: function() {
        return peekBytejs();
      }
    },
    errorjs: {
      error: function() {
        errorjs();
      }
    }
  };
  const buffer = readFileSync(process.argv[2]);
  const module = await WebAssembly.compile(buffer);
  const instance = await WebAssembly.instantiate(module, importObject);
  result = instance.exports.sendResult();
  console.log(result);
  console.log(output);
  console.log(unloaded_result);
};

run();
