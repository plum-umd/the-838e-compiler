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
const box_type_tag    = 1;
const cons_type_tag   = 2;
const str_type_tag    = 3;
const symbol_type_tag = 4;
const port_type_tag   = 5;
const vector_type_tag = 6;
const flonum_type_tag = 7; 
const prefab_type_tag = 8;
const proc_type_tag   = 9;
const bignum_type_tag = 10;

const ptr_type_mask = ((1 << imm_shift) - 1);
const ptr_addr_mask =  ~ptr_type_mask;
const int_shift = (1 + imm_shift);
const int_type_mask = ((1 << int_shift) - 1);
const int_type_tag = (0 << (int_shift - 1));
const nonint_type_tag = (1 << (int_shift - 1));
const char_shift = (int_shift + 1);
const char_type_mask = ((1 << char_shift) - 1);
const char_type_tag = ((0 << (char_shift - 1)) | nonint_type_tag);
const nonchar_type_tag = ((1 << (char_shift - 1)) | nonint_type_tag);
const val_true = ((0 << char_shift) | nonchar_type_tag);
const val_false = ((1 << char_shift) | nonchar_type_tag);
const val_eof  = ((2 << char_shift) | nonchar_type_tag);
const val_void = ((3 << char_shift) | nonchar_type_tag);
const val_empty = ((4 << char_shift) | nonchar_type_tag);
const void_answer = -1;

function ans_str(result) {
  var ret_answer;
  if (cons_type_tag === (ptr_type_mask & result)) {
    ret_answer = "(".concat(' ', ")");
  } else if (box_type_tag === (ptr_type_mask & result)) {
    ret_answer = "#&".concat(' ', "");
  } else if (int_type_tag === (int_type_mask & result)) {
    ret_answer = (result >> int_shift);
  } else if (char_type_tag === (char_type_mask & result)) {
    ret_answer = "#\\".concat('', String.fromCodePoint(result >> char_shift));
  } else {
    switch (result) {
    case val_true:
      ret_answer = "#t"; break;
    case val_false:
      ret_answer = "#f"; break;
    case val_eof:
      ret_answer = "#<eof>"; break;
    case val_empty:
      ret_answer = "()"; break;
    case val_void:
      /* nothing */ break;
    default:
    }
  }
  return ret_answer;  
}

const fs = require('fs')

function writeBytejs(c) {
  fs.writeSync(process.stdout.fd, String.fromCodePoint(c >> int_shift));
}

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
  var result = instance.exports.sendResult();
  if (result != val_void) {
    console.log(ans_str(result));
  }
};

run();
