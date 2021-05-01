/*
  Bit layout of values

  Values are either:
  - Immediates: end in #b000
  - Pointers

  Immediates are either
  - Integers:   end in  #b000
  - Characters: end in #b0100
  - True:              #b1100
  - False:           #b1 1100
  - Eof:            #b10 1100
  - Void:           #b11 1100
  - Empty:         #b100 1100
*/

const imm_shift = 2;
const word_size = 4;
const box_type_tag    = 0x00000001;
const cons_type_tag   = 0x00000002;
const str_type_tag    = 0x00000003;
const symbol_type_tag = 0x10000001;
const port_type_tag   = 0x10000002;
const vector_type_tag = 0x10000003;
const flonum_type_tag = 0x20000001; 
const prefab_type_tag = 0x20000002;
const proc_type_tag   = 0x20000003;
const bignum_type_tag = 0x30000001;

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

function ans_str(result, ui32array) {
  var str;
  if (cons_type_tag === (ptr_type_mask & result)) {
    if (process.argv[3] == "io") {
      str = cons_ans_str_io(result, ui32array)
    } else {
      str = "'(".concat(cons_ans_str(result, ui32array, 1))
    }    
  } else if (box_type_tag === (ptr_type_mask & result)) {    
    str = "#&".concat(ans_str(ui32array[(box_type_tag ^ result) / word_size], ui32array));
  } else if (int_type_tag === (int_type_mask & result)) {
    str = (result >> int_shift);
  } else if (char_type_tag === (char_type_mask & result)) {
    str = "#\\".concat(String.fromCodePoint(result >> char_shift));
  } else if (str_type_tag === (ptr_type_mask & result)) {
    str = "\"".concat(string_ans_str(result, ui32array), "\"");
  } else {
    switch (result) {
    case val_true:
      str = "#t"; break;
    case val_false:
      str = "#f"; break;
    case val_eof:
      str = "#<eof>"; break;
    case val_empty:
      if (process.argv[3] == "io") {
        str = "'()"; break;
      } else {
        str = "()"; break;
      } 
    case val_void:
      if (process.argv[3] == "io") {
        str = "(void)"; break;
      } else {
        /* nothing */ break;
      }
    default:
    }
  }
  return str;  
}

function cons_ans_str(result, ui32array, rp_flag) {
  var car = ui32array[((cons_type_tag ^ result) / word_size) + 1]
  var cdr = ui32array[(cons_type_tag ^ result) / word_size]
  var str
  var next_rp_flag = 0

  if ((car & ptr_type_mask) === cons_type_tag) {
    next_rp_flag = 1
    var carstr = "(".concat(cons_ans_str(car, ui32array, next_rp_flag))
  } else {
    var carstr = ans_str(car, ui32array).toString()
  }

  if (cdr === val_empty) {
    str = carstr
  } else if (cons_type_tag === (ptr_type_mask & cdr)) {
    str = carstr.concat(" ", cons_ans_str(cdr, ui32array, next_rp_flag))
  } else {
    str = carstr.concat(" . ", ans_str(cdr, ui32array))
  }

  if (rp_flag === 1) {
    return str.concat(")")
  } else {
    return str
  }
}

function cons_ans_str_io(result, ui32array) {
  var car = ui32array[((cons_type_tag ^ result) / word_size) + 1]
  var cdr = ui32array[(cons_type_tag ^ result) / word_size]
  var str
  var carstr = "(cons ".concat(ans_str(car, ui32array))

  if (cdr === val_empty) {
    str = carstr.concat(" '()")
  } else if ((ptr_type_mask & cdr) === cons_type_tag) {
    str = carstr.concat(" ", ans_str(cdr, ui32array))
  } else {
    str = carstr.concat(" ", ans_str(cdr, ui32array))
  }
  return str.concat(")")
}

function string_ans_str(result, ui32array) {
  let st_idx = (str_type_tag ^ result) / word_size
  let end_idx = st_idx + ans_str(ui32array[st_idx])
  var answer = ""
  for (var i = st_idx + 1; i <= end_idx; i++){
    answer = answer.concat(String.fromCodePoint(ui32array[i] >> char_shift))
  }
  return answer
}

const fs = require('fs')

if (process.argv[3] == "io") {
  var output = "";
}

function writeBytejs(c) {
  if (process.argv[3] == "io") {
    output = output.concat('', String.fromCodePoint(c >> int_shift));
  } else {
    fs.writeSync(process.stdout.fd, String.fromCodePoint(c >> int_shift));
  }
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
  if (process.argv[3] == "io") {
    console.log(output)
  }
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
  var i32 = new Uint32Array(instance.exports.memory.buffer);
  
  if (process.argv[3] == "io") {
    if ((result & ptr_type_mask) === 0) {
      console.log(result);
    } else {
      console.log(ans_str(result, i32));
    }
    console.log(output);
  } else {
    if (result != val_void) {
      console.log(ans_str(result, i32));
    }
  }
};

run();
