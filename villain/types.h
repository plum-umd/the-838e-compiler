#ifndef VILLAIN_TYPES_H
#define VILLAIN_TYPES_H

/*
  Bit layout of values

  Values are either:
  - Immediates: end in #b000
  - Pointers

  Immediates are either
  - Integers:   end in  #b0 000
  - Characters: end in #b01 000
  - True:              #b11 000
  - False:           #b1 11 000
  - Eof:            #b10 11 000
  - Void:           #b11 11 000
  - Empty:         #b100 11 000
*/
#define imm_shift        3
#define ptr_type_mask    (uint64_t)((((uint64_t)7) << 60) | (uint64_t)((1 << imm_shift) - 1))
#define ptr_addr_mask    (uint64_t)~ptr_type_mask
#define box_type_tag     (uint64_t)0x1
#define cons_type_tag    (uint64_t)0x2
#define str_type_tag     (uint64_t)0x3
#define symbol_type_tag  (uint64_t)0x4
#define port_type_tag    (uint64_t)0x5
#define vector_type_tag  (uint64_t)0x6
#define flonum_type_tag  (uint64_t)0x7	 
#define bignum_type_tag  (uint64_t)0x1000000000000003
#define proc_type_tag    (uint64_t)0x1000000000000002

#define int_shift        (1 + imm_shift)
#define int_type_mask    ((1 << int_shift) - 1)
#define int_type_tag     (0 << (int_shift - 1))
#define nonint_type_tag  (1 << (int_shift - 1))
#define char_shift       (int_shift + 1)
#define char_type_mask   ((1 << char_shift) - 1)
#define char_type_tag    ((0 << (char_shift - 1)) | nonint_type_tag)
#define nonchar_type_tag ((1 << (char_shift - 1)) | nonint_type_tag)
#define val_true  ((0 << char_shift) | nonchar_type_tag)
#define val_false ((1 << char_shift) | nonchar_type_tag)
#define val_eof   ((2 << char_shift) | nonchar_type_tag)
#define val_void  ((3 << char_shift) | nonchar_type_tag)
#define val_empty ((4 << char_shift) | nonchar_type_tag)

#endif
