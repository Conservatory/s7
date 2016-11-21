define s7print
print s7_object_to_c_string(hidden_sc, $arg0)
end
document s7print
interpret the argument as an s7 value and display it
end

# the current expression is sc->cur_code
# the current environment is sc->envir
# the error environment is sc->owlet
# so for example, to see the current local variables, s7p sc->envir
# source ~/.gdbinit reloads


define s7eval
print s7_object_to_c_string(hidden_sc, s7_eval_c_string(hidden_sc, $arg0))
end
document s7eval
eval the argument (a string)
end


define s7stack
print s7_object_to_c_string(hidden_sc, s7_stacktrace(sc))
end
document s7stack
display the currently active local environments
end


define s7value
print s7_object_to_c_string(hidden_sc, s7_name_to_value(hidden_sc, $arg0))
end
document s7value
print the value of the variable passed by its print name: s7v "*features*"
end


define s7bt
set logging overwrite on
set logging redirect on
set logging on
bt
set logging off
# now gdb.txt has the backtrace
print s7_decode_bt()
end
document s7bt
print a backtrace with s7 objects decoded as much as possible
end

define s7btfull
set logging overwrite on
set logging redirect on
set logging on
bt full
set logging off
print s7_decode_bt()
end
document s7btfull
print a full backtrace with s7 objects decoded as much as possible
end


# this requires -g3 compilation
define s7cell
  set $escape = 27
  set $cell = (s7_cell *)$arg0
  set $type = $cell.tf.type_field
  set $is_bad_type = (($type <= T_FREE) || ($type >= NUM_TYPES))

  printf "%s\n", describe_type_bits(hidden_sc, $cell)
  printf "hloc: %d, ", $cell.hloc

  printf "fields: %p %p %p %p %p\n",\
     $cell.object.cons.car, $cell.object.cons.cdr, $cell.object.cons.opt1, $cell.object.cons.opt2, $cell.object.cons.opt3

  if (($type == T_NIL) || ($type == T_BOOLEAN) || ($type == T_UNIQUE))
      printf "constant:       %s\n", $cell.object.unq.name
  end

  if (($type == T_INTEGER) || ($is_bad_type))
      printf "integer_value:  %lld\n", $cell.object.number.integer_value
  end

  if (($type == T_REAL) || ($is_bad_type))
      printf "real_value:     %f\n", $cell.object.number.real_value
  end

  if (($type == T_RATIO) || ($is_bad_type))
      printf "fraction_value: %lld / %lld\n", $cell.object.number.fraction_value.numerator, $cell.object.number.fraction_value.denominator
  end

  if (($type == T_COMPLEX) || ($is_bad_type))
      printf "complex_value:  %f + %fi\n", $cell.object.number.complex_value.rl, $cell.object.number.complex_value.im
  end

  if ($type == T_BIG_INTEGER)
     if (WITH_GMP)
        printf "big integer: %p\n", $cell.object.number.big_integer
     else
        printf "big integer??\n"
     end
  end

  if ($type == T_BIG_RATIO)
     if (WITH_GMP)
        printf "big ratio: %p\n", $cell.object.number.big_ratio
     else
        printf "big ratio??\n"
     end
  end

  if ($type == T_BIG_REAL)
     if (WITH_GMP)
        printf "big real: %p\n", $cell.object.number.big_real
     else
        printf "big real??\n"
     end
  end

  if ($type == T_BIG_COMPLEX)
     if (WITH_GMP)
        printf "big complex: %p\n", $cell.object.number.big_complex
     else
        printf "big complex??\n"
     end
  end

  if (($is_bad_type) || (($cell.tf.flag & T_PRINT_NAME) != 0))
      printf "pval: padding:  %s, name: %s\n", $cell.object.number.pval.padding, $cell.object.number.pval.name
  end

  if ($is_bad_type)
      printf "ul_value:       %ld, %lld\n", $cell.object.number.ul_value, $cell.object.number.ull_value 
  end

  if ($type == T_BAFFLE)
      printf "baffle:         %d\n", $cell.object.baffle_key
  end

  if ($type == T_C_POINTER)
      printf "c_pointer:      %p\n", $cell.object.c_pointer
  end

  if ($type == T_RANDOM_STATE)
    if (WITH_GMP) 
       printf "random-state\n"
    else
       printf "random-state: seed: %lld, carry: %lld\n", $cell.object.rng.seed, $cell.object.rng.carry
    end
  end

  if (($type == T_PAIR) || ($is_bad_type))
      printf "cons:           car: %p, cdr: %p, opt1: %p, opt2: %p, opt3: %p\n", \
         $cell.object.cons.car, $cell.object.cons.cdr, $cell.object.cons.opt1, $cell.object.cons.opt2, $cell.object.cons.opt3
      printf "                    line: %d, op: %d\n", \
         $cell.object.sym_cons.line, $cell.object.sym_cons.op
  end

  if (($type == T_CHARACTER) || ($is_bad_type))	 
      printf "chr:            c: %c, up_c: %c, alpha_c: %d, digit_c: %d, space_c: %d, upper_c: %d, lower_c: %d", \
         $cell.object.chr.c, $cell.object.chr.up_c, \
         $cell.object.chr.alpha_c, $cell.object.chr.digit_c, $cell.object.chr.space_c, $cell.object.chr.upper_c, $cell.object.chr.lower_c
      if ($type == T_CHARACTER) 
        printf ", c_name: %s, length: %d", $cell.object.chr.c_name, $cell.object.chr.length
      end
  end

  if ($type == T_SLOT)
      printf "slt:            sym: %p, val: %p, nxt: %p, pending_value: %p, expr: %p\n", \
         $cell.object.slt.sym, $cell.object.slt.val, $cell.object.slt.nxt, $cell.object.slt.pending_value, $cell.object.slt.expr
  end

  if ($type == T_LET)
      printf "envr:           slots: %p, nxt: %p, id: %lld\n", $cell.object.envr.slots,  $cell.object.envr.nxt,  $cell.object.envr.id
  end

  if ($type == T_ITERATOR)
      printf "iter:           obj: %p, cur: %p, loc: %lld\n", $cell.object.iter.obj,  $cell.object.iter.cur,  $cell.object.iter.lc.loc
  end

  if ($type == T_COUNTER)
      printf "ctr:            result: %p, list: %p, env: %p, cap: %lld, len: %lld\n", \
         $cell.object.ctr.result, $cell.object.ctr.list, $cell.object.ctr.env, $cell.object.ctr.cap, $cell.object.ctr.len
  end

  if ($type == T_C_OBJECT)
      if (num_types >= $cell.object.c_obj.type)
          printf "c_obj:          %c[31mtype: %d%c[0m, value: %p, e: %p\n", $escape, $cell.object.c_obj.type, $escape, $cell.object.c_obj.value ,$cell.object.c_obj.e
      else
          printf "c_obj:          type: %d, value: %p, e: %p\n", $cell.object.c_obj.type, $cell.object.c_obj.value ,$cell.object.c_obj.e
      end
  end

  if (($type == T_STRING) || ($is_bad_type))
      if (($cell.tf.type_field & T_BYTE_VECTOR) != 0)
         printf "byte-vector:     "
      else
         printf "string:         "
      end
      printf "length: %d, hash: %d, needs_free: %d, initial_slot: %p", \
         $cell.object.string.length, $cell.object.string.hash, $cell.object.string.str_ext.needs_free, $cell.object.string.initial_slot
      if ($type == T_STRING)
         printf ", svalue: %s", $cell.object.string.svalue
      end
      printf "\n"
  end

  if (($type == T_SYMBOL) || ($is_bad_type))
      printf "sym:            name: %p, global_slot: %p, local_slot: %p, \n                    id: %d, tag: %d, accessor: %d\n", \
         $cell.object.sym.name, $cell.object.sym.global_slot, $cell.object.sym.local_slot, $cell.object.sym.id, $cell.object.sym.tag, $cell.object.string.str_ext.accessor
  end

  if (($type == T_SYNTAX) || ($is_bad_type))
      printf "syn:            symbol: %p, op: %d\n", $cell.object.syn.symbol, $cell.object.syn.op
  end

  if (($type == T_CONTINUATION) || ($is_bad_type))
      printf "continuation:   continuation: %p, stack: %p, start_start: %p, stack_end: %p, op_stack: %p, key: %d\n", \
         $cell.object.cwcc.continuation, $cell.object.cwcc.stack, $cell.object.cwcc.stack_start, $cell.object.cwcc.stack_end, $cell.object.cwcc.op_stack, \
         $cell.object.cwcc.continuation->local_key
  end

  if (($type == T_VECTOR) || ($is_bad_type) || ($type == T_INT_VECTOR) || ($type == T_FLOAT_VECTOR))
      printf "\nvector:         length: %lld, dim_info: %p, vget: %p, vset: %p, objects: %p\n", \
         $cell.object.vector.length, $cell.object.vector.dim_info, $cell.object.vector.vget, $cell.object.vector.vset, $cell.object.vector.elements.objects
  end

  if (($type == T_HASH_TABLE) || ($is_bad_type))
      printf "hasher:         mask: %lld, elements: %p, entries: %d,\n                    hash_func: %p, loc: %p, dproc: %p\n", \
         $cell.object.hasher.mask, $cell.object.hasher.elements, $cell.object.hasher.entries, $cell.object.hasher.hash_func, $cell.object.hasher.loc, $cell.object.hasher.dproc
  end

  if (($type == T_INPUT_PORT) || ($type == T_OUTPUT_PORT))
      printf "prt:            port: %p, data: %p, size: %d, point: %d,\n                    line_number: %d, file_number: %d, is_closed: %d, ptype: %d\n", \
         $cell.object.prt.port, $cell.object.prt.data, $cell.object.prt.size, $cell.object.prt.point, \
	 $cell.object.prt.line_number, $cell.object.prt.file_number, $cell.object.prt.is_closed, $cell.object.prt.ptype
  end

  if (($type == T_CATCH) || ($is_bad_type))
      printf "rcatch:     goto_loc: %d, op_stack_loc: %d, tag: %p, handler: %p\n", \
         $cell.object.rcatch.goto_loc, $cell.object.rcatch.op_stack_loc, $cell.object.rcatch.tag, $cell.object.rcatch.handler
  end

  if (($type == T_GOTO) || ($is_bad_type))
      printf "rexit:      goto_loc: %d, op_stack_loc: %d, active: %d\n", $cell.object.rexit.goto_loc, $cell.object.rexit.op_stack_loc, $cell.object.rexit.active
  end

  if (($type == T_DYNAMIC_WIND) || ($is_bad_type))
      printf "winder:     in: %p, out: %p, body: %p, state: %d\n", \
         $cell.object.winder.in, $cell.object.winder.out, $cell.object.winder.body, $cell.object.winder.state
  end

  if (($type == T_CLOSURE) || ($type == T_CLOSURE_STAR) || ($type == T_MACRO) || ($type == T_BACRO) || ($is_bad_type))
      printf "func:           args: %p, body: %p, env: %p, setter: %p, arity: %d\n", \
         $cell.object.func.args, $cell.object.func.body, $cell.object.func.env, $cell.object.func.setter, $cell.object.func.arity
  end
 
  if (($type >= T_C_FUNCTION_STAR) || ($is_bad_type))
      printf "c-fnc:          c_proc: %p, ff: %p, setter: %p,\n                required_args: %d, optional_args: %d, all_args: %d, rest_arg: %d\n", \
         $cell.object.fnc.c_proc, $cell.object.fnc.ff, $cell.object.fnc.setter, \
         $cell.object.fnc.required_args, $cell.object.fnc.optional_args, $cell.object.fnc.all_args, $cell.object.fnc.rest_arg
      if ((!$is_bad_type) && ($cell.object.fnc.c_proc != 0))
         printf "                name: %s\n", $cell.object.fnc.c_proc->name
  end

end
