#include "subprocess.h"

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

extern "C" {

using hol_light::Subprocess;

CAMLprim value RecvString(value unit) {
  CAMLparam1(unit);
  try {
    std::string s = Subprocess::child_comms().ReceiveString();
    CAMLlocal1(ml_s);
    ml_s = caml_copy_string(s.c_str());
    CAMLreturn(ml_s);
  } catch (std::exception& e) {
    caml_failwith(e.what());
  }
}

CAMLprim value RecvInt(value unit) {
  CAMLparam1(unit);
  try {
    int64_t v = Subprocess::child_comms().ReceiveInt();
    CAMLreturn(Val_long(v));
  } catch (std::exception& e) {
    caml_failwith(e.what());
  }
}

CAMLprim value SendString(value ml_s) {
  // Note: This assumes the string has no embedded null characters.
  std::string s(String_val(ml_s));
  try {
    Subprocess::child_comms().SendString(s);
  } catch (std::exception& e) {
    caml_failwith(e.what());
  }
  return Val_unit;
}

CAMLprim value SendInt(value v) {
  try {
    Subprocess::child_comms().SendInt(Long_val(v));
  } catch (std::exception& e) {
    caml_failwith(e.what());
  }
  return Val_unit;
}
}
