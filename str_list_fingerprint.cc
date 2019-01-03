#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "farmhash.h"

namespace {
// The maximum signed int in 64-bit Ocaml is 2**62-1.
// We drop an additional bit to avoid unsigned->signed conversion subtleties.
constexpr uint64_t kMask = (static_cast<uint64_t>(1) << 62) - 1;
}  // namespace

namespace util {
inline uint64_t Fingerprint(uint64_t x, uint64_t y) {
  return Fingerprint(Uint128(x, y));
}
}  // namespace farmhash

extern "C" {

CAMLprim value TheoremFingerprint(value str_list) {
  CAMLparam1(str_list);
  bool first_iteration = true;
  uint64_t result = 0;
  while (str_list != Val_emptylist) {
    const auto& s = Field(str_list, 0);
    uint64_t f = util::Fingerprint64(String_val(s), caml_string_length(s));
    result = first_iteration ? f : util::Fingerprint(result, f);
    str_list = Field(str_list, 1);
    first_iteration = false;
  }
  CAMLreturn(Val_long(result & kMask));
}
}
