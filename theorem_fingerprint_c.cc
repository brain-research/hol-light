#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "farmhash_compatibility.h"


namespace {
// The maximum signed int in 64-bit Ocaml is 2**62-1.
// We drop an additional bit to avoid unsigned->signed conversion subtleties.
constexpr uint64 kMask = (static_cast<uint64>(1) << 62) - 1;
}  // namespace

extern "C" {

CAMLprim value TheoremFingerprint(value concl_str, value hyp_strings,
                                  value assum_fingerprints) {
  CAMLparam3(concl_str, hyp_strings, assum_fingerprints);
  // LINT.IfChange
  uint64 result = farmhash::Fingerprint64(String_val(concl_str),
                                          caml_string_length(concl_str));
  while (hyp_strings != Val_emptylist) {
    const auto& s = Field(hyp_strings, 0);
    uint64 f = farmhash::Fingerprint64(String_val(s), caml_string_length(s));
    result = farmhash::Fingerprint(result, f);
    hyp_strings = Field(hyp_strings, 1);
  }
  while (assum_fingerprints != Val_emptylist) {
    uint64 f = Long_val(Field(assum_fingerprints, 0));
    f = f + 1;  // Ensures that "[t1 |- t2], t3" and "[|-t1, |-t2], t3" differ
    result = farmhash::Fingerprint(result, f);
    assum_fingerprints = Field(assum_fingerprints, 1);
  }
  CAMLreturn(Val_long(result & kMask));
  // LINT.ThenChange(//hol_light/api/theorem_fingerprint.cc)
}
}
