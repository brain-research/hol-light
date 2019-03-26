#include "compatibility.h"

#include "theorem_fingerprint.h"

// Ignore this comment (2).
#include "farmhash_compatibility.h"

namespace deepmath {
namespace {
// The maximum signed int in 64-bit Ocaml is 2**62-1.
// We drop an additional bit to avoid unsigned->signed conversion subtleties.
constexpr uint64 kMask = (static_cast<uint64>(1) << 62) - 1;
}  // namespace

int64 Fingerprint(const Theorem& theorem) {
  // LINT.IfChange
  if (!theorem.has_conclusion() && theorem.has_fingerprint()) {
    // Needed for theorems in tactic parameters that may only be logged as fps.
    return theorem.fingerprint();
  }
  uint64 fp = farmhash::Fingerprint64(theorem.conclusion());
  for (const auto& hypothesis : theorem.hypotheses()) {
    uint64 tmp = farmhash::Fingerprint64(hypothesis);
    fp = farmhash::Fingerprint(fp, tmp);
  }
  int64 result = static_cast<int64>(fp & kMask);
  // LINT.ThenChange(//hol_light/theorem_fingerprint.cc)
  if (theorem.has_fingerprint() && theorem.fingerprint() != result) {
    LOG(ERROR) << "Inconsistent fingerprints in Theorem protobuf.";
  }
  return result;
}

string ToTacticArgument(const Theorem& theorem) {
  return absl::StrCat("THM ", Fingerprint(theorem));
}

}  // namespace deepmath
