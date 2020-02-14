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
  if (!theorem.hypotheses().empty() && !theorem.assumptions().empty()) {
    LOG(ERROR) << "Goal can only have one of hypotheses or assumptions.";
  }
  if (!theorem.has_conclusion() && theorem.has_fingerprint()) {
    // Needed for theorems in tactic parameters that may only be logged as fps.
    return theorem.fingerprint();
  }
  uint64 fp = farmhash::Fingerprint64(theorem.conclusion());
  for (const auto& hypothesis : theorem.hypotheses()) {
    uint64 tmp = farmhash::Fingerprint64(hypothesis);
    fp = farmhash::Fingerprint(fp, tmp);
  }
  for (const auto& assumption : theorem.assumptions()) {
    uint64 tmp = Fingerprint(assumption);
    tmp = tmp + 1;  // Ensures that "[t1 |- t2], t3", "[|-t1, |-t2], t3" differ
    fp = farmhash::Fingerprint(fp, tmp);
  }
  int64 result = static_cast<int64>(fp & kMask);
  // LINT.ThenChange(//hol_light/theorem_fingerprint_c.cc)
  if (theorem.has_fingerprint() && theorem.fingerprint() != result) {
    LOG(ERROR) << "Inconsistent fingerprints in Theorem protobuf.";
  }
  return result;
}

string ToTacticArgument(const Theorem& theorem) {
  return absl::StrCat("THM ", Fingerprint(theorem));
}

}  // namespace deepmath
