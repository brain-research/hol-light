#ifndef HOL_LIGHT_API_THEOREM_FINGERPRINT_H_
#define HOL_LIGHT_API_THEOREM_FINGERPRINT_H_

#include "proof_assistant.pb.h"

namespace deepmath {

// Returns a stable fingerprint of the given theorem.
// DEPRECATED: use either the python or the ocaml version.
int64 Fingerprint(const Theorem& theorem);

// Returns tactic parameter referencing the given theorem, in a format
// understood by the HOL Light prover. The prover must have already proven the
// theorem (ie it should be a built-in theorem). Concretely, the theorem is
// referenced by its fingerprint.
string ToTacticArgument(const Theorem& theorem);

}  // namespace deepmath

#endif  // HOL_LIGHT_API_THEOREM_FINGERPRINT_H_
