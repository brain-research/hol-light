#ifndef HOL_LIGHT_API_HOL_LIGHT_PROVER_H_
#define HOL_LIGHT_API_HOL_LIGHT_PROVER_H_

#include "compatibility.h"

#include <memory>

#include "proof_assistant.pb.h"
#include "comm.h"
// Ignore this comment (4).

namespace deepmath {

// Exposes methods for interacting programmatically with the HOL Light theorem
// prover.
class HolLightProver {
 public:
  explicit HolLightProver(Comm* comm);
  ~HolLightProver() {}

  // Apply a tactic to a goal. The response is either of the list of new
  // subgoals, or a string indicating why application of the tactic failed. This
  // function is not expected to return a non-OK status; this would imply an
  // internal error which has put the prover into a corrupted state.
  util::StatusOr<ApplyTacticResponse> ApplyTactic(
      const ApplyTacticRequest& request);

  // Apply a rule. The response is either a new theorem, or a string indicating
  // why application of the rule failed. Not expected to return non-OK status.
  util::StatusOr<ApplyRuleResponse> ApplyRule(const ApplyRuleRequest& request);

  // Verifies that a proof is sound.
  util::StatusOr<VerifyProofResponse> VerifyProof(
      const VerifyProofRequest& request);

  // Registers a theorem
  util::StatusOr<RegisterTheoremResponse> RegisterTheorem(
      const RegisterTheoremRequest& request);

  // Use the Create method for safe creation of new objects.
  HolLightProver() {}

  // Methods mimicking interactive proof construction.
  // Sets a goal.
  util::Status SetGoal(const Theorem& goal);

  // Gets the current goal list. Requires that SetGoal has been called.
  util::StatusOr<GoalList> GetGoals();

  // Attempts to apply a tactic to the current goal. Returns an error if the
  // tactic can't be parsed or is inapplicable.
  util::Status ApplyTactic(const string& tactic);

  // Methods for proof verification

  // Registers the last theorem proven. Requires that SetGoal has been
  // called and the current goal (returned by GetGoal) is empty.
  // After this call, the theorem can be specified using ToTacticArgument.
  util::Status RegisterLastTheorem();

  // Checks if last theorem has the same fingerprint as the given theorem.
  util::Status CompareLastTheorem(const Theorem& theorem);

  // Cheats the given theorem, so that it can be specified later using
  // ToTacticArgument. Returns the fingerprint computed by OCaml.
  util::Status CheatTheorem(const Theorem& theorem, int64* fingerprint_result);

  // Defines a new symbol, where the definition contains the type of definition
  // and the defining term. Some definition types require additional parameters.
  util::Status Define(const Definition& definition);

  // Defines a new type.
  util::Status DefineType(const TypeDefinition& definition,
                          int64* fingerprint_result);

  // Determines how terms will be represented as strings henceforth.
  // (1=pretty printed, 2=sexpr)
  util::Status SetTermEncoding(int encoding);

  // Returns an error if the proof fails. We will use this to populate the
  // response.
  util::Status VerifyProofImpl(const VerifyProofRequest& request);
  util::Status SendGoal(const Theorem& goal);
  util::Status SendTheorem(const Theorem& theorem);
  util::Status ReceiveTheorem(Theorem* theorem);
  util::Status ReceiveGoals(GoalList* goals);

  util::StatusOr<string> NegateGoal(const Theorem& goal);

 private:
  Comm* comm_;
};

}  // namespace deepmath

#endif  // HOL_LIGHT_API_HOL_LIGHT_PROVER_H_
