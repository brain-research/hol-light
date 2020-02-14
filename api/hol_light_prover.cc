#include "hol_light_prover.h"

#include <sstream>

// Ignore this comment (1).
// Ignore this comment (2).
#include "proof_assistant.pb.h"
// Ignore this comment (3).
#include "google/protobuf/stubs/status.h"
// Ignore this comment (5).

using ::util::Status;
using ::util::StatusOr;

namespace deepmath {
namespace {
// LINT.IfChange
enum Request {
  kSetGoal = 0,
  kGetGoals = 1,
  kRotate = 2,
  kApplyTactic = 3,
  kUndo = 4,
  kRegisterLastTheorem = 5,
  kDefine = 6,
  kSetEncoding = 7,
  kApplyTacticToGoal = 8,
  kRegisterTheorem = 9,
  kCompareLastTheorem = 10,
  kDefineType = 11,
  kNegateGoal = 12,
  kApplyRule = 13,
};
// LINT.ThenChange(//hol_light/sandboxee.ml)

constexpr int kTermEncodingSExpr = 2;
}  // namespace

HolLightProver::HolLightProver(Comm* comm) : comm_(comm) {
  CHECK_OK(SetTermEncoding(kTermEncodingSExpr));
}

util::StatusOr<ApplyTacticResponse> HolLightProver::ApplyTactic(
    const ApplyTacticRequest& request) {
  RETURN_IF_ERROR(comm_->SendInt(kApplyTacticToGoal));
  RETURN_IF_ERROR(SendGoal(request.goal()));
  RETURN_IF_ERROR(comm_->SendString(request.tactic()));
  LOG(INFO) << "Calling HOL Light to apply tactic; setting timer (in ms):"
            << request.timeout_ms();
  auto timer = comm_->GetTimer(request.timeout_ms());
  RETURN_IF_ERROR(comm_->GetStatus());
  ApplyTacticResponse response;
  int64 result;
  RETURN_IF_ERROR(comm_->ReceiveInt(&result));
  if (result == kOk) {
    RETURN_IF_ERROR(ReceiveGoals(response.mutable_goals()));
  } else {
    RETURN_IF_ERROR(comm_->ReceiveString(response.mutable_error()));
  }
  return response;
}

util::StatusOr<ApplyRuleResponse> HolLightProver::ApplyRule(
    const ApplyRuleRequest& request) {
  RETURN_IF_ERROR(comm_->SendInt(kApplyRule));
  RETURN_IF_ERROR(comm_->SendString(request.rule()));
  LOG(INFO) << "Calling HOL Light to apply rule; setting timer (in ms):"
            << request.timeout_ms();
  auto timer = comm_->GetTimer(request.timeout_ms());
  RETURN_IF_ERROR(comm_->GetStatus());
  ApplyRuleResponse response;
  int64 result;
  RETURN_IF_ERROR(comm_->ReceiveInt(&result));
  if (result == kOk) {
    RETURN_IF_ERROR(ReceiveTheorem(response.mutable_theorem()));
  } else {
    RETURN_IF_ERROR(comm_->ReceiveString(response.mutable_error()));
  }
  return response;
}

util::StatusOr<VerifyProofResponse> HolLightProver::VerifyProof(
    const VerifyProofRequest& request) {
  VerifyProofResponse response;
  Status status = VerifyProofImpl(request);
  response.set_sound(status.ok());
  if (!status.ok()) {
    // add error_msg only in case of an error to keep message small
    response.set_error_msg(status.ToString());
  }
  return response;
}

util::Status HolLightProver::VerifyProofImpl(
    const VerifyProofRequest& request) {
  // TODO(stewbasic): Distinguish parse failures from tactic failures?
  RETURN_IF_ERROR(SetGoal(request.goal()));
  for (const auto& tactic : request.tactics()) {
    RETURN_IF_ERROR(ApplyTactic(tactic));
  }
  ASSIGN_OR_RETURN(auto goals, GetGoals());
  if (!goals.goals().empty()) {
    return util::InvalidArgumentError("Proof did not close all subgoals.");
  }
  RETURN_IF_ERROR(CompareLastTheorem(request.theorem()));
  RETURN_IF_ERROR(RegisterLastTheorem());
  return util::OkStatus();
}

Status HolLightProver::SetGoal(const Theorem& goal) {
  RETURN_IF_ERROR(comm_->SendInt(kSetGoal));
  RETURN_IF_ERROR(SendGoal(goal));
  return comm_->GetStatus();
}

StatusOr<GoalList> HolLightProver::GetGoals() {
  RETURN_IF_ERROR(comm_->SendInt(kGetGoals));
  RETURN_IF_ERROR(comm_->GetStatus());
  GoalList goals;
  RETURN_IF_ERROR(ReceiveGoals(&goals));
  return goals;
}

StatusOr<string> HolLightProver::NegateGoal(const Theorem& goal) {
  RETURN_IF_ERROR(comm_->SendInt(kNegateGoal));
  RETURN_IF_ERROR(SendGoal(goal));
  RETURN_IF_ERROR(comm_->GetStatus());
  string negated_goal_as_term;
  RETURN_IF_ERROR(comm_->ReceiveString(&negated_goal_as_term));
  return negated_goal_as_term;
}

Status HolLightProver::ReceiveGoals(GoalList* goals) {
  int64 n, m;
  RETURN_IF_ERROR(comm_->ReceiveInt(&n));
  for (int64 i = 0; i < n; ++i) {
    Theorem* goal = goals->add_goals();
    goal->set_tag(Theorem::GOAL);
    RETURN_IF_ERROR(comm_->ReceiveInt(&m));
    for (int64 j = 0; j < m; ++j) {
      string* term = (j == 0) ? goal->mutable_pretty_printed()
                              : (j == 1) ? goal->mutable_conclusion()
                                         : goal->add_hypotheses();
      RETURN_IF_ERROR(comm_->ReceiveString(term));
    }
  }
  return util::OkStatus();
}

Status HolLightProver::SendGoal(const Theorem& goal) {
  // TODO(stewbasic): Consider replacing goal by theorem and combining this with
  // SendTheorem.
  RETURN_IF_ERROR(comm_->SendInt(1 + goal.hypotheses_size()));
  RETURN_IF_ERROR(comm_->SendString(goal.conclusion()));
  for (const auto& hyp : goal.hypotheses()) {
    RETURN_IF_ERROR(comm_->SendString(hyp));
  }
  return util::OkStatus();
}

Status HolLightProver::ReceiveTheorem(Theorem* theorem) {
  int64 n;
  theorem->set_tag(Theorem::THEOREM);
  RETURN_IF_ERROR(comm_->ReceiveInt(&n));
  for (int64 i = 0; i < n; ++i) {
    string* term = (i == 0) ? theorem->mutable_pretty_printed()
                            : (i == 1) ? theorem->mutable_conclusion()
                                       : theorem->add_hypotheses();
    RETURN_IF_ERROR(comm_->ReceiveString(term));
  }
  return util::OkStatus();
}

Status HolLightProver::SendTheorem(const Theorem& theorem) {
  RETURN_IF_ERROR(comm_->SendInt(1 + theorem.hypotheses_size()));
  RETURN_IF_ERROR(comm_->SendString(theorem.conclusion()));
  for (const auto& hypothesis : theorem.hypotheses()) {
    RETURN_IF_ERROR(comm_->SendString(hypothesis));
  }
  return util::OkStatus();
}

Status HolLightProver::ApplyTactic(const string& tactic) {
  auto timer = comm_->GetTimer(deepmath::ApplyTacticRequest().timeout_ms());
  RETURN_IF_ERROR(comm_->SendInt(kApplyTactic));
  RETURN_IF_ERROR(comm_->SendString(tactic));
  return comm_->GetStatus();
}

Status HolLightProver::RegisterLastTheorem() {
  RETURN_IF_ERROR(comm_->SendInt(kRegisterLastTheorem));
  return comm_->GetStatus();
}

Status HolLightProver::CompareLastTheorem(const Theorem& theorem) {
  RETURN_IF_ERROR(comm_->SendInt(kCompareLastTheorem));
  RETURN_IF_ERROR(SendTheorem(theorem));
  return comm_->GetStatus();
}

Status HolLightProver::Define(const Definition& def) {
  RETURN_IF_ERROR(comm_->SendInt(kDefine));
  RETURN_IF_ERROR(comm_->SendString(def.definition_type()));
  RETURN_IF_ERROR(comm_->SendString(def.definition_term()));
  if (def.definition_type() == "SPEC") {
    RETURN_IF_ERROR(comm_->SendInt(def.theorem_arg()));
    RETURN_IF_ERROR(comm_->SendInt(def.constants().size()));
    for (const string& constant : def.constants()) {
      RETURN_IF_ERROR(comm_->SendString(constant));
    }
  } else if (def.definition_type() == "RECURSIVE") {
    RETURN_IF_ERROR(comm_->SendInt(def.theorem_arg()));
  }
  return comm_->GetStatus();
}

Status HolLightProver::DefineType(const TypeDefinition& def,
                                  int64* fingerprint_result) {
  RETURN_IF_ERROR(comm_->SendInt(kDefineType));
  RETURN_IF_ERROR(comm_->SendString(def.type_name()));
  RETURN_IF_ERROR(comm_->SendString(def.abs_name()));
  RETURN_IF_ERROR(comm_->SendString(def.rep_name()));
  RETURN_IF_ERROR(comm_->SendInt(def.theorem_arg()));
  RETURN_IF_ERROR(comm_->GetStatus());
  RETURN_IF_ERROR(comm_->ReceiveInt(fingerprint_result));
  return util::OkStatus();
}

Status HolLightProver::SetTermEncoding(int encoding) {
  RETURN_IF_ERROR(comm_->SendInt(kSetEncoding));
  RETURN_IF_ERROR(comm_->SendInt(static_cast<int64>(encoding)));
  return comm_->GetStatus();
}

StatusOr<RegisterTheoremResponse> HolLightProver::RegisterTheorem(
    const RegisterTheoremRequest& request) {
  const auto& theorem = request.theorem();
  // Disallow theorems with hypotheses
  RegisterTheoremResponse response;
  if (!theorem.hypotheses().empty()) {
    response.set_error_msg("Theorem with hypotheses are not supported.");
    return response;
  }
  int64 fingerprint_from_ocaml;
  Status status = CheatTheorem(theorem, &fingerprint_from_ocaml);
  if (!status.ok()) {
    response.set_error_msg(status.ToString());
  }
  if (status.ok() && request.theorem().has_fingerprint() &&
      request.theorem().fingerprint() != fingerprint_from_ocaml) {
    std::ostringstream msg;
    msg << "Fingerprint " << request.theorem().fingerprint()
        << " in RegisterTheoremRequest does not match fingerprint "
        << fingerprint_from_ocaml << " returned from HOL Light.\n";
    response.set_error_msg(msg.str());
  }
  response.set_fingerprint(fingerprint_from_ocaml);
  return response;
}

Status HolLightProver::CheatTheorem(const Theorem& theorem,
                                    int64* fingerprint_result) {
  switch (theorem.tag()) {
    case Theorem::TYPE_DEFINITION:
      return DefineType(theorem.type_definition(), fingerprint_result);
    case Theorem::DEFINITION: {
      RETURN_IF_ERROR(comm_->SendInt(kRegisterTheorem));
      RETURN_IF_ERROR(SendTheorem(theorem));
      RETURN_IF_ERROR(comm_->GetStatus());
      RETURN_IF_ERROR(comm_->ReceiveInt(fingerprint_result));
      return util::OkStatus();
    }
    case Theorem::THEOREM: {
      RETURN_IF_ERROR(comm_->SendInt(kRegisterTheorem));
      RETURN_IF_ERROR(SendTheorem(theorem));
      RETURN_IF_ERROR(comm_->GetStatus());
      RETURN_IF_ERROR(comm_->ReceiveInt(fingerprint_result));
      return util::OkStatus();
    }
    default:
      return util::UnimplementedError(::absl::StrCat(
          "Theorem registration not implemented for theorem tag ",
          Theorem::Tag_Name(theorem.tag())));
  }
  return util::OkStatus();
}

}  // namespace deepmath
