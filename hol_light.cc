#include "hol_light.h"

#include <exception>
#include <iostream>

namespace n2formal_hol_light {

namespace {
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
};

enum Response {
  kOk = 0,
  kError = 1,
};

constexpr char kHolPath[] = "core";

}  // namespace

HolLight::HolLight()
    : subprocess_(Subprocess::Start(kHolPath)), comms_(subprocess_.comms()) {
  std::cout << "Started HOL Light; waiting for initialization.\n";
  comms_.ReceiveInt();
  std::cout << "HOL Light ready.\n";
  // Hardcode SEXP encoding.
  comms_.SendInt(kSetEncoding);
  comms_.SendInt(2);
  comms_.ReceiveInt();
}

void HolLight::ApplyTactic(const ApplyTacticRequest& request,
                           ApplyTacticResponse* response) const {
  // TODO: Time limit
  comms_.SendInt(kApplyTacticToGoal);
  SendStatement(request.goal());
  comms_.SendString(request.tactic());
  CheckStatus();
  int64_t result = comms_.ReceiveInt();
  if (result == kOk) {
    ReceiveGoals(response->mutable_goals());
  } else {
    response->set_error(comms_.ReceiveString());
  }
}

void HolLight::VerifyProof(const VerifyProofRequest& request,
                           VerifyProofResponse* response) const {
  // TODO: Implement
}

void HolLight::SendStatement(const Statement& statement) const {
  comms_.SendInt(1 + statement.hypotheses_size());
  comms_.SendString(statement.conclusion());
  for (const auto& hypothesis : statement.hypotheses()) {
    comms_.SendString(hypothesis);
  }
}

void HolLight::ReceiveStatement(Statement* statement) const {
  int64_t n = comms_.ReceiveInt();
  for (int64_t i = 0; i < n; ++i) {
    std::string* term = (i == 0) ? statement->mutable_conclusion()
                                 : statement->add_hypotheses();
    *term = comms_.ReceiveString();
  }
}

void HolLight::ReceiveGoals(ApplyTacticResponse::GoalList* goals) const {
  for (int64_t n = comms_.ReceiveInt(); n > 0; --n) {
    ReceiveStatement(goals->add_goals());
  }
}

void HolLight::CheckStatus() const {
  if (comms_.ReceiveInt() != 0) {
    throw std::logic_error(comms_.ReceiveString());
  }
}

}  // namespace n2formal_hol_light
