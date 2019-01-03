#ifndef HOL_LIGHT_H_
#define HOL_LIGHT_H_

#include "hol_light.pb.h"
#include "subprocess.h"

namespace n2formal_hol_light {

class HolLight {
 public:
  HolLight();
  void ApplyTactic(const ApplyTacticRequest& request,
                   ApplyTacticResponse* response) const;
  void VerifyProof(const VerifyProofRequest& request,
                   VerifyProofResponse* response) const;

 private:
  void SendStatement(const Statement& statement) const;
  void ReceiveStatement(Statement* statement) const;
  void ReceiveGoals(ApplyTacticResponse::GoalList* goals) const;
  void CheckStatus() const;
  Subprocess subprocess_;

  // comms_ belongs to subprocess_
  const Comms& comms_;
};

}  // namespace n2formal_hol_light

#endif  // HOL_LIGHT_H_
