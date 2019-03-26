#ifndef THIRD_PARTY_HOL_LIGHT_DIRECT_COMM_H_
#define THIRD_PARTY_HOL_LIGHT_DIRECT_COMM_H_

#include <google/protobuf/stubs/status.h>
#include <memory>
#include <string>
#include "../subprocess.h"

typedef int64_t int64;

#include "comm.h"

namespace hol_light {

using google::protobuf::util::Status;
using std::string;

class SubprocessComm : public deepmath::Comm {
 public:
  virtual ~SubprocessComm() override;
  virtual Status GetStatus() override;
  virtual Status SendInt(int64 n) override;
  virtual Status SendString(const string& s) override;
  virtual Status ReceiveInt(int64* n) override;
  virtual Status ReceiveString(string* s) override;
  virtual std::unique_ptr<deepmath::ScopedTimer> GetTimer(
      int time_milliseconds) override;

  // Return nullptr on error.
  static SubprocessComm* Create();

 protected:
  SubprocessComm();

 private:
  Subprocess subprocess_;
  // comms_ belongs to subprocess_
  const Comms& comms_;
  const Status ok_, comms_failure_;
};

}  // namespace hol_light

#endif  // THIRD_PARTY_HOL_LIGHT_DIRECT_COMM_H_
