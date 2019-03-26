#ifndef HOL_LIGHT_API_COMM_H_
#define HOL_LIGHT_API_COMM_H_

#include <sys/types.h>
#include <memory>
#include <string>
#include "google/protobuf/stubs/status.h"

namespace deepmath {
using google::protobuf::util::Status;
using std::string;
typedef int64_t int64;

// LINT.IfChange
enum Response {
  kOk = 0,
  kError = 1,
};
// LINT.ThenChange(//hol_light/sandboxee.ml)

class ScopedTimer {
 public:
  virtual ~ScopedTimer() {}
};

class Comm {
 public:
  virtual ~Comm() {}
  virtual Status GetStatus() = 0;
  virtual Status SendInt(int64 n) = 0;
  virtual Status SendString(const string& s) = 0;
  virtual Status ReceiveInt(int64* n) = 0;
  virtual Status ReceiveString(string* s) = 0;
  virtual std::unique_ptr<ScopedTimer> GetTimer(int time_milliseconds) = 0;

 protected:
  Comm() {}
};

}  // namespace deepmath

#endif  // HOL_LIGHT_API_COMM_H_
