#include "compatibility.h"

namespace absl {
string StrCat(const string& s1, const string& s2) { return s1 + s2; }
string StrCat(const string& s1, int64 i) { return s1 + std::to_string(i); }
}  // namespace absl

namespace google {
namespace protobuf {
namespace util {

Status OkStatus() { return Status(); }

Status InvalidArgumentError(const string& msg) {
  return Status(error::INVALID_ARGUMENT, msg);
}

Status UnimplementedError(const string& msg) {
  return Status(error::UNIMPLEMENTED, msg);
}

}  // namespace util
}  // namespace protobuf
}  // namespace google
