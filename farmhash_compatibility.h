#include "farmhash.h"

namespace farmhash {
inline uint64_t Fingerprint(uint64_t x, uint64_t y) {
  return Fingerprint(Uint128(x, y));
}
}  // namespace farmhash
