#include "subprocess_comm.h"

#include <signal.h>

#include <chrono>
#include <condition_variable>
#include <exception>
#include <iostream>
#include <thread>

namespace hol_light {
namespace {
enum Request {
  kSetEncoding = 7,
};
enum Response {
  kOk = 0,
  kError = 1,
};
constexpr char kHolPath[] = "hol_light_sandboxee";
}  // namespace

SubprocessComm::~SubprocessComm() {}

SubprocessComm::SubprocessComm()
    : ok_(google::protobuf::util::Status::OK),
      comms_failure_(google::protobuf::util::Status(
          google::protobuf::util::error::INTERNAL,
          "Communication with sandbox (subprocess_comm.cc) failed.")),
      subprocess_(Subprocess::Start(kHolPath)),
      comms_(subprocess_.comms()) {
  std::cout << "Waiting for hol-light to get ready." << std::endl << std::flush;
  comms_.ReceiveInt();
  std::cout << "HOL Light ready.\n";
  // Hardcode SEXP encoding.
  comms_.SendInt(kSetEncoding);
  comms_.SendInt(2);
  if (comms_.ReceiveInt() != kOk) {
    std::cerr << "Could not set encoding.\n";
    throw std::logic_error("Could not set encoding");
  }
}

Status SubprocessComm::GetStatus() {
  try {
    int result = comms_.ReceiveInt();
    if (result == kOk) return ok_;
    string error = comms_.ReceiveString();
    return google::protobuf::util::Status(
        google::protobuf::util::error::INTERNAL, error);
  } catch (...) {
    return comms_failure_;
  }
}

Status SubprocessComm::SendInt(int64 n) {
  try {
    comms_.SendInt(n);
    return ok_;
  } catch (...) {
    return comms_failure_;
  }
}

Status SubprocessComm::SendString(const string& s) {
  try {
    comms_.SendString(s);
    return ok_;
  } catch (...) {
    return comms_failure_;
  }
}

Status SubprocessComm::ReceiveInt(int64* n) {
  try {
    *n = comms_.ReceiveInt();
    return ok_;
  } catch (...) {
    return comms_failure_;
  }
}

Status SubprocessComm::ReceiveString(string* s) {
  try {
    *s = comms_.ReceiveString();
    return ok_;
  } catch (...) {
    return comms_failure_;
  }
}

class ScopedTimerThread : public deepmath::ScopedTimer {
 public:
  ScopedTimerThread(pid_t pid, int time_milliseconds)
      : thread_([pid, time_milliseconds, this]() {
          this->SendSignal(pid, time_milliseconds);
        }) {}

  ~ScopedTimerThread() override {
    Cancel();
    thread_.join();
  }

 private:
  void Cancel() {
    cancelled_ = true;
    condition_variable_.notify_all();
  }

  bool SleepUnlessCancelled(int time_milliseconds) {
    std::unique_lock<std::mutex> lock(mutex_);
    auto now = std::chrono::system_clock::now();
    return !condition_variable_.wait_until(
        lock, now + std::chrono::milliseconds(time_milliseconds),
        [this]() { return this->cancelled_; });
  }

  void SendSignal(pid_t pid, int wait_seconds) {
    if (SleepUnlessCancelled(wait_seconds)) {
      kill(pid, SIGINT);
    }
  }
  mutable std::condition_variable condition_variable_;
  mutable std::mutex mutex_;
  bool cancelled_ = false;
  std::thread thread_;
};

std::unique_ptr<deepmath::ScopedTimer> SubprocessComm::GetTimer(
    int time_milliseconds) {
  return std::unique_ptr<ScopedTimerThread>(
      new ScopedTimerThread(subprocess_.pid(), time_milliseconds));
}

SubprocessComm* SubprocessComm::Create() {
  try {
    return new SubprocessComm();
  } catch (...) {
    return nullptr;
  }
}

}  // namespace hol_light
