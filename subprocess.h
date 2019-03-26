#ifndef SUBPROCESS_H_
#define SUBPROCESS_H_

#include <sys/types.h>
#include <unistd.h>
#include <stdexcept>
#include <string>

namespace hol_light {

class Comms {
 public:
  int64_t ReceiveInt() const;
  void SendInt(int64_t val) const;

  std::string ReceiveString() const;
  void SendString(const std::string& val) const;

 private:
  Comms() : receive_fd_(0), send_fd_(0) {}
  Comms(int receive_fd, int send_fd)
      : receive_fd_(receive_fd), send_fd_(send_fd) {}

  template <typename T>
  T Receive() const;
  template <typename T>
  void Send(T t) const;

  int receive_fd_, send_fd_;
  friend class Subprocess;
};

class Subprocess {
 public:
  static Subprocess Start(const std::string& cmd);
  // The child process should call child_comms.
  static const Comms& child_comms();

  const Comms& comms() const { return comms_; }
  const void Signal(int signal) const;

  Subprocess(const Subprocess&) = delete;
  Subprocess& operator=(const Subprocess&) = delete;
  Subprocess(Subprocess&&);
  Subprocess& operator=(Subprocess&&);
  ~Subprocess();

  inline pid_t pid() { return pid_; }

 private:
  Subprocess(pid_t pid, int receive_fd, int send_fd);
  pid_t pid_;
  Comms comms_;
};

}  // namespace hol_light

#endif  // SUBPROCESS_H_
