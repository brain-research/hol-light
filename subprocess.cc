#include "subprocess.h"

#include <signal.h>

namespace hol_light {
namespace {
enum Type {
  kInt64 = 1,
  kString = 2,
};

constexpr char kReceiveFd[] = "COMMS_RECEIVE_FD";
constexpr char kSendFd[] = "COMMS_SEND_FD";

void SetEnv(const char* name, int value) {
  auto s = std::to_string(value);
  setenv(name, s.data(), 1);
}

int GetEnv(const char* name) { return std::stoi(getenv(name)); }

void MaybeClose(int fd) {
  if (fd) close(fd);
}

}  // namespace

int64_t Comms::ReceiveInt() const {
  if (Receive<Type>() != kInt64) {
    throw std::logic_error("Expected kInt64.");
  }
  return Receive<int64_t>();
}

void Comms::SendInt(int64_t val) const {
  Send(kInt64);
  write(send_fd_, &val, sizeof(val));
}

std::string Comms::ReceiveString() const {
  if (Receive<Type>() != kString) {
    throw std::logic_error("Expected kString.");
  }
  std::size_t len = Receive<std::size_t>();
  char data[len];
  for (int i = 0; i < len;) {
    int bytes = read(receive_fd_, data + i, len - i);
    if (bytes == 0) {
      throw std::logic_error("Comms::ReceiveString failed.");
    }
    i += bytes;
  }
  return {data, len};
}

void Comms::SendString(const std::string& val) const {
  Send(kString);
  std::size_t len = val.size();
  Send(len);
  const char* data = val.data();
  for (int i = 0; i < len;) {
    int bytes = write(send_fd_, data + i, len - i);
    if (bytes == 0) {
      throw std::logic_error("Comms::SendString failed.");
    }
    i += bytes;
  }
}

template <typename T>
T Comms::Receive() const {
  T t;
  int len = read(receive_fd_, &t, sizeof(t));
  if (len != sizeof(t)) {
    throw std::logic_error("Comms::Receive failed");
  }
  return t;
}

template <typename T>
void Comms::Send(T t) const {
  int len = write(send_fd_, &t, sizeof(t));
  if (len != sizeof(t)) {
    throw std::logic_error("Comms::Send failed");
  }
}

Subprocess Subprocess::Start(const std::string& cmd) {
  int fd[4];
  if (pipe(fd) || pipe(fd + 2)) {
    throw std::runtime_error("Failed to create pipes.");
  }
  pid_t child = fork();
  if (child == -1) {
    throw std::runtime_error("Failed to fork.");
  }
  if (child) {
    close(fd[1]);
    close(fd[2]);
    return Subprocess(child, fd[0], fd[3]);
  } else {
    close(fd[0]);
    close(fd[3]);
    SetEnv(kReceiveFd, fd[2]);
    SetEnv(kSendFd, fd[1]);
    // TODO(kbk): Consider using excele.
    setenv("CHEAT_BUILTIN", "", 1);
    execl(cmd.data(), cmd.data(), nullptr);
  }
}

const Comms& Subprocess::child_comms() {
  static auto* comms = new Comms(GetEnv(kReceiveFd), GetEnv(kSendFd));
  return *comms;
}

Subprocess::Subprocess(Subprocess&& rhs) { *this = std::move(rhs); }
Subprocess& Subprocess::operator=(Subprocess&& rhs) {
  pid_ = rhs.pid_;
  comms_ = rhs.comms_;
  rhs.comms_ = {0, 0};
}

Subprocess::~Subprocess() {
  MaybeClose(comms_.receive_fd_);
  MaybeClose(comms_.send_fd_);
}

const void Subprocess::Signal(int signal) const { kill(pid_, signal); }
Subprocess::Subprocess(pid_t pid, int receive_fd, int send_fd)
    : pid_(pid), comms_(receive_fd, send_fd) {}

}  // namespace hol_light
