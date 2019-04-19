
#include <grpcpp/grpcpp.h>

#include <memory>
#include <mutex>

#include "proof_assistant.grpc.pb.h"
#include "subprocess_comm.h"
#include "hol_light_prover.h"

namespace deepmath {
namespace {

using grpc::Status;

class HolLightProverWithComm {
 public:
  HolLightProverWithComm()
      : comm_(hol_light::SubprocessComm::Create()),
        prover_(new HolLightProver(comm_.get())) {}

  ~HolLightProverWithComm() {}

  HolLightProver* get() { return prover_.get(); }

 private:
  std::unique_ptr<Comm> comm_;
  std::unique_ptr<HolLightProver> prover_;
};

class ProofAssistantServiceImpl final : public ProofAssistantService::Service {
 private:
  Status ApplyTactic(grpc::ServerContext* context,
                     const ApplyTacticRequest* request,
                     ApplyTacticResponse* response) override {
    std::lock_guard<std::mutex> lock(mu_);
    auto statusor = prover_.get()->ApplyTactic(*request);
    if (statusor.ok()) {
      *response = statusor.ValueOrDie();
    } else {
      const auto& status = statusor.status();
      return grpc::Status(static_cast<grpc::StatusCode>(status.error_code()),
                          status.error_message());
    }
    return Status::OK;
  }
  Status VerifyProof(grpc::ServerContext* context,
                     const VerifyProofRequest* request,
                     VerifyProofResponse* response) override {
    std::lock_guard<std::mutex> lock(mu_);
    auto statusor = prover_.get()->VerifyProof(*request);
    if (statusor.ok()) {
      *response = statusor.ValueOrDie();
    } else {
      const auto& status = statusor.status();
      return grpc::Status(static_cast<grpc::StatusCode>(status.error_code()),
                          status.error_message());
    }
    return Status::OK;
  }
  Status RegisterTheorem(grpc::ServerContext* context,
                         const RegisterTheoremRequest* request,
                         RegisterTheoremResponse* response) override {
    std::lock_guard<std::mutex> lock(mu_);
    auto statusor = prover_.get()->RegisterTheorem(*request);
    if (statusor.ok()) {
      *response = statusor.ValueOrDie();
    } else {
      const auto& status = statusor.status();
      return grpc::Status(static_cast<grpc::StatusCode>(status.error_code()),
                          status.error_message());
    }
    return Status::OK;
  }
  HolLightProverWithComm prover_;
  std::mutex mu_;
};

void RunServer() {
  std::cout << "Initializing server..." << std::endl;
  std::string server_address("0.0.0.0:2000");
  ProofAssistantServiceImpl service;
  std::cout << "Service implementation initialized" << std::endl;

  grpc::ServerBuilder builder;
  // Listen on the given address without any authentication mechanism.
  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  // Register "service" as the instance through which we'll communicate with
  // clients. In this case it corresponds to an *synchronous* service.
  builder.RegisterService(&service);
  // Increase maximum size of a request to a gigabyte.
  builder.SetMaxReceiveMessageSize(1024 * 1024 * 1024);
  builder.SetMaxSendMessageSize(1024 * 1024 * 1024);

  // Finally assemble the server.
  std::unique_ptr<grpc::Server> server(builder.BuildAndStart());

  // Wait for the server to shutdown. Note that some other thread must be
  // responsible for shutting down the server for this call to ever return.
  std::cout << "Waiting..." << std::endl;
  server->Wait();
}
}  // namespace
}  // namespace deepmath

int main(int argc, char** argv) {
  deepmath::RunServer();
  std::cout << "Exiting..." << std::endl;
  return 0;
}
