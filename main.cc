#include <grpcpp/grpcpp.h>
#include <memory>
#include <mutex>

#include "hol_light.grpc.pb.h"
#include "hol_light.h"

namespace n2formal_hol_light {
namespace {

using grpc::Status;

class HolLightServiceImpl final : public HolLightService::Service {
 private:
  Status ApplyTactic(grpc::ServerContext* context,
                     const ApplyTacticRequest* request,
                     ApplyTacticResponse* response) override {
    std::lock_guard<std::mutex> lock(mu_);
    try {
      hol_light_.ApplyTactic(*request, response);
    } catch (std::exception& e) {
      return Status(grpc::StatusCode::INTERNAL, e.what());
    }
    return Status::OK;
  }
  Status VerifyProof(grpc::ServerContext* context,
                     const VerifyProofRequest* request,
                     VerifyProofResponse* response) override {
    std::lock_guard<std::mutex> lock(mu_);
    try {
      hol_light_.VerifyProof(*request, response);
    } catch (std::exception& e) {
      return Status(grpc::StatusCode::INTERNAL, e.what());
    }
    return Status::OK;
  }
  HolLight hol_light_;
  std::mutex mu_;
};

void RunServer() {
  std::string server_address("0.0.0.0:2000");
  HolLightServiceImpl service;

  grpc::ServerBuilder builder;
  // Listen on the given address without any authentication mechanism.
  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  // Register "service" as the instance through which we'll communicate with
  // clients. In this case it corresponds to an *synchronous* service.
  builder.RegisterService(&service);
  // Finally assemble the server.
  std::unique_ptr<grpc::Server> server(builder.BuildAndStart());

  // Wait for the server to shutdown. Note that some other thread must be
  // responsible for shutting down the server for this call to ever return.
  server->Wait();
}
}  // namespace
}  // namespace n2formal_hol_light

int main(int argc, char** argv) {
  n2formal_hol_light::RunServer();

  return 0;
}
