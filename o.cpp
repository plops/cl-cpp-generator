#include "org_types.h"
#include <complex>
namespace N {
class CommandsHandler {
public:
  CommandsHandler(const DeviceCallbacks callbacks);
  void HandleRxBlock(const uint16_t data);

}

void HandleRxBlock(const uint16_t data){
  uint16_t a = 3;
  uint16_t b;
  ;
  a += data;
}
};
