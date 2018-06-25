#include "rkt2cpp.h"
class t {
public:
  auto operator()() {
    return [=](auto x) { return x(x); }([=](auto factgen) {
      return [=](auto n) {
        if (n == 0) {
          return 1;
        } else {
          return mul(n, factgen(factgen)(sub(n, 1)));
        }
      };
    });
  }
} t;
int main() {
  if (!is_void<decltype(t()(10))>::value)
    cout << t()(10) << '\n';
  return 0;
}
