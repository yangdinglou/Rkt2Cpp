#include "rkt2cpp.h"
class Y {
public:
  auto operator()() {
    return [=](auto h) {
      return [=](auto x) {
        return x(x);
      }([=](auto g) { return h([=](auto t) { return g(g)(t); }); });
    };
  }
} Y;
class fac {
public:
  auto operator()() {
    return Y()([=](auto f) {
      return [=](auto x) {
        if (x < 2) {
          return 1;
        } else {
          return mul(x, f(sub(x, 1)));
        }
      };
    });
  }
} fac;
int main() {
  if (!is_void<decltype(fac()(10))>::value)
    cout << fac()(10) << '\n';
  return 0;
}
