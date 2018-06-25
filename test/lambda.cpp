#include "rkt2cpp.h"
class Y {
public:
  auto operator()() {
    return [=](auto b) {
      return [=](auto f) {
        return b([=](auto x) { return f(f)(x); });
      }([=](auto f) { return b([=](auto x) { return f(f)(x); }); });
    };
  }
} Y;
class F {
public:
  auto operator()() {
    return [=](auto f) {
      return [=](auto n) {
        if (n == 0) {
          return 1;
        } else {
          return mul(n, f(sub(n, 1)));
        }
      };
    };
  }
} F;
int main() {
  if (!is_void<decltype(Y()(F())(10))>::value)
    cout << Y()(F())(10) << '\n';
  return 0;
}
