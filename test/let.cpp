#include "rkt2cpp.h"
template <typename T1> class f {
public:
  T1 x;
  f(T1 _t1) : x(_t1) {}
  auto operator()() {
    return [=](auto a) {
      if (x > a) {
        return x;
      } else {
        return sub(0, x);
      }
    }(0);
  }
};
int main() {
  if (!is_void<decltype(f(1)())>::value)
    cout << f(1)() << '\n';
  if (!is_void<decltype(f(-3)())>::value)
    cout << f(-3)() << '\n';
  return 0;
}
