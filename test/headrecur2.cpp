#include "rkt2cpp.h"
template <typename T1> class f {
public:
  T1 x;
  f(T1 _t1) : x(_t1) {}
  auto operator()() {
    if (x == 1) {
      return 1;
    } else {
      return mul(x, f(sub(x, 1))());
    }
  }
};
int main() {
  if (!is_void<decltype(f(10)())>::value)
    cout << f(10)() << '\n';
  return 0;
}
