#include "rkt2cpp.h"
template <typename T1, typename T2> class f {
public:
  T1 a;
  T2 b;
  f(T1 _t1, T2 _t2) : a(_t1), b(_t2) {}
  auto operator()() {
    if ((a == b) ? (a > 0) : (false)) {
      a = sum(a, 1);
      return a;
    } else {
      b = sub(b, 1);
      return b;
    }
  }
};
int main() {
  if (!is_void<decltype(f(0, 0)())>::value)
    cout << f(0, 0)() << '\n';
  if (!is_void<decltype(f(1, 0)())>::value)
    cout << f(1, 0)() << '\n';
  if (!is_void<decltype(f(1, 1)())>::value)
    cout << f(1, 1)() << '\n';
  if (!is_void<decltype(f(0, 1)())>::value)
    cout << f(0, 1)() << '\n';
  return 0;
}
