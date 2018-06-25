#include "rkt2cpp.h"
template <typename T1> class square {
public:
  T1 x;
  square(T1 _t1) : x(_t1) {}
  auto operator()() { return mul(x, x); }
};
template <typename T1, typename T2, typename T3> class exptiter {
public:
  T1 b;
  T2 n;
  T3 a;
  exptiter(T1 _t1, T2 _t2, T3 _t3) : b(_t1), n(_t2), a(_t3) {}
  auto operator()() {
    if (n == 0) {
      return a;
    } else {
      if (even(n)) {
        return exptiter(square(b)(), divi(n, 2), a)();
      } else {
        return exptiter(b, sub(n, 1), mul(b, a))();
      }
    }
  }
};
template <typename T1, typename T2> class fastexpt {
public:
  T1 b;
  T2 n;
  fastexpt(T1 _t1, T2 _t2) : b(_t1), n(_t2) {}
  auto operator()() { return exptiter(b, n, 1)(); }
};
int main() {
  if (!is_void<decltype(fastexpt(5, 3)())>::value)
    cout << fastexpt(5, 3)() << '\n';
  return 0;
}
