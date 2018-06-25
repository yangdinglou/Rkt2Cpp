#include "rkt2cpp.h"
template <typename T1> class f {
public:
  T1 x;
  f(T1 _t1) : x(_t1) {}
  auto operator()() { return car(x); }
};
template <typename T1> class g {
public:
  T1 x;
  g(T1 _t1) : x(_t1) {}
  auto operator()() { return cdr(x); }
};
int main() {
  if (!is_void<decltype(f(Rkt_Data{1, 2})())>::value)
    cout << f(Rkt_Data{1, 2})() << '\n';
  if (!is_void<decltype(g(Rkt_Data{3, 4})())>::value)
    cout << g(Rkt_Data{3, 4})() << '\n';
  return 0;
}
