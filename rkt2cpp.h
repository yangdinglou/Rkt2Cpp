#ifndef RKT2CPP_H
#define RKT2CPP_H

#include <any>
#include <vector>
#include <iostream>
#include <type_traits>
using namespace std;

class Rkt_Data {
private:
    enum type {
        LST,
        INT,
        BOOL,
        CHAR,
        STRING,
        SYMBOL
    } t;
    std::vector<Rkt_Data> container;
    std::any value;
public:
    Rkt_Data() {
        t = LST;
    }

    friend std::istream &operator>>(std::istream &is, Rkt_Data &data) {
        char flag;

        if (flag == '(') {
            flag = is.get();
            Rkt_Data tmp = Rkt_Data();
            while ((flag = is.peek())&&(flag)) {
                is >> tmp;

            }
        }
    }
};

template<typename ...Args>
auto sum(Args&&... args) {
    return (... + args );
}

template<typename ...Args>
auto sub(Args&&... args) {
    return (... - args );
}

template<typename ...Args>
auto mul(Args&&... args) {
    return (... * args );
}

template<typename ...Args>
auto div(Args&&... args) {
    return (... / args );
}
#endif //RKT2CPP_H
