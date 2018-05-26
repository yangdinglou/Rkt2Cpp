//
// Created by ydl on 18-5-21.
//

#ifndef RKT2CPP_H
#define RKT2CPP_H

#include <any>
#include <vector>
#include <iostream>
#include <type_traits>
#include <algorithm>
#include <any>
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
    bool endflag;
public:
    Rkt_Data() {
        t = LST;
        endflag=0;
    }

    friend std::istream &operator>>(std::istream &is, Rkt_Data &data) {
        char flag=is.peek();
        while ((flag=='\n')||(flag==' '))
        {
            is.get();
            flag=is.peek();
        }
        if (flag == '(') {
            Rkt_Data tmp=Rkt_Data();
            is.get();
            while (1) {
                tmp=Rkt_Data();
                is >> tmp;
                //if(!tmp.container.empty())tmp.t=LST;
                data.container.push_back(tmp);
                if(tmp.endflag)
                {
                    is.get();
                    break;
                }
            }

        }
        else if(flag>'0'&&flag<'9'){
            data.t=INT;
            int tmp=int();
            is>>tmp;
            data.value=make_any<int>(tmp);
        }
        else if(flag=='#'){
            flag=is.get();

            switch (flag)
            {
                case 't':
                {
                    data.t=BOOL;
                    data.value=make_any<bool>(true);
                    break;
                }
                case 'f':
                {
                    data.t=BOOL;
                    data.value=make_any<bool>(false);
                    break;
                }
                case '\\':
                {
                    flag=is.get();
                    data.t=CHAR;
                    data.value=make_any<char>(flag);
                    break;
                }

            }
        }
        else if(flag=='"')
        {
            is.get();
            string s;
            getline(is,s,'"');
            data.t=STRING;
            data.value=make_any<string>(s);

        } else{
            char tmp=is.get();
            string s;
            s+=tmp;
            tmp=is.peek();
            while (tmp!=' '&&tmp!=')'&&tmp!='\n')
            {
                s+=tmp;
                is.get();
                tmp=is.peek();
            }
            data.t=SYMBOL;
            data.value=make_any<string>(s);
        }
        flag=is.peek();
        while ((flag=='\n')||(flag==' '))
        {
            is.get();
            if(is.eof())puts("0");
            flag=is.peek();
        }
        if(flag==')')
        {
            data.endflag=1;
        }
        return is;
    }
    friend std::ostream &operator<<(std::ostream &os,const Rkt_Data &data) {
        switch (data.t){
            case LST:
            {
                os<<'(';
                for(auto &i:data.container)
                {
                    os<<i<<' ';
                }
                os<<')';
                break;
            }
            case INT:
            {
                os<<any_cast<int>(data.value)<<' ';
                break;
            }
            case BOOL:
            {
                switch (any_cast<bool>(data.value))
                {
                    case false:
                    {
                        os<<"#f"<<" ";
                        break;
                    }
                    case true:
                    {
                        os<<"#t"<<" ";
                        break;
                    }
                }
            }
            case CHAR:
            {
                os<<"#\\"<<any_cast<char>(data.value)<<' ';
                break;
            }
            case STRING:
            {
                os<<'\"'<<any_cast<string>(data.value)<<"\" ";
                break;
            }
            case SYMBOL:
            {
                os<<any_cast<string>(data.value)<<' ';
                break;
            }
        }
        return os;
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
