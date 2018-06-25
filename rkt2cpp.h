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
#include <initializer_list>
#include <any>
#include <cassert>
using namespace std;

class Symbol
{
private:
    string s;
public:
    Symbol(){};
    Symbol(string ss):s(ss){};
    operator string()
    {
        return s;
    }
};

class Rkt_Data {
public:
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

    Rkt_Data() {
        t = LST;
        endflag=0;
    }

    Rkt_Data(initializer_list<Rkt_Data> lst){
        t=LST;
        for(auto &i:lst)
        {
            container.push_back(Rkt_Data(i));
        }
    }
    Rkt_Data(int i)
    {
        t=INT;
        value=make_any<int>(i);
    }
    Rkt_Data(bool b)
    {
        t=BOOL;
        value=make_any<bool>(b);
    }
    Rkt_Data(char c)
    {
        t=CHAR;
        value=make_any<char>(c);
    }
    Rkt_Data(string s)
    {
        t=STRING;
        value=make_any<string>(s);
    }
    Rkt_Data(Symbol s)
    {
        t=SYMBOL;
        value=make_any<Symbol>(s);
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
            data.value=make_any<Symbol>(Symbol(s));
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
                os<< static_cast<string>(any_cast<Symbol>(data.value))<<' ';
                break;
            }
        }
        return os;
    }
    Rkt_Data car()
    {
        try {
            throw (this->t==LST);
        }catch (bool flag){
            if(!flag){
                cerr<<"Not LST"<<'\n';
                return Rkt_Data();
            } else{
                return container[0];
            }
        }
    }
    Rkt_Data cdr()
    {
        try {
            throw (this->t==LST);
        }catch (bool flag){
            if(!flag){
                cerr<<"Not LST"<<'\n';
                return Rkt_Data();
            }
            else if(this->container.size()<2)
            {
                cerr<<"No cdr"<<'\n';
            }
            else{
                Rkt_Data tmp;
                tmp.container=vector<Rkt_Data>(this->container.begin()+1,this->container.end());
                return tmp;
            }
        }
    }
    operator int()
    {
        assert(t==INT);
        return any_cast<int>(value);
    }
    operator char()
    {
        assert(t==CHAR);
        return any_cast<char>(value);
    }
    operator string()
    {
        assert(t==STRING);
        return any_cast<string>(value);
    }
    operator Symbol()
    {
        assert(t==SYMBOL);
        return any_cast<Symbol>(value);
    }
    operator bool()
    {
        assert(t==BOOL);
        return any_cast<bool>(value);
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
auto divi(Args&&... args) {
    return (... / args );
}


bool even(int n)
{
    return n%2==0;
}

bool odd(int n)
{
    return n%2!=0;
}

#endif //RKT2CPP_H
