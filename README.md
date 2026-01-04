I'll probably add something here, but know that I worked incredibly hard on this little bit of code; in fact, I had to start from scratch at least seven times. I finally found the solution to all my problems: recreating the `next` statement of the chain using `using`, which allows me to make my links dynamic while maintaining 100% compile time. ;) I'm very proud of this result! Maybe I'll work on embellishing it as a library...?! Who knows!

*Also, this is my first project of 2026!*

I've learned so much since I started meta-programming just a month and three weeks ago! I'm really starting to see significant progress and I'm also beginning to tackle challenges that I'm increasingly passionate about! I can't wait to see what the future holds ;)

*This is definitely not finished yet, I still have many more ideas to implement.*

---

#### Output
```console
*********** END ***********
class std::tuple<double,int,double>
50
25
45.3
------
class std::tuple<int>
3
------
class std::tuple<double,char const * __ptr64>
25.6
d
------
***************************
```

#### Main
```cpp
int main() {
  using Pipeline =
    FunctionOperator<
      SubscriptOperator<
        FunctionOperator<>
  >>;
  
  Pipeline pipeline({});
  
  pipeline(50, 25, 45.3)[3](25.6, "d");
}
```

#### Code
```cpp
// Copyright (c) January 2026 Félix-Olivier Dumas. All rights reserved.
// Licensed under the terms described in the LICENSE file

#define OFF 0
#define ON 1

#define ENABLE_ALIAS ON

struct End {
    template<typename FinalState>
    End(FinalState& s) {
        std::cout << "*********** END ***********\n";
        std::apply([&](auto&&... op_tuples) {
            ((std::apply([&](auto&&... args) {
                ((std::cout << args << "\n"), ...);
                std::cout << "------\n";
            }, op_tuples)), ...);
        }, s);
        std::cout << "***************************\n\n";
    }

    void end() {
        std::cout << "Operator chain ended!\n";
    }
};

/********************************************/

template<typename T>
struct is_terminal : std::false_type {};

template<>
struct is_terminal<End> : std::true_type {};

/********************************************/

struct StatelessOperator;

template<typename CurrentState>
struct StatefulOperator {
public:
    StatefulOperator() = default;
    StatefulOperator(CurrentState s) : state_(std::move(s)) {}

protected:
    CurrentState state_;
};

/********************************************/

template<typename Operator>
struct MetaOperator;

template<
    template<typename, typename> class Operator,
    typename Next,
    typename State
>
struct MetaOperator<Operator<Next, State>> {
    template<typename T1, typename T2>
    using template_type = Operator<T1, T2>;
    using next_type = Next;
};

/********************************************/

template<
    typename Next,
    typename CurrentState
>
struct FunctionOperator_:
    StatefulOperator<CurrentState>,
    MetaOperator<FunctionOperator_<Next, CurrentState>>
{
    template<typename... Args>
    auto operator()(Args&&... args) {
        auto t_ = std::make_tuple(std::forward<Args>(args)...);

        auto concat_state_args = std::tuple_cat(
            this->state_,
            std::make_tuple(t_)
        );

        if constexpr (is_terminal<Next>::value)
            return End{ concat_state_args };
        else
            return Next::template template_type<
              typename Next::next_type,
              decltype(concat_state_args)
            > (std::move(concat_state_args));
    }
};

template<
    typename Next,
    typename CurrentState
>
struct SubscriptOperator_:
    StatefulOperator<CurrentState>,
    MetaOperator<SubscriptOperator_<Next, CurrentState>>
{
    template<typename Arg>
    auto operator[](Arg arg) {
        auto t_ = std::make_tuple(std::move(arg));

        auto concat_state_args = std::tuple_cat(
            this->state_,
            std::make_tuple(t_)
        );

        if constexpr (is_terminal<Next>::value)
            return End{ concat_state_args };
        else 
            return Next::template template_type<
                typename Next::next_type,
                decltype(concat_state_args)
            > (std::move(concat_state_args));
    }
};

#if ENABLE_ALIAS
    using DEFAULT_NEXT_TYPE = End;
    using DEFAULT_STATE_TYPE = std::tuple<>;

    #define GENERATE_ALIAS(alias_name, backend_name) \
        template<typename N=DEFAULT_NEXT_TYPE, typename S=DEFAULT_STATE_TYPE> \
        using alias_name = backend_name<N,S>;

    GENERATE_ALIAS(FunctionOperator, FunctionOperator_);
    GENERATE_ALIAS(SubscriptOperator, SubscriptOperator_);
#endif
```
