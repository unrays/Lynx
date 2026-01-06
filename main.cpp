// Copyright (c) January 2026 Félix-Olivier Dumas. All rights reserved.
// Licensed under the terms described in the LICENSE file

#include <iostream>

namespace v4 {
    #define OFF 0
    #define ON 1

    #define GENERATE_HAS_FUNCTION_TRAIT(function_name)                    \
        template <typename T, typename = void>                            \
        struct has_function_##function_name : std::false_type {};         \
                                                                          \
        template <typename T>                                             \
        struct has_function_##function_name<                              \
            T,                                                            \
            std::void_t<decltype(std::declval<T>().function_name())>      \
        > : std::true_type {};                                            \

    /*template<typename T, typename... Args>
    concept HasOnOperated = requires(T t, Args&&... args) {
        { t.onOperated(std::forward<Args>(args)...) };
    };*/

    template<typename T>
    concept HasOnOperated = requires {
        &T::onOperated;
    };

        //faire c++14/17/20

    /********************************************/

    template<int N>
    struct MultiplyBy {
        static int apply(int x) {
            return x * N;
        }

        static auto apply_if_multiple_of_10(int x)
            -> std::enable_if_t<(N % 10 == 0), int>
        {
            return x * N;
        }
    };

    template<int N, typename ...Args>
    constexpr auto n_arguments(Args&&... args) noexcept(false)
        -> std::enable_if_t<(sizeof...(Args) == N), void>
    {
        std::cout << sizeof...(Args) << " arguments\n";
    }

    template<typename T, int N, typename ...Args>
    constexpr auto n_arguments_of_type(Args&&... args) noexcept(false)
        -> std::enable_if_t<
            (sizeof...(Args) == N) &&
            std::conjunction_v<std::is_same<Args, T>...>
        , void>
    {
        std::cout << typeid(T).name() << " type\n";
        std::cout << sizeof...(Args) << " arguments\n";
    }

    /********************************************/

    struct End {
        template<typename FinalState>
        End(FinalState& s) {
            // SEULE ELEMENT RUNTIME DU SYSTÈME COMPLET ;)

            std::cout << "*********** END ***********\n";
            //std::cout << typeid(s).name() << "\n";

            /*std::apply([&](auto&&... args) {
                ((std::cout << args << "\n"), ...);
            }, s);*/

            std::apply([&](auto&&... op_tuples) {

                ((std::apply([&](auto&&... args) {
                    //std::cout << typeid(op_tuples).name() << "\n";
                    ((std::cout << args << "\n"), ...);
                    std::cout << "------\n";
                    }, op_tuples)), ...);
                }, s);

            std::cout << "***************************\n\n";
        }

        void end() {
            std::cout << "Operator chain ended!\n";
        }

        void get() {

        }
    };

    template<typename T>
    struct is_terminal : std::false_type {};

    template<>
    struct is_terminal<End> : std::true_type {};

    /********************************************/

    struct MetaOperator {}; //metaoperator?

    /********************************************/

    struct StatelessOperator : MetaOperator {};

    /********************************************/

    static constexpr std::tuple<> DEFAULT_STATE_VALUE{};

    template<typename CurrentState>
    struct StatefulOperator : MetaOperator {
    public:
        StatefulOperator() = default;
        StatefulOperator(CurrentState s) : state_(std::move(s)) {}

    protected:
        CurrentState state_;
    };

    /********************************************/

    //peut etre faire une macro pour le Arity type

    template<typename Operator>
    struct OperatorTraits : MetaOperator {}; //meta op est boilerplate

    template<
        template<std::size_t, typename, typename> class Operator,
        std::size_t Arity,
        typename Next,
        typename State
    >
    struct OperatorTraits<
        Operator<Arity, Next, State>
    > : MetaOperator
    {
        template<std::size_t T1, typename T2, typename T3>
        using template_type = Operator<T1, T2, T3>;

        static constexpr std::size_t arity = Arity;

        using next_type = Next;

        //fonction inspect() qui return l'accès aux methodes?
    };

    /********************************************/

    /*template<
        template<std::size_t, typename, typename> class DerivedOperator,
        std::size_t Arity,
        typename Next,
        typename State
    >
    struct FunctionOperatorBase<DerivedOperator<Arity, Next, State>> :
        OperatorTraits<DerivedOperator<Arity, Next, State>>,
        StatefulOperator<State>
    {

    };*/

    /********************************************/

    //GENERATE_HAS_FUNCTION_TRAIT(onOperated); //manque params jsp finalement cause variadic

    /********************************************/

    template<typename>
    struct FunctionOperatorBase;

    template<
        template<std::size_t, typename, typename> class DerivedOperator,
        std::size_t Arity,
        typename Next,
        typename State
    >
    struct FunctionOperatorBase<DerivedOperator<Arity, Next, State>> {
        using Derived_t = DerivedOperator<Arity, Next, State>;

        template<typename... Args>
        auto operator()(Args&&... args) 
            -> std::enable_if_t<
                (Arity == 0 || sizeof...(Args) == Arity),
                decltype(std::declval<Derived_t>().onOperated(std::forward<Args>(args)...))
            >
        {
            return static_cast<Derived_t*>(this)
                ->onOperated(std::forward<Args>(args)...);
        }
    };

    template<
        std::size_t Arity,
        typename Next,
        typename CurrentState
    >
    struct FunctionOperator_:
        OperatorTraits<FunctionOperator_<Arity, Next, CurrentState>>,
        FunctionOperatorBase<FunctionOperator_<Arity, Next, CurrentState>>,
        StatefulOperator<CurrentState>
    {
        FunctionOperator_(CurrentState s = DEFAULT_STATE_VALUE)
            : StatefulOperator<CurrentState>(std::move(s)) {}

        template<typename... Args>
        auto onOperated(Args&&... args) noexcept(false) {
            auto concat_state_args = std::tuple_cat(
                this->state_,
                std::make_tuple(
                    std::make_tuple(std::forward<Args>(args)...)
                )
            );

            if constexpr (is_terminal<Next>::value)
                return End{ concat_state_args };
            else
                return Next::template template_type<
                    Next::arity,
                    typename Next::next_type,
                    decltype(concat_state_args)
                > (std::move(concat_state_args));
        }
    };

    /********************************************/

    template<typename>
    struct SubscriptOperatorBase;

    template<
        template<std::size_t, typename, typename> class DerivedOperator,
        std::size_t Arity,
        typename Next,
        typename State
    >
    struct SubscriptOperatorBase<DerivedOperator<Arity, Next, State>> {
        using Derived_t = DerivedOperator<Arity, Next, State>;

        template<typename... Args>
        auto operator[](Args&&... args)
            -> std::enable_if_t<
                (Arity == 0 || sizeof...(Args) == Arity),
                decltype(std::declval<Derived_t>().onOperated(std::forward<Args>(args)...))
            >
        {
            return static_cast<Derived_t*>(this)
                ->onOperated(std::forward<Args>(args)...);
        }

        template<typename T>
        auto operator[](T arg) {
            return static_cast<Derived_t*>(this)
                ->onOperated(std::move(arg));
        }
    };

    template<
        std::size_t Arity,
        typename Next,
        typename CurrentState
    >
    struct SubscriptOperator_ :
        OperatorTraits<SubscriptOperator_<Arity, Next, CurrentState>>,
        SubscriptOperatorBase<SubscriptOperator_<Arity, Next, CurrentState>>,
        StatefulOperator<CurrentState>
    {
        SubscriptOperator_(CurrentState s = DEFAULT_STATE_VALUE)
            : StatefulOperator<CurrentState>(std::move(s)) {}

        //C'EST LUI LE TABARN

        template<typename... Args>
        auto onOperated(Args&&... args) noexcept(false) {
            auto concat_state_args = std::tuple_cat(
                this->state_,
                std::make_tuple(
                    std::make_tuple(std::forward<Args>(args)...)
                )
            );

            if constexpr (is_terminal<Next>::value)
                return End{ concat_state_args };
            else {
                return Next::template template_type<
                    Next::arity,
                    typename Next::next_type,
                    decltype(concat_state_args)
                >(std::move(concat_state_args));
            }
        }

        template<typename T>
        auto onOperated(T arg) noexcept(false) {
            auto concat_state_args = std::tuple_cat(
                this->state_,
                std::make_tuple(
                    std::make_tuple(std::move(arg))
                )
            );

            if constexpr (is_terminal<Next>::value)
                return End{ concat_state_args };
            else {
                return Next::template template_type<
                    Next::arity,
                    typename Next::next_type,
                    decltype(concat_state_args)
                > (std::move(concat_state_args));
            }
        }
    };

    /********************************************/

    static constexpr std::size_t DEFAULT_ARITY_VALUE = 0;
    using DEFAULT_NEXT_TYPE = End;
    using DEFAULT_STATE_TYPE = std::tuple<>;

    /********************************************/

    template<
        std::size_t Arity = DEFAULT_ARITY_VALUE,
        typename Next = DEFAULT_NEXT_TYPE,
        typename State = DEFAULT_STATE_TYPE
    >
    struct FunctionOperator_n : FunctionOperator_<Arity, Next, State> {};

    template<
        typename Next = DEFAULT_NEXT_TYPE,
        typename State = DEFAULT_STATE_TYPE
    >
    struct FunctionOperator : FunctionOperator_<DEFAULT_ARITY_VALUE, Next, State> {};

    /********************************************/

    template<
        std::size_t Arity = DEFAULT_ARITY_VALUE,
        typename Next = DEFAULT_NEXT_TYPE,
        typename State = DEFAULT_STATE_TYPE
    >
    struct SubscriptOperator_n : SubscriptOperator_<Arity, Next, State> {};

    template<
        typename Next = DEFAULT_NEXT_TYPE,
        typename State = DEFAULT_STATE_TYPE
    >
    struct SubscriptOperator : SubscriptOperator_<DEFAULT_ARITY_VALUE, Next, State> {};


    /********************************************/


#if ENABLE_ALIAS
    using DEFAULT_NEXT_TYPE = End;
    using DEFAULT_STATE_TYPE = std::tuple<>;
    //faire same pour filter, juste mettre valeur par def

#define GENERATE_ALIAS(alias_name, backend_name) \
                template<typename N=DEFAULT_NEXT_TYPE, typename S=DEFAULT_STATE_TYPE> \
                using alias_name = backend_name<N,S>;

        //en gros, si le arg1 est un meta operator, on met Operator<0, Next, State>

        //GENERATE_ALIAS(FunctionOperator, FunctionOperator_);
        //GENERATE_ALIAS(SubscriptOperator, SubscriptOperator_);
#endif

    struct Object {};
}


int main() {
    {
        using namespace v6;

        using Pipeline =
            FunctionOperator<
            SubscriptOperator<
            FunctionOperator_n<>
            >>; 
        
        Pipeline pipeline({ std::tuple{} });
        
        pipeline(50.4, 25, 45.3)[3](25.6, "d"); //class Result?
  }
}
