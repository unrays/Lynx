/*********************************************************************
 * EXOTIC.lynx - Header-only C++ compile-time pipeline library
 *
 * Copyright (c) 2026 Félix-Olivier Dumas
 * All rights reserved.
 *
 * Licensed under the Boost Software License, Version 1.0.
 * You may obtain a copy of the License at:
 *     https://www.boost.org/LICENSE_1_0.txt
 *
 * Description:
 *     Provides facilities for building composable, type-safe operator
 *     pipelines entirely at compile time. Intended for constructing
 *     domain-specific languages (DSLs) using template metaprogramming.
 *
 * This file is part of the EXOTIC collection, a set of modern,
 * header-only C++ libraries focused on compile-time abstractions.
 *
 * Note:
 *     This is my first library ever, it might not be perfect but
 *     I'm really really proud of it ;)
 *
 * For more information, visit: https://github.com/unrays/Lynx
 *
 * Author: Félix-Olivier Dumas
 * Version: 1.1.0
 * Last Updated: 17 January 2026
 *********************************************************************/

#ifndef EXOTIC_LYNX_H
#define EXOTIC_LYNX_H

#include <tuple>
#include <type_traits>
#include <utility>
#include <iostream>

namespace EXOTIC {
namespace lynx {
/********************************************/
/*                  DEFINES                 */
/********************************************/
#define OFF 0
#define ON 1

#define ENABLE_ALIAS ON

/********************************************/
/*        GENERATE HAS FUNCTION MACROS      */
/********************************************/
#define LYNX_GENERATE_HAS_FUNCTION_TRAIT(function_name)          \
    template <typename T, typename = void>                       \
    struct has_function_##function_name : std::false_type {};    \
                                                                 \
    template <typename T>                                        \
    struct has_function_##function_name<                         \
        T,                                                       \
        std::void_t<decltype(std::declval<T>().function_name())> \
    > : std::true_type {};

#define LYNX_GENERATE_HAS_FUNCTION_DEF(function_name)                               \
    template<typename, typename = void>                                             \
    struct has_##function_name : std::false_type {};                                \
                                                                                    \
    template<typename T>                                                            \
    struct has_##function_name<                                                     \
        T,                                                                          \
        std::void_t<decltype(&T::function_name)>                                    \
    > : std::true_type {};                                                          \
                                                                                    \
    template<typename, typename = void>                                             \
    struct has_##function_name##_dummy : std::false_type {};                        \
                                                                                    \
    template<typename T>                                                            \
    struct has_##function_name##_dummy<                                             \
        T,                                                                          \
        std::void_t<decltype(std::declval<T>().function_name(std::declval<int>()))> \
    > : std::true_type {};

/********************************************/
/*        HAS FUNCTION INSTANCES            */
/********************************************/
LYNX_GENERATE_HAS_FUNCTION_DEF(onOperated)

/********************************************/
/*               END OPERATORS              */
/********************************************/
template<typename DerivedOperator>
struct EndOperatorBase {
    EndOperatorBase() = default;

    template<typename FinalState>
    EndOperatorBase(FinalState&& s) {
        static_cast<DerivedOperator*>(this)
            ->onOperated(std::forward<FinalState>(s));
    }
};

struct DefaultEndOperator : EndOperatorBase<DefaultEndOperator> {
    using EndOperatorBase<DefaultEndOperator>::EndOperatorBase;

    template<typename State>
    void onOperated(State&& s) {
        std::cout << "*********** Default END Operator ***********\n";
        std::apply([&](auto&&... op_tuples) {
            ((std::apply([&](auto&&... args) {
                ((std::cout << args << "\n"), ...);
                std::cout << "------\n";
                }, op_tuples)), ...);
            }, s);
        std::cout << "********************************************\n\n";
    }
};

struct EmptyEndOperator : EndOperatorBase<EmptyEndOperator> {
    using EndOperatorBase<EmptyEndOperator>::EndOperatorBase;
};

/********************************************/
/*               TYPE TRAITS                */
/********************************************/
template<typename T>
struct is_end_operator : std::is_base_of<EndOperatorBase<T>, T> {};

/********************************************/
/*              OPERATOR BASES              */
/********************************************/
struct StatelessOperator {};

static constexpr std::tuple<> DEFAULT_STATE_VALUE{};

template<typename CurrentState>
struct StatefulOperator {
public:
    StatefulOperator() = default;
    StatefulOperator(CurrentState s) : state_(std::move(s)) {}

protected:
    CurrentState state_;
};

/********************************************/
/*               OPERATOR TRAITS            */
/********************************************/
template<typename Operator>
struct OperatorTraits {};

template<
    template<std::size_t, typename, typename> class Operator,
    std::size_t Arity,
    typename Next,
    typename State
>
struct OperatorTraits<Operator<Arity, Next, State>> {
    template<std::size_t T1, typename T2, typename T3>
    using template_type = Operator<T1, T2, T3>;

    static constexpr std::size_t arity = Arity;
    using next_type = Next;
};

/********************************************/
/*               PIPE OPERATOR              */
/********************************************/
template<typename>
struct PipeOperatorBase;

template<
    template<std::size_t, typename, typename> class DerivedOperator,
    std::size_t Arity,
    typename Next,
    typename State
>
struct PipeOperatorBase<DerivedOperator<Arity, Next, State>> {
    using Derived_t = DerivedOperator<Arity, Next, State>;

    template<typename T>
    auto operator|(T&& other)
        -> decltype(std::declval<Derived_t>().onOperated(std::declval<T>()))
    {
        return static_cast<Derived_t*>(this)
            ->onOperated(std::forward<T>(other));
    }
};

/********************************************/
/*             PIPE OPERATOR IMPL           */
/********************************************/
template<
    std::size_t Arity,
    typename Next,
    typename CurrentState
>
struct PipeOperator_:
    OperatorTraits<PipeOperator_<Arity, Next, CurrentState>>,
    PipeOperatorBase<PipeOperator_<Arity, Next, CurrentState>>,
    StatefulOperator<CurrentState>
{
    using StatefulOperator<CurrentState>::StatefulOperator;
    friend PipeOperatorBase<PipeOperator_<Arity, Next, CurrentState>>;

private:
    template<typename T>
    auto onOperated(T&& arg) {
        auto concat_state_args = std::tuple_cat(
            this->state_,
            std::make_tuple(std::make_tuple(std::forward<T>(arg)))
        );

        if constexpr (is_end_operator<Next>::value) {
            if constexpr (has_onOperated_dummy<Next>::value)
                return Next{ concat_state_args };
            else
                return concat_state_args;
        }
        else
            return Next::template template_type<
                Next::arity,
                typename Next::next_type,
                decltype(concat_state_args)
            > (std::move(concat_state_args));
    }
};

/********************************************/
/*          FUNCTION OPERATOR BASE          */
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

/********************************************/
/*          FUNCTION OPERATOR IMPL          */
/********************************************/
template<
    std::size_t Arity,
    typename Next,
    typename CurrentState
>
struct FunctionOperator_ :
    OperatorTraits<FunctionOperator_<Arity, Next, CurrentState>>,
    FunctionOperatorBase<FunctionOperator_<Arity, Next, CurrentState>>,
    StatefulOperator<CurrentState>
{
    using StatefulOperator<CurrentState>::StatefulOperator;
    friend FunctionOperatorBase<FunctionOperator_<Arity, Next, CurrentState>>;

private:
    template<typename... Args>
    auto onOperated(Args&&... args) {
        auto concat_state_args = std::tuple_cat(
            this->state_,
            std::make_tuple(std::make_tuple(std::forward<Args>(args)...))
        );

        if constexpr (is_end_operator<Next>::value) {
            if constexpr (has_onOperated_dummy<Next>::value)
                return Next{ concat_state_args };
            else
                return concat_state_args;
        }
        else
            return Next::template template_type<
                Next::arity,
                typename Next::next_type,
                decltype(concat_state_args)
            > (std::move(concat_state_args));
    }
};

/********************************************/
/*          SUBSCRIPT OPERATOR BASE         */
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

/********************************************/
/*         SUBSCRIPT OPERATOR IMPL          */
/********************************************/
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
    using StatefulOperator<CurrentState>::StatefulOperator;
    friend SubscriptOperatorBase<SubscriptOperator_<Arity, Next, CurrentState>>;

private:
#if __cplusplus >= 202302L
    template<typename... Args>
    auto onOperated(Args&&... args) {
        auto concat_state_args = std::tuple_cat(
            this->state_,
            std::make_tuple(std::make_tuple(std::forward<Args>(args)...))
        );

        if constexpr (is_end_operator<Next>::value) {
            if constexpr (has_onOperated_dummy<Next>::value)
                return Next{ concat_state_args };
            else
                return concat_state_args;
        }
        else
            return Next::template template_type<
                Next::arity,
                typename Next::next_type,
                decltype(concat_state_args)
            > (std::move(concat_state_args));
    }
#endif

private:
    template<typename T>
    auto onOperated(T arg) {
        auto concat_state_args = std::tuple_cat(
            this->state_,
            std::make_tuple(std::make_tuple(std::move(arg)))
        );

        if constexpr (is_end_operator<Next>::value) {
            if constexpr (has_onOperated_dummy<Next>::value)
                return Next{ concat_state_args };
            else
                return concat_state_args;
        }
        else
            return Next::template template_type<
                Next::arity,
                typename Next::next_type,
                decltype(concat_state_args)
            > (std::move(concat_state_args));
    }
};

/********************************************/
/*          OPERATOR ALIAS MACROS           */
/********************************************/
#if ENABLE_ALIAS
    static constexpr std::size_t DEFAULT_ARITY_VALUE = 0;
    using DEFAULT_NEXT_TYPE = EmptyEndOperator;
    using DEFAULT_STATE_TYPE = std::tuple<>;

    #define LYNX_GENERATE_OPERATOR_ALIAS(alias_name, backend_name)              \
        template<                                                               \
            std::size_t Arity = DEFAULT_ARITY_VALUE,                            \
            typename Next = DEFAULT_NEXT_TYPE,                                  \
            typename State = DEFAULT_STATE_TYPE                                 \
        >                                                                       \
        struct alias_name##_n : backend_name<Arity, Next, State> {};            \
                                                                                \
        template<                                                               \
            typename Next = DEFAULT_NEXT_TYPE,                                  \
            typename State = DEFAULT_STATE_TYPE                                 \
        >                                                                       \
        struct alias_name : backend_name<DEFAULT_ARITY_VALUE, Next, State> {};
#endif

inline namespace alias {
    LYNX_GENERATE_OPERATOR_ALIAS(FunctionOperator, FunctionOperator_);
    LYNX_GENERATE_OPERATOR_ALIAS(SubscriptOperator, SubscriptOperator_);
    LYNX_GENERATE_OPERATOR_ALIAS(PipeOperator, PipeOperator_);
}
} // namespace EXOTIC::lynx
} // namespace EXOTIC

#endif // EXOTIC_LYNX_H
