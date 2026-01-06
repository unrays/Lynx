# Operator

> Operator is a C++ compile-time meta programming library for building composable operator pipelines. This library is intended for developers wishing to create their own DSL using ultra-high-performance, fully compile-time and type-safe operator pipelines.

---

## Table of Contents
1. [Motivation](#motivation)
2. [Installation](#installation)
    - [Requirements](#requirements)
    - [Include](#include)
3. [Usage](#usage)
    - [Step 1: Create a pipeline](#step-1-create-a-pipeline)
    - [Step 2: Execute the pipeline](#step-2-execute-the-pipeline)
    - [Step 3: End the pipeline](#step-3-end-the-pipeline)
4. [Examples](#examples)
    - [Example 1: Specify the size of arguments](#example-1-specify-the-size-of-arguments)
    - [Example 2: Implement your own operator](#example-2-implement-your-own-operator)
        - [Base provided by the API](#base-provided-by-the-api)
        - [Implementation using this base](#implementation-using-this-base)
5. [Contributing](#contributing)
6. [Roadmap](#roadmap)
7. [License](#license)


---

## Motivation
Originally, my plan was to implement a fluent design in my ECS to clarify the interactions with my personal ECS. I then got to work and began developing a small, entirely compile-time system. Gradually, this small side project transformed into a full-fledged project, which itself evolved into a library. The main problem with chaining compile-time operators is that it's difficult to extract information from the next node and adapt that node to the conditions of the previous one if everything is compiled-time. The solution, using template and `using` statements, allows me to model and recreate subsequent nodes from scratch with their original attributes, but adding the types and information of the current node, such as the data container or size constraints, for example.

---

## Installation
Instructions on how to install, include, or build the library.

### Requirements
- **C++ Standard:** C++11 up to C++26
- **Compilers:** GCC 11+, Clang 13+, MSVC 2019+ (any compiler supporting C++11 to C++26).  
- **Dependencies:** Only the C++ standard library (`<tuple>`, `<type_traits>`, `<concepts>`, `<utility>`, `<iostream>`). No external dependencies.

### Include
This library is **header-only**, so you just need to include the main header in your project:

```cpp
#include "operator.h"
using namespace v6;
```

---


## Usage

Basic usage of the library involves creating operator chains and terminating them with `Result` or your own implementation.

### Step 1: Create a pipeline
```cpp
#include "operator.h"
using namespace v6;

// Create a FunctionOperator pipeline
auto pipeline = FunctionOperator<SubscriptOperator<>>{};

// Equivalent explicit template version specifying arity, state, and next operator
auto pipeline = FunctionOperator<0, std::tuple<>, SubscriptOperator<0, std::tuple<>, End>>{};
```

### Step 2: Execute the pipeline
```cpp
#include "operator.h"
using namespace v6;

// Provide some arguments; the pipeline collects them internally
pipeline(0, 250, 500)[750, 1000];
```

### Step 3: End the pipeline
```cpp
#include "operator.h"
using namespace v6;

// Automatically terminates when pipeline reaches End
auto final_state = pipeline(10, 20)[30, 40, 50]; // returns collected arguments
```

---

## Examples

### Example 1: Specify the size of arguments

```cpp
#include "operator.h"
using namespace v6;

auto pipeline = SubscriptOperator<3,
                    FunctionOperator<5,
                        SubscriptOperator<> // 0 = no constraints by default
                    >
                >{};

pipeline[0, 10, 20](30, 40, 50, 60, 70)[80]; // Compiles

pipeline[0, 10](20, 30, 40)[50]; // Doesn't compile
```

---

### Example 2: Implement your own operator

#### Base provided by the API

```cpp
#include "operator.h"
using namespace v6;

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
  ```

---

#### Implementation using this base

```cpp
#include "operator.h"
using namespace v6;

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
```

## Contributing

Contributions are welcome! You can help by:

- Reporting bugs or issues
- Suggesting new features or improvements
- Submitting pull requests with fixes or new functionality

Please follow these guidelines:

1. Fork the repository
2. Create a new branch for your feature or bug fix
3. Make your changes and write tests if applicable
4. Submit a pull request describing your changes

---

## Roadmap

Planned features and improvements for future releases:

- [ ] Add support for additional operator types
- [ ] Improve compile-time diagnostics and error messages
- [ ] Extend examples and documentation

This roadmap may evolve as the library grows.

---

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

<p align="center"><sub>© Félix-Olivier Dumas 2026</sub></p>
