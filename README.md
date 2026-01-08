# Linkly

> Linkly is a C++ compile-time meta programming library for building composable operator pipelines.  
> This library is intended for developers wishing to create their own DSL using ultra-high-performance, fully compile-time and type-safe operator pipelines.

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
Originally, my plan was to implement a fluent design system for the front end of my ECS game engine. I started a week ago and I had never dealt with this kind of concept before.  I then got to work and began developing a small, entirely compile-time system. Gradually, this small side project transformed into a full-fledged project, which itself evolved into a library.  

The main problem with chaining compile-time elements is that, since these elements are compiled before being received in the previous node, it is not directly possible to interact with or modify its templated types.
The solution, using template and `using` statements, allows me to model and recreate `subsequent nodes` from scratch with their original attributes, but adding the types and information of the current node, such as the data container or size constraints, for example.

This project is the culmination of my journey learning metaprogramming in C++, which I naively began a little less than two months ago. This is my first library ever, and I'm sure there are many things that can be improved. Feel free to share your suggestions!

---

## Installation

Instructions on how to install, include, or build the library.

### Requirements
- **C++ Standard:** C++11 up to C++26  
- **Compilers:** GCC 11+, Clang 13+, MSVC 2019+ (any compiler supporting C++11 to C++26)  
- **Dependencies:** Only the C++ standard library (`<tuple>`, `<type_traits>`, `<concepts>`, `<utility>`, `<iostream>`). No external dependencies.

### Include
This library is **header-only**, so you just need to include the main header in your project:

```cpp
#include "linkly.hpp"
using namespace linkly;
````

---

## Usage

Basic usage of the library involves creating operator chains and terminating them with `Result` or your own implementation.

### Step 1: Create a pipeline

```cpp
#include "linkly.hpp"
using namespace linkly;

// Create a FunctionOperator pipeline
auto pipeline = FunctionOperator<SubscriptOperator<>>{};

// Equivalent explicit template version specifying arity, state, and next operator
auto pipeline = FunctionOperator<0, std::tuple<>, SubscriptOperator<0, std::tuple<>, DefaultEndOperator>>{};
```

### Step 2: Execute the pipeline

```cpp
#include "linkly.hpp"
using namespace linkly;

// Provide some arguments; the pipeline collects them internally
pipeline(0, 250, 500)[750, 1000];
```

### Step 3: End the pipeline

```cpp
#include "linkly.hpp"
using namespace linkly;

// Automatically terminates when pipeline reaches End
auto final_state = pipeline(10, 20)[30, 40, 50]; // returns collected arguments (tuple by default)
```

---

## Examples

### Example 1: Specify the size of arguments

```cpp
#include "linkly.hpp"
using namespace linkly;

auto pipeline = SubscriptOperator<3,
                    FunctionOperator<5,
                        SubscriptOperator<> // 0 = no constraints by default
                    >
                >{};

pipeline[0, 10, 20](30, 40, 50, 60, 70)[80]; // Compiles

pipeline[0, 10](20, 30, 40)[50]; // Doesn't compile
```


### Example 2: Implement your own operator

#### Base provided by the API

```cpp
#include "linkly.hpp"
using namespace linkly;

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

#### Example of implementation using this base

```cpp
#include "linkly.hpp"
using namespace linkly;

template<
    std::size_t Arity, // Number of arguments required
    typename Next, // Represents the subsequent structure type
    typename CurrentState // Type of the stored state
>
struct EntityIndexerOperator_:
    OperatorTraits<EntityIndexerOperator_<Arity, Next, CurrentState>>, // Allows for type introspection 
    SubscriptOperatorBase<EntityIndexerOperator_<Arity, Next, CurrentState>>, // Crtp base for generic operator attributes
    StatefulOperator<CurrentState> // Allows support for State by providing constructor, storage, etc...
{
    using StatefulOperator<CurrentState>::StatefulOperator; // Using the base's constructor
    friend SubscriptOperatorBase<EntityIndexerOperator_<Arity, Next, CurrentState>>; // Allows private implementation for onOperated

private:
    template<typename... Args>
    auto onOperated(Args&&... args) { // hooked function called when the base operator is called.

        // Adds the arguments to the current state using std::tuple_cat()
        auto concat_state_args = std::tuple_cat(
            this->state_,
            std::make_tuple(std::make_tuple(std::forward<Args>(args)...))
        );

        // Checks if the next node is normal or terminal
        if constexpr (is_end_operator<Next>::value) {
            if constexpr (has_onOperated_dummy<Next>::value) // Checks if terminal implements onOperated()
                return Next{ concat_state_args }; // If so, returns the terminal node with the current state 
            else
                return concat_state_args; // Otherwise, returns directly the state as it's raw type (tuple in this case) 
        }
        else
            return Next::template template_type<
                sizeof...(args), // Limits the next node to accepting the same number of arguments
                                 // In this case, we want the number of components to be equal to the number of entities

                typename Next::next_type, // Uses the same typename Next as normal, no change here


                decltype(concat_state_args) // Resolves the type of the current state and sends it
                                            // This allows the next node to store the tuple as a member

            > (std::move(concat_state_args)); // std::move the current state and passes it to the next node via its constructor
    }
};

```

---

## Contributing

Contributions are welcome! You can help by:

* Reporting bugs or issues
* Suggesting new features or improvements
* Submitting pull requests with fixes or new functionality

Please follow these guidelines:

1. Fork the repository
2. Create a new branch for your feature or bug fix
3. Make your changes and write tests if applicable
4. Submit a pull request describing your changes

---

## Roadmap

Planned features and improvements for future releases:

* [ ] Add support for additional operator types
* [ ] Improve compile-time diagnostics and error messages
* [ ] Extend examples and documentation
* [ ] Fix SFINAE to support C++17 and prior

> This roadmap may evolve as the library grows.

---

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

<p align="center"><sub>© Félix-Olivier Dumas 2026</sub></p>
