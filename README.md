# Oazo

## Overview

This repository contains a custom language called Oazo, developed using Typed Racket. The language can handle various programming constructs, including function applications, conditionals, lambda expressions, and mutable states. It features a type-safe approach with a type-checking mechanism that leverages Typed Racket's advanced type system to ensure correctness throughout the interpretation process.

## Features

- **Typed Abstract Syntax Trees (ASTs)**: Utilizes advanced type definitions (`ExprC`, `TExprC`, `Ty`) to represent the structure of expressions, ensuring type safety and correctness.
- **Recursive Descent Parser**: Implements a parser (`parse` function) that converts S-expressions into typed ASTs, handling constructs such as conditionals, lambda expressions, applications, and sequencing.
- **Type Checking**: Provides a type-checking function (`type-check`) that ensures type consistency for expressions using an environment (`TEnv`) to map symbols to types.
- **Environment-Based Evaluation**: Executes stateful computations with an environment-based interpreter (`interp`), supporting mutable state, function applications, sequencing, closures, and array operations.
- **Mutable State Management**: Implements a store (`Store`) to manage mutable variables and arrays, supporting imperative programming features within a functional language context.
- **Turing Completeness**: The language achieves Turing completeness through support for conditionals, recursion, and mutable state, enabling the expression of any computable function.

## Components

- **AST Definitions**: Different expression types (`NumC`, `IdC`, `AppC`, `LamC`, etc.) are represented as Racket structs, defining the fundamental building blocks of the language.
- **Type Environment (`TEnv`)**: Maps symbols to types, allowing the type-checking function to validate expressions and ensure type correctness.
- **Runtime Environment (`Env`)**: Maps symbols to memory locations, providing the context for evaluating expressions and managing mutable state.
- **Interpreter (`interp`)**: Evaluates expressions by traversing the AST, leveraging environments to handle stateful computations and closures to capture lexical scope.
- **Store (`Store`)**: A list of `Storage` structs that allows mutable state management, including variable assignments and array modifications.

## Usage

### Running the Interpreter

To run the interpreter, you need to have Typed Racket installed. You can execute the interpreter by providing an expression in the custom language, which will then be parsed, type-checked, and evaluated.

## Technical Highlights

- **Recursive Evaluation**: The `interp` function recursively evaluates expressions while maintaining state consistency through environments and stores.
- **Closures for Lexical Scope**: Closures (`ClosV`) are used to capture the lexical environment in which a function is defined, enabling higher-order functions and supporting recursion.
- **Dynamic State Management**: The store (`Store`) enables dynamic state manipulation, allowing the implementation of imperative features such as mutable variables and array updates.
