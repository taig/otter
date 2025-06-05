package io.taig.otter

type TypescriptZodState[A] = ContextState[TypescriptZodState.Reference, A]

object TypescriptZodState:
  enum Reference:
    case Shared(self: Typescript.Value)
    case Type(self: Typescript.Value)
    case Expression(self: Typescript.Value)
