package io.taig.otter

enum TypescriptZod:
  case Shared(self: Typescript[TypescriptZod])
  case Split(typescript: Typescript.Value, zod: Typescript.Value)
