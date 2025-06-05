package io.taig.otter

import cats.derived.*
import cats.Order

enum TypescriptZod derives Order:
  case Shared(self: Typescript[TypescriptZod])
  case Split(typescript: Typescript.Value, zod: Typescript.Value)
