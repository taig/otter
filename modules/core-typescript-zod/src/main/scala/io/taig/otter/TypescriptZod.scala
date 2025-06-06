package io.taig.otter

import cats.derived.*
import cats.Order

enum TypescriptZod derives Order:
  case Shared(self: Typescript[TypescriptZod])
  case Split(typescript: Typescript[TypescriptZod], zod: Typescript[TypescriptZod])
  case Type(self: Typescript[TypescriptZod])
  case Expression(self: Typescript[TypescriptZod])
