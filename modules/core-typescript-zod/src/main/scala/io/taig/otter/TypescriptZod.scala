package io.taig.otter

import cats.derived.*
import cats.Order

enum TypescriptZod derives Order:
  case Shared(self: Typescript[TypescriptZod])
  case Split(tpe: Typescript[TypescriptZod], expression: Typescript[TypescriptZod])
  case Type(self: Typescript[TypescriptZod])
  case Expresseion(self: Typescript[TypescriptZod])
