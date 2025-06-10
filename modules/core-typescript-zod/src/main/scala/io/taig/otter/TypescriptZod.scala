package io.taig.otter

import cats.derived.*
import cats.Order
import cats.syntax.all.*

enum TypescriptZod derives Order:
  case Shared(self: Typescript[TypescriptZod])
  case Split(typescript: Typescript.Value, zod: Typescript.Value)

  def toTypescript: Typescript.Value = this match
    case Shared(self)         => Typescript.Value(self.map(_.toTypescript))
    case Split(typescript, _) => typescript

  def toZod: Typescript.Value = this match
    case Shared(self)  => Typescript.Value(self.map(_.toZod))
    case Split(_, zod) => zod

object TypescriptZod:
  def apply(typescript: Typescript[Typescript.Value]): TypescriptZod =
    Shared(typescript.map(value => TypescriptZod(value.self)))
