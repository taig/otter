package io.taig.otter

import cats.derived.*
import cats.Order
import cats.syntax.all.*

enum TypescriptZod derives Order:
  case Shared(self: Typescript[TypescriptZod])
  case Split(typescript: Typescript[Typescript.Value], zod: Typescript[Typescript.Value])

  def toTypescript: Typescript[Typescript.Value] = this match
    case Shared(self)         => self.map(self => Typescript.Value(self.toTypescript))
    case Split(typescript, _) => typescript

  def toZod: Typescript[Typescript.Value] = this match
    case Shared(self)  => self.map(self => Typescript.Value(self.toZod))
    case Split(_, zod) => zod

object TypescriptZod:
  def apply(typescript: Typescript[Typescript.Value]): TypescriptZod =
    Shared(typescript.map(value => TypescriptZod(value.self)))