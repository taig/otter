package io.taig.otter

import cats.data.Chain
import io.taig.otter.codec.TypescriptPrinter
import cats.Show

enum Typescript:
  case Array(self: Typescript)
  case Boolean
  case Nullable(self: Typescript)
  case Number
  case Object(fields: Chain[(String, Typescript)])
  case Reference(name: String)
  case String
  case Union(left: Typescript, right: Typescript)

  override def toString: String = TypescriptPrinter.print(this)

object Typescript:
  given Show[Typescript] = Show.fromToString
