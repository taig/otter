package io.taig.otter

import cats.Order
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.derived.*
import cats.derived
import cats.syntax.all.*
import cats.Traverse

enum Typescript[+A] derives Order, Traverse:
  case Any
  case Array(self: A)
  case Boolean
  case Dynamic(value: String)
  case Enumeration(values: NonEmptyChain[String])
  case Literal(value: String)
  case Nullable(self: A)
  case Number
  case Object(fields: Chain[(String, A)])
  case Record(key: A, value: A)
  case Recursive(self: A)
  case Reference(name: String)
  case String
  case Tuple(values: Chain[A])
  case Union(values: NonEmptyChain[A])
  case Void
  

object Typescript:
  final case class Value(self: Typescript[Value]) extends AnyVal:
    def containsRecursive: Boolean = self match
      case Array(self) => self.containsRecursive
      case Nullable(self) => self.containsRecursive
      case Object(fields) => fields.exists((_, value) => value.containsRecursive)
      case Record(key, value) => key.containsRecursive || value.containsRecursive
      case Recursive(_) => true
      case Tuple(values) => values.exists(_.containsRecursive)
      case Union(values) => values.exists(_.containsRecursive)
      case _ => false
    

  object Value:
    given Order[Typescript.Value] with
      override def compare(x: Value, y: Value): Int = x.self.compare(y.self)
