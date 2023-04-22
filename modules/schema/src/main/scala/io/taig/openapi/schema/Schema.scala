package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import cats.{Eq, Eval, Semigroup}
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.validation.syntax.*
import io.taig.validation.{identifiers, Constraint, Validation, Violation}

import scala.Tuple.Append
import scala.deriving.*
import scala.util.matching.Regex

abstract class Schema[A]:
  self =>
  type Self[a] <: Schema[a] { type Self[a] = self.Self[a] }
  type Codec <: OpenApi

  def constraints: Chain[Constraint[OpenApi]]

  def description: Option[String]
  final def modifyDescription(f: Option[String] => Option[String]): Self[A] = copy(f(description), example, name)
  final def setDescription(description: Option[String]): Self[A] = self.modifyDescription(_ => description)
  final def withDescription(description: String): Self[A] = setDescription(description.some)
  final def withoutDescription: Self[A] = setDescription(none)

  def example: Option[A]
  final def modifyExample(f: Option[A] => Option[A]): Self[A] = copy(description, f(example), name)
  final def setExample(example: Option[A]): Self[A] = self.modifyExample(_ => example)
  final def withExample(example: A): Self[A] = setExample(example.some)
  final def withoutExample(example: A): Self[A] = setExample(none)

  def name: Option[String]
  final def modifyName(f: Option[String] => Option[String]): Self[A] = copy(description, example, f(name))
  final def setName(name: Option[String]): Self[A] = self.modifyName(_ => name)
  final def withName(name: String): Self[A] = setName(name.some)
  final def withoutName: Self[A] = setName(none)

  final def const(value: => A): Schema[Unit] = imap(_ => ())(_ => value)

  final def optional: Optional.Of[Option[A], Codec] = Optional(this)

  def copy(description: Option[String], example: Option[A], name: Option[String]): Self[A] { type Codec = self.Codec }

  def imap[B](f: A => B)(g: B => A): Self[B] { type Codec = self.Codec }
  def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Self[C] { type Codec = self.Codec }

  final def validate(validation: Validation[OpenApi, A, A, Unit]): Self[A] { type Codec = self.Codec } =
    ivalidate(validation.tap)(identity)

  def decode(openapi: OpenApi): Validated[Violations, A]
  def encode(a: A): Codec

//  def scoped[B](path: Path, schema: Schema[B]): Self[(A, B)] = ???
//    override def decode(openapi: OpenApi): Validated[Violations, (A, B)] = Schema.this.decode(openapi).andThen { a =>
//      openapi.asObject.flatMap(_.get("")) match
//        case Some(value) => schema.decode(value).map(b => (a, b))
//        case None => ???
//    }

object Schema:
  type Of[A, B <: OpenApi] = Schema[A] { type Codec = B }
