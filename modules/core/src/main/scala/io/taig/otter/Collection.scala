package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Collection[A](description: Option[String]) extends Schema[A](description):
  self =>
  override type Self[a] <: Collection[a]

  final override def decode(data: Option[Data.Value]): Validated[Violations, A] = data match
    case Some(_: Data.Array) => decodeArray(data.asInstanceOf[Option[Data.Array]])
    case Some(data)          => Violations.rootNec(Violation.tpe("array", actual = data.name)).invalid
    case None                => decodeArray(None)
  def decodeArray(data: Option[Data.Array]): Validated[Violations, A]
  final override def encode(a: A): Data = encodeArray(a).getOrElse(Data.Null)
  def encodeArray(a: A): Option[Data.Array]

object Collection:
  def apply[A](schema: Collection[A], description: Option[String]): Collection[A] =
    new Collection[A](description) { export schema.* }

  abstract private class Root[A](description: Option[String]) extends Collection[A](description):
    self =>
    final override type Self[a] = Collection[a]

    final override def description(f: Option[String] => Option[String]): Collection[A] =
      Collection(this, f(description))

    final override def optional: Collection[Option[A]] = new Root[Option[A]](description):
      export self.constraints
      override def isOptional: Boolean = true
      override def decodeArray(data: Option[Data.Array]): Validated[Violations, Option[A]] =
        data.fold(none.valid)(_ => self.decodeArray(data).map(_.some))
      override def encodeArray(a: Option[A]): Option[Data.Array] = a.flatMap(self.encodeArray)

    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Collection[B] =
      new Root[B](description):
        export self.isOptional
        override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
        override def decodeArray(data: Option[Data.Array]): Validated[Violations, B] =
          self.decodeArray(data).andThen(validation(_).leftMap(Violations.root))
        override def encodeArray(b: B): Option[Data.Array] = self.encodeArray(g(b))

  def apply[A](schema: Schema[A]): Collection[Chain[A]] = new Root[Chain[A]](None):
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decodeArray(data: Option[Data.Array]): Validated[Violations, Chain[A]] = data match
      case Some(data) =>
        data.values.zipWithIndex.traverse { case (data, index) =>
          schema.decode(data).leftMap(_.modifyHistory(index /: _))
        }
      case None => Violations.rootNec(Violation.required).invalid
    override def encodeArray(a: Chain[A]): Option[Data.Array] = Data.Array(a.map(schema.encode)).some

  sealed abstract class Value[A](description: Option[String]) extends Collection[A](description):
    self =>
    final override type Self[a] = Collection.Value[a]

    override def description(f: Option[String] => Option[String]): Collection.Value[A] = Value(this, f(description))

    override def optional: Collection.Value[Option[A]] = new Value[Option[A]](description):
      export self.constraints
      override def isOptional: Boolean = true
      override def print(a: Option[A]): Option[Chain[Option[String]]] = ???
      override def parse(values: Option[Chain[Option[String]]]): Validated[Violations, Option[A]] = ???
      override def decodeArray(data: Option[Data.Array]): Validated[Violations, Option[A]] = ???
      override def encodeArray(a: Option[A]): Option[Data.Array] = ???

    override def ivalidate[B](validation: Validation[A, B])(g: B => A): Collection.Value[B] = new Value[B](description):
      export self.isOptional
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def decodeArray(data: Option[Data.Array]): Validated[Violations, B] = ???
      override def encodeArray(a: B): Option[Data.Array] = ???
      override def parse(values: Option[Chain[Option[String]]]): Validated[Violations, B] = ???
      override def print(a: B): Option[Chain[Option[String]]] = ???

    def print(a: A): Option[Chain[Option[String]]]
    def parse(values: Option[Chain[Option[String]]]): Validated[Violations, A]

  object Value:
    def apply[A](schema: Collection.Value[A], description: Option[String]): Collection.Value[A] =
      new Value[A](description) { export schema.* }

    def apply[A](schema: Schema.Value[A]): Collection.Value[Chain[A]] = new Value[Chain[A]](None):
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def decodeArray(data: Option[Data.Array]): Validated[Violations, Chain[A]] = ???
      override def encodeArray(a: Chain[A]): Option[Data.Array] = Data.Array(a.map(schema.encode)).some
      override def parse(values: Option[Chain[Option[String]]]): Validated[Violations, Chain[A]] = ???
      override def print(as: Chain[A]): Option[Chain[Option[String]]] =
        as.map(schema.print).some
