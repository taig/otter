package io.taig.otter

import io.taig.otter.Product.Reader
import cats.data.Chain
import cats.syntax.all.*
import cats.Comonad
import scala.annotation.targetName
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import cats.data.Validated
import io.taig.otter.Codec.Result

sealed trait Product[+F[+_], +A, B] extends Codec[F, A, B], Product.Reader[F, A, B], Product.Writer[F, A, B]:
  self =>

  def schemas: Chain[F[Codec[F, ?, ?]]]

  override def imap[C](f: B => C)(g: C => B): Product[F, A, C] = new Product[F, A, C]:
    export self.schemas
    override def decodeWithRemainders(data: Option[Data.Array]): Codec.Result[(Option[Data.Array], C)] =
      self.decodeWithRemainders(data).map(_.map(f))
    override def encode(c: C): Option[Data.Array] = self.encode(g(c))

  override def default(value: B): Product[F, A, B] = new Product[F, A, B]:
    export self.{encode, schemas}
    override def decodeWithRemainders(data: Option[Data.Array]): Codec.Result[(Option[Data.Array], B)] =
      data.fold((data, value).valid)(_ => self.decodeWithRemainders(data))

  override def optional: Product[F, A, Option[B]] = new Product[F, A, Option[B]]:
    export self.schemas
    override def decodeWithRemainders(data: Option[Data.Array]): Codec.Result[(Option[Data.Array], Option[B])] =
      data.fold((data, none).valid)(_ => self.decodeWithRemainders(data).map(_.map(_.some)))
    override def encode(b: Option[B]): Option[Data.Array] = b.flatMap(self.encode)

object Product:
  sealed trait Reader[+F[+_], +A, +B] extends Codec.Reader[F, A, B]:
    self =>

    def schemas: Chain[F[Codec.Reader[F, ?, ?]]]

    final override def map[C](f: B => C): Product.Reader[F, A, C] = new Product.Reader[F, A, C]:
      export self.schemas
      override def decodeWithRemainders(data: Option[Data.Array]): Codec.Result[(Option[Data.Array], C)] =
        self.decodeWithRemainders(data).map(_.map(f))

    override def default[B1 >: B](value: B1): Product.Reader[F, A, B1] = new Product.Reader[F, A, B1]:
      export self.schemas
      override def decodeWithRemainders(data: Option[Data.Array]): Codec.Result[(Option[Data.Array], B1)] =
        data.fold((data, value).valid)(_ => self.decodeWithRemainders(data))

    override def optional: Product.Reader[F, A, Option[B]] = new Product.Reader[F, A, Option[B]]:
      export self.schemas
      override def decodeWithRemainders(data: Option[Data.Array]): Codec.Result[(Option[Data.Array], Option[B])] =
        data.fold((data, none).valid)(_ => self.decodeWithRemainders(data).map(_.map(_.some)))

    final override def decode(data: Option[Data.Value]): Codec.Result[B] = data match
      case Some(data: Data.Array) => decode(Some(data))
      case Some(data) =>
        Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String(data.name))).invalid
      case None => decode(None)

    @targetName("decodeArray")
    // TODO disallow additional items
    def decode(data: Option[Data.Array]): Codec.Result[B] = decodeWithRemainders(data).map { case (_, b) => b }

    def decodeWithRemainders(data: Option[Data.Array]): Codec.Result[(Option[Data.Array], B)]

  sealed trait Writer[+F[+_], +A, -B] extends Codec.Writer[F, A, B]:
    self =>

    def schemas: Chain[F[Codec.Writer[F, ?, ?]]]

    final override def contramap[C](f: C => B): Product.Writer[F, A, C] = new Product.Writer[F, A, C]:
      export self.schemas
      override def encode(c: C): Option[Data.Array] = self.encode(f(c))

    override def optional: Product.Writer[F, A, Option[B]] = new Product.Writer[F, A, Option[B]]:
      export self.schemas
      override def encode(b: Option[B]): Option[Data.Array] = b.flatMap(self.encode)

    override def encode(b: B): Option[Data.Array]

  val Empty: Product[Nothing, Nothing, Unit] = new Product[Nothing, Nothing, Unit]:
    override def schemas: Chain[Nothing] = Chain.empty
    override def decodeWithRemainders(data: Option[Data.Array]): Codec.Result[(Option[Data.Array], Unit)] =
      data match
        case Some(Data.Array(values)) =>
          val length = values.length

          Validated.cond(
            length === 0,
            (data, ()),
            Violations.rootNec(Violation(Constraint.Collection.MaxItems(reference = 0), actual = Data.Number(length)))
          )
        case None =>
          Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = Data.String("null"))).invalid
    override def encode(b: Unit): Option[Data.Array] = Data.Array.Empty.some

  def apply[F[+_]: Comonad, A, B](schema: F[Codec[F, A, B]]): Product[F, schema.type, B] =
    new Product[F, schema.type, B]:
      override def schemas: Chain[F[Codec[F, ?, ?]]] = Chain.one(schema)
      override def decodeWithRemainders(data: Option[Data.Array]): Result[(Option[Data.Array], B)] = data
        .toValid(Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = Data.String("null"))))
        .map(_.uncons)
        .andThen:
          case Some((head, tail)) => schema.extract.decode(head).tupleLeft(tail.some)
          case None =>
            Violations
              .rootNec(Violation(Constraint.Collection.MinItems(reference = 1), actual = Data.Number(0)))
              .invalid
      override def encode(b: B): Option[Data.Array] = Data.Array.of(schema.extract.encode(b).getOrElse(Data.Null)).some
