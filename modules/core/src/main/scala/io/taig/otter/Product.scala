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

  override def asReader: Product.Reader[F, A, B] = this
  override def asWriter: Product.Writer[F, A, B] = this

  def schemas: Chain[F[Codec[F, ?, ?]]]

  override def imap[C](f: B => C)(g: C => B): Product[F, A, C] = new Product[F, A, C]:
    export self.schemas
    override def decodeWithRemainders(data: Option[Data.Array]): Codec.Result[(Option[Data.Array], C)] =
      self.asReader.map(f).decodeWithRemainders(data)
    override def encode(c: C): Option[Data.Array] = self.asWriter.contramap(g).encode(c)

  override def default(value: B): Product[F, A, B] = new Product[F, A, B]:
    export self.{encode, schemas}
    override def decodeWithRemainders(data: Option[Data.Array]): Codec.Result[(Option[Data.Array], B)] =
      self.asReader.default(value).decodeWithRemainders(data)

  override def optional: Product[F, A, Option[B]] = new Product[F, A, Option[B]]:
    export self.schemas
    override def decodeWithRemainders(data: Option[Data.Array]): Codec.Result[(Option[Data.Array], Option[B])] =
      self.asReader.optional.decodeWithRemainders(data)
    override def encode(b: Option[B]): Option[Data.Array] = self.asWriter.optional.encode(b)

  def zip[F1[+a] >: F[a], C, D](product: Product[F1, C, D]): Product[F1, A & C, (B, D)] =
    new Product[F1, A & C, (B, D)]:

      override def schemas: Chain[F1[Codec[F1, ?, ?]]] = self.schemas ++ product.schemas

      override def decodeWithRemainders(data: Option[Data.Array]): Result[(Option[Data.Array], (B, D))] =
        self.asReader.zip(product.asReader).decodeWithRemainders(data)

      override def encode(b: (B, D)): Option[Data.Array] = self.asWriter.zip(product.asWriter).encode(b)

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

    def zip[F1[+a] >: F[a], C, D](product: Product.Reader[F1, C, D]): Product.Reader[F1, A & C, (B, D)] =
      new Product.Reader[F1, A & C, (B, D)]:
        override def schemas: Chain[F1[Codec.Reader[F1, ?, ?]]] = self.schemas ++ product.schemas

        override def decodeWithRemainders(data: Option[Data.Array]): Result[(Option[Data.Array], (B, D))] =
          self.decodeWithRemainders(data) match
            case Validated.Valid((remainders, a)) => product.decodeWithRemainders(remainders).map(_.tupleLeft(a))
            case Validated.Invalid(violations) =>
              product.decodeWithRemainders(data).fold(violations.combine, _ => violations).invalid

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

    def zip[F1[+a] >: F[a], C, D](product: Product.Writer[F1, C, D]): Product.Writer[F1, A & C, (B, D)] =
      new Product.Writer[F1, A & C, (B, D)]:
        override def schemas: Chain[F1[Codec.Writer[F1, ?, ?]]] = self.schemas ++ product.schemas
        override def encode(bd: (B, D)): Option[Data.Array] = (self.encode(bd._1), product.encode(bd._2)) match
          case (Some(left), Some(right)) => (left ++ right).some
          case (None, Some(right))       => (Data.Array.fill(self.schemas.length.toInt)(Data.Null) ++ right).some
          case (Some(left), None)        => (left ++ Data.Array.fill(product.schemas.length.toInt)(Data.Null)).some
          case (None, None)              => Data.Array.fill(schemas.length.toInt)(Data.Null).some

    override def encode(b: B): Option[Data.Array]

  val Empty: Product[Nothing, Nothing, Unit] = new Product[Nothing, Nothing, Unit]:
    override def schemas: Chain[Nothing] = Chain.empty
    override def decodeWithRemainders(data: Option[Data.Array]): Codec.Result[(Option[Data.Array], Unit)] = data match
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
