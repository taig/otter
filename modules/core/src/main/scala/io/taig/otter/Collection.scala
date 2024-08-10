package io.taig.otter

import cats.syntax.all.*
import cats.data.Validated
import cats.Invariant
import io.taig.otter.Codec.Result

sealed abstract class Collection[+F[+a] <: Data.Optional[a], +O <: Data, A] extends Codec[F, Data.Array[O], A]:
  self =>

  def constraints: Vector[Constraint.Collection]

  def codec: Codec[?, ?, ?]

  final override def modifyMetadata(f: Metadata => Metadata): Collection[F, O, A] = new Collection[F, O, A]:
    export self.{codec, constraints, decode, default, encode, isOptional}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Collection[F, O, A] = new Collection[F, O, A]:
    export self.{codec, constraints, encode, metadata}
    override def default: Option[A] = f(self.default)
    override def isOptional: Boolean = default.nonEmpty
    override def decode(data: Option[Vector[Data]]): Codec.Result[A] = (data, default) match
      case (None, Some(default)) => default.valid
      case _                     => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Collection[F, O, B] = new Collection[F, O, B]:
    export self.{codec, constraints, metadata, isOptional}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Option[Vector[Data]]): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): F[Data.Array[O]] = self.encode(g(b))

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Collection[F, O, B] = imap(evidence.from)(evidence.to)

  override def optional: Collection[Data.Optional, O, Option[A]] = new Collection[Data.Optional, O, Option[A]]:
    export self.{codec, constraints, metadata}
    override def isOptional: Boolean = true
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Option[Vector[Data]]): Codec.Result[Option[A]] =
      data.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
    override def encode(a: Option[A]): Data.Optional[Data.Array[O]] = a.map(self.encode).getOrElse(Data.Null)

  override def decode(data: Data): Codec.Result[A] = data match
    case Data.Array(values) => decode(values.some)
    case Data.Null          => decode(none)
    case _ => Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String(data.name))).invalid

  def decode(data: Option[Vector[Data]]): Codec.Result[A]

object Collection:
  def apply[F[+a] <: Data.Optional[a], O <: Data, A](
      of: Codec[F, O, A],
      minItems: Option[Int],
      maxItems: Option[Int],
      uniqueItems: Boolean
  ): Collection[Data.Required, F[O], Vector[A]] = new Collection[Data.Required, F[O], Vector[A]]:
    override def constraints: Vector[Constraint.Collection] =
      minItems.map(Constraint.Collection.MinItems.apply).toVector ++
        minItems.map(Constraint.Collection.MaxItems.apply).toVector ++
        Option.when(uniqueItems)(Constraint.Collection.UniqueItems).toVector
    override def isOptional: Boolean = false
    override def codec: Codec[?, ?, ?] = of
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Vector[A]] = None
    def verifyMinItems(values: Vector[Data]): Codec.Result[Unit] = minItems.traverse_ { reference =>
      val length = values.length
      Validated.cond(
        length >= reference,
        (),
        Violations.rootNec(Violation(Constraint.Collection.MinItems(reference), actual = Data.Number(length)))
      )
    }
    def verifyMaxItems(values: Vector[Data]): Codec.Result[Unit] = maxItems.traverse_ { reference =>
      val length = values.length
      Validated.cond(
        length >= reference,
        (),
        Violations.rootNec(Violation(Constraint.Collection.MaxItems(reference), actual = Data.Number(length)))
      )
    }
    def verifyUniqueItems(values: Vector[Data]): Codec.Result[Unit] =
      values.groupBy(identity).collect { case (a, as) if as.sizeCompare(1) > 0 => a }.toVector match
        case Vector() => ().valid
        case values =>
          Violations.rootNec(Violation(Constraint.Collection.UniqueItems, actual = Data.Array(values))).invalid
    override def decode(data: Option[Vector[Data]]): Codec.Result[Vector[A]] = data
      .toValid(Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String("null"))))
      .andThen(decode)
    def decode(values: Vector[Data]): Codec.Result[Vector[A]] = verifyMinItems(values) *>
      verifyMaxItems(values) *>
      values.zipWithIndex.traverse { case (data, index) => of.decode(data).leftMap(index /: _) }
    override def encode(as: Vector[A]): Data.Array[F[O]] = Data.Array(as.map(of.encode))

  def nonEmpty[F[+a] <: Data.Optional[a], O <: Data, A](
      of: Codec[F, O, A],
      minItems: Option[Int],
      maxItems: Option[Int],
      uniqueItems: Boolean
  ): Collection[Data.Required, F[O], (A, Vector[A])] = new Collection[Data.Required, F[O], (A, Vector[A])]:
    val wrapped = Collection(of, minItems = minItems.max(1.some), maxItems, uniqueItems)
    override def constraints: Vector[Constraint.Collection] = wrapped.constraints
    override def isOptional: Boolean = false
    override def codec: Codec[?, ?, ?] = of
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[(A, Vector[A])] = None
    override def encode(aas: (A, Vector[A])): Data.Array[F[O]] = wrapped.encode(aas._1 +: aas._2)
    override def decode(data: Option[Vector[Data]]): Codec.Result[(A, Vector[A])] =
      // Safe to call .head, because `wrapped` will perform a length check
      wrapped.decode(data).map(values => (values.head, values.tail))

  given invariant[F[+a] <: Data.Optional[a], O <: Data]: Invariant[Collection[F, O, *]] with
    override def imap[A, B](fa: Collection[F, O, A])(f: A => B)(g: B => A): Collection[F, O, B] = fa.imap(f)(g)
