package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*

sealed abstract class Collection[+O <: Data, A] extends Codec[Data.Array[O], A]:
  self =>

  def constraints: Vector[Constraint.Collection]

  def codec: Codec[?, ?]

  final override def modifyMetadata(f: Metadata => Metadata): Collection[O, A] = new Collection[O, A]:
    export self.{codec, constraints, decode, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def imap[B](f: A => B)(g: B => A): Collection[O, B] = new Collection[O, B]:
    export self.{codec, constraints, metadata}
    override def decode(data: Option[Vector[Data]]): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): Data.Array[O] = self.encode(g(b))

  final override def to[B](using convert: Convert[A, B]): Collection[O, B] = imap(convert.to)(convert.from)

  override def decode(data: Data): Codec.Result[A] = data match
    case Data.Array(values) => decode(values.some)
    case _                  => Violations.rootNec(Violation.tpe("array", actual = data.name)).invalid

  protected def decode(data: Option[Vector[Data]]): Codec.Result[A]

object Collection:
  final private case class Apply[O <: Data, A](
      codec: Codec[O, A],
      minItems: Option[Int],
      maxItems: Option[Int],
      uniqueItems: Boolean
  ) extends Collection[O, Vector[A]]:
    override def constraints: Vector[Constraint.Collection] =
      minItems.map(Constraint.Collection.MinItems.apply).toVector ++
        minItems.map(Constraint.Collection.MaxItems.apply).toVector ++
        Option.when(uniqueItems)(Constraint.Collection.UniqueItems).toVector
    override def metadata: Metadata = Metadata.Empty
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
      values.zipWithIndex.traverse { case (data, index) => codec.decode(data).leftMap(index /: _) }
    override def encode(as: Vector[A]): Data.Array[O] = Data.Array(as.map(codec.encode))

  final case class NonEmpty[O <: Data, A](
      codec: Codec[O, A],
      minItems: Option[Int],
      maxItems: Option[Int],
      uniqueItems: Boolean
  ) extends Collection[O, (A, Vector[A])]:
    val of = Collection(codec, minItems = minItems.max(1.some), maxItems, uniqueItems)
    override def constraints: Vector[Constraint.Collection] = of.constraints
    override def metadata: Metadata = Metadata.Empty
    override def encode(aas: (A, Vector[A])): Data.Array[O] = of.encode(aas._1 +: aas._2)
    override def decode(data: Option[Vector[Data]]): Codec.Result[(A, Vector[A])] =
      // Safe to call .head, because `wrapped` will perform a length check
      of.decode(data).map(values => (values.head, values.tail))

  def apply[O <: Data, A](
      codec: Codec[O, A],
      minItems: Option[Int],
      maxItems: Option[Int],
      uniqueItems: Boolean
  ): Collection[O, Vector[A]] = Apply(codec, minItems, maxItems, uniqueItems)

  def nonEmpty[O <: Data, A](
      codec: => Codec[O, A],
      minItems: Option[Int],
      maxItems: Option[Int],
      uniqueItems: Boolean
  ): Collection[O, (A, Vector[A])] = NonEmpty(codec, minItems, maxItems, uniqueItems)

  given [O <: Data]: CodecInvariant[Collection[O, *]] with
    override def imap[A, B](fa: Collection[O, A])(f: A => B)(g: B => A): Collection[O, B] = fa.imap(f)(g)

  given [O <: Data, A]: Metadata.Ops[Collection[O, A]] with
    extension (self: Collection[O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Collection[O, A] = self.modifyMetadata(f)
