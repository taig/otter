package io.taig.otter

import io.taig.otter as Base
import io.taig.otter.Tuple.Reader

sealed trait Schema[+S[_], +A, B] extends Schema.Reader[S, A, B], Schema.Writer[S, A, B]:
  def ivalidate[C, D, E](validation: SchemaValidation[B, C, D, E])(f: E => B): Schema[S, A, E]
  override def optional: Schema[S, A, Option[B]]

object Schema:
  sealed trait Reader[+S[_], +A, +B] extends Product, Serializable:
    def optional: Schema.Reader[S, A, Option[B]]
    def validate[C, D, E](validation: SchemaValidation[B, C, D, E]): Schema.Reader[S, A, E]

  sealed trait Writer[+S[_], +A, -B] extends Product, Serializable:
    def contramap[C](f: C => B): Schema.Writer[S, A, C]
    def optional: Schema.Writer[S, A, Option[B]]

sealed trait Primitive[A] extends Schema[Nothing, Nothing, A], Primitive.Reader[A], Primitive.Writer[A]:
  override def ivalidate[B, C, D](validation: SchemaValidation[A, B, C, D])(f: D => A): Primitive[D] =
    Primitive.Modify(this, validation, f)
  override def optional: Primitive[Option[A]] = Primitive.Optional(this)

object Primitive:
  sealed trait Required[A] extends Primitive[A]:
    override def ivalidate[B, C, D](validation: SchemaValidation[A, B, C, D])(f: D => A): Primitive.Required[D] =
      Required.Modify(this, validation, f)

  object Required:
    sealed trait Reader[+A] extends Primitive.Reader[A]:
      override def validate[B, C, D](validation: SchemaValidation[A, B, C, D]): Primitive.Required.Reader[D] =
        Reader.Modify(this, validation)

    object Reader:
      final case class Modify[A, B, C, D](self: Primitive.Required.Reader[A], validation: SchemaValidation[A, B, C, D])
          extends Primitive.Required.Reader[D]

    sealed trait Writer[-A] extends Primitive.Writer[A]:
      override def contramap[B](f: B => A): Primitive.Required.Writer[B] = Writer.Modify(this, f)

    object Writer:
      final case class Modify[A, B](self: Primitive.Required.Writer[A], f: B => A) extends Primitive.Required.Writer[B]

    final case class Modify[A, B, C, D](
        self: Primitive.Required[A],
        validation: SchemaValidation[A, B, C, D],
        f: D => A
    ) extends Primitive.Required[D]

    final case class Root[A](tpe: Type[A]) extends Primitive.Required[A]

  sealed trait Reader[+A] extends Schema.Reader[Nothing, Nothing, A]:
    override def validate[B, C, D](validation: SchemaValidation[A, B, C, D]): Primitive.Reader[D] =
      Reader.Modify(this, validation)
    override def optional: Primitive.Reader[Option[A]] = Reader.Optional(this)

  object Reader:
    final case class Modify[A, B, C, D](self: Primitive.Reader[A], validation: SchemaValidation[A, B, C, D])
        extends Primitive.Reader[D]

    final case class Optional[A](self: Primitive.Reader[A]) extends Primitive.Reader[Option[A]]

  final case class Modify[A, B, C, D](
      self: Primitive[A],
      validation: SchemaValidation[A, B, C, D],
      f: D => A
  ) extends Primitive[D]

  sealed trait Writer[-A] extends Schema.Writer[Nothing, Nothing, A]:
    override def contramap[B](f: B => A): Primitive.Writer[B] = Writer.Modify(this, f)
    override def optional: Primitive.Writer[Option[A]] = Writer.Optional(this)

  object Writer:
    final case class Modify[B, A](self: Primitive.Writer[A], f: B => A) extends Primitive.Writer[B]
    final case class Optional[A](self: Primitive.Writer[A]) extends Primitive.Writer[Option[A]]

  final case class Optional[A](self: Primitive[A]) extends Primitive[Option[A]]

sealed trait Tuple[+S[_], +A, B] extends Schema[S, A, B], Tuple.Reader[S, A, B], Tuple.Writer[S, A, B]:
  final override def ivalidate[C, D, E](validation: SchemaValidation[B, C, D, E])(f: E => B): Tuple[S, A, E] =
    Tuple.Modify(this, validation, f)
  final override def optional: Tuple[S, A, Option[B]] = Tuple.Optional(this)
  final def product[T[a] >: S[a], C, D](schema: Tuple[T, C, D]): Tuple[T, A | C, (B, D)] = Tuple.Product(this, schema)

object Tuple:
  sealed trait Reader[+S[_], +A, +B] extends Schema.Reader[S, A, B]:
    def size: Int
    override def validate[C, D, E](validation: SchemaValidation[B, C, D, E]): Tuple.Reader[S, A, E] =
      Reader.Modify(this, validation)
    override def optional: Tuple.Reader[S, A, Option[B]] = Reader.Optional(this)
    final def product[T[a] >: S[a], C, D](schema: Tuple.Reader[T, C, D]): Tuple.Reader[T, A | C, (B, D)] =
      Reader.Product(this, schema)

  object Reader:
    case object Empty extends Tuple.Reader[Nothing, Nothing, Unit]:
      override def size: Int = 0

    final case class Modify[S[_], A, B, C, D, E](self: Tuple.Reader[S, A, B], validation: SchemaValidation[B, C, D, E])
        extends Tuple.Reader[S, A, E]:
      export self.size

    final case class One[S[_], A <: Schema.Reader[S, ?, B], B](schema: S[A]) extends Tuple.Reader[S, S[A], B]:
      override def size: Int = 1

    final case class Optional[S[_], A, B](self: Tuple.Reader[S, A, B]) extends Tuple.Reader[S, A, Option[B]]:
      export self.size

    final case class Product[S[_], A, B, C, D](left: Tuple.Reader[S, A, B], right: Tuple.Reader[S, C, D])
        extends Tuple.Reader[S, A | C, (B, D)]:
      override def size: Int = left.size + right.size

  sealed trait Writer[+S[_], +A, -B] extends Schema.Writer[S, A, B]:
    def size: Int
    override def contramap[C](f: C => B): Tuple.Writer[S, A, C] = Writer.Modify(this, f)
    override def optional: Tuple.Writer[S, A, Option[B]] = Writer.Optional(this)
    final def product[T[a] >: S[a], C, D](schema: Tuple.Writer[T, C, D]): Tuple.Writer[T, A | C, (B, D)] =
      Writer.Product(this, schema)

  object Writer:
    case object Empty extends Tuple.Writer[Nothing, Nothing, Unit]:
      override def size: Int = 0

    final case class Modify[S[_], A, B, C](self: Tuple.Writer[S, A, B], f: C => B) extends Tuple.Writer[S, A, C]:
      export self.size

    final case class One[S[_], A <: Schema.Writer[S, ?, B], B](schema: S[A]) extends Tuple.Writer[S, S[A], B]:
      override def size: Int = 1

    final case class Optional[S[_], A, B](self: Tuple.Writer[S, A, B]) extends Tuple.Writer[S, A, Option[B]]:
      export self.size

    final case class Product[S[_], A, B, C, D](left: Tuple.Writer[S, A, B], right: Tuple.Writer[S, C, D])
        extends Tuple.Writer[S, A | C, (B, D)]:
      override def size: Int = left.size + right.size

  case object Empty extends Tuple[Nothing, Nothing, Unit]:
    override def size: Int = 0

  final case class Modify[S[_], A, B, C, D, E](
      self: Tuple[S, A, B],
      validation: SchemaValidation[B, C, D, E],
      f: E => B
  ) extends Tuple[S, A, E]:
    export self.size

  final case class One[S[_], T[_], A <: Schema[?, ?, B], B](schema: T[A]) extends Tuple[S, T[A], B]:
    override def size: Int = 1

  final case class Optional[S[_], A, B](self: Tuple[S, A, B]) extends Tuple[S, A, Option[B]]:
    export self.size

  final case class Product[S[_], A, B, C, D](left: Tuple[S, A, B], right: Tuple[S, C, D])
      extends Tuple[S, A | C, (B, D)]:
    override def size: Int = left.size + right.size
