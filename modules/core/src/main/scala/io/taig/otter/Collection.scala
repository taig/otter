package io.taig.otter
import cats.implicits.*
import io.taig.otter.operation.CollectionSchemaInvariant

final case class Collection[+S[_], A](value: Collection.Value[S, A], metadata: Metadata)

object Collection:
  sealed abstract class Value[+S[_], A] extends Product with Serializable:
    def constraints: Vector[Constraint.Collection]

    def schema: Reference[S, ?]

    final def imap[B](f: A => B)(g: B => A): Collection.Value[S, B] = Value.Modify(self = this, f, g)

    def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection.Value[T, A]

  object Value:
    private def constraints(
        minimum: Option[Int],
        maximum: Option[Int],
        unique: Boolean
    ): Vector[Constraint.Collection] = Vector(
      minimum.map(Constraint.Collection.Minimum.apply),
      maximum.map(Constraint.Collection.Maximum.apply),
      Option.when(unique)(Constraint.Collection.Unique)
    ).flatten

    final private[otter] case class Indexed[S[_], A](
        schema: Reference[S, A],
        minimum: Option[Int],
        maximum: Option[Int],
        unique: Boolean
    ) extends Collection.Value[S, Vector[A]]:
      override def constraints: Vector[Constraint.Collection] = Value.constraints(minimum, maximum, unique)
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection.Value[T, Vector[A]] =
        copy(schema = schema.mapK[S1, T](fK))

    final private[otter] case class Linked[S[_], A](
        schema: Reference[S, A],
        minimum: Option[Int],
        maximum: Option[Int],
        unique: Boolean
    ) extends Collection.Value[S, List[A]]:
      override def constraints: Vector[Constraint.Collection] = Value.constraints(minimum, maximum, unique)
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection.Value[T, List[A]] =
        copy(schema = schema.mapK[S1, T](fK))

    final private[otter] case class Modify[S[_], A, B](self: Collection.Value[S, A], f: A => B, g: B => A)
        extends Collection.Value[S, B]:
      export self.{constraints, schema}
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection.Value[T, B] =
        copy(self = self.mapK[S1, T](fK))

  given [Value[_]]: CollectionSchemaInvariant[Collection[Value, *], Value] with
    override def linked[A](
        schema: => Value[A],
        minimum: Option[Int],
        maximum: Option[Int],
        unique: Boolean
    ): Collection[Value, List[A]] = Collection(
      value = Value.Linked(schema = Reference.later(schema), minimum, maximum, unique),
      metadata = Metadata.Empty
    )

    override def indexed[A](
        schema: => Value[A],
        minimum: Option[Int],
        maximum: Option[Int],
        unique: Boolean
    ): Collection[Value, Vector[A]] = Collection(
      value = Value.Indexed(schema = Reference.later(schema), minimum, maximum, unique),
      metadata = Metadata.Empty
    )

    override def imap[A, B](fa: Collection[Value, A])(f: A => B)(g: B => A): Collection[Value, B] =
      fa.copy(value = fa.value.imap(f)(g))

    extension [A](self: Collection[Value, A])
      override def metadata: Metadata = self.metadata
      override def metadata(f: Metadata => Metadata): Collection[Value, A] =
        self.copy(metadata = f(self.metadata))
