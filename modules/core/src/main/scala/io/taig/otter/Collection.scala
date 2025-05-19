package io.taig.otter
import cats.implicits.*
import io.taig.otter.Metadata
import io.taig.otter.schema.CollectionSchema

sealed abstract class Collection[+S[_], A] extends Product with Serializable:
  def constraints: Vector[Constraint.Collection]

  def schema: Reference[S, ?]

  final def imap[B](f: A => B)(g: B => A): Collection[S, B] = Collection.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, A]

object Collection:
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
  ) extends Collection[S, Vector[A]]:
    override def constraints: Vector[Constraint.Collection] = Collection.constraints(minimum, maximum, unique)
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, Vector[A]] =
      copy(schema = schema.mapK[S1, T](fK))

  final private[otter] case class Linked[S[_], A](
      schema: Reference[S, A],
      minimum: Option[Int],
      maximum: Option[Int],
      unique: Boolean
  ) extends Collection[S, List[A]]:
    override def constraints: Vector[Constraint.Collection] = Collection.constraints(minimum, maximum, unique)
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, List[A]] =
      copy(schema = schema.mapK[S1, T](fK))

  final private[otter] case class Modify[S[_], A, B](self: Collection[S, A], f: A => B, g: B => A)
      extends Collection[S, B]:
    export self.{constraints, schema}
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, B] =
      copy(self = self.mapK[S1, T](fK))

  given [Value[_]]: CollectionSchema[Collection[Value, *], Value] with
    override def linked[A](
        schema: => Value[A],
        minimum: Option[Int],
        maximum: Option[Int],
        unique: Boolean
    ): Collection[Value, List[A]] = Linked(schema = Reference.later(schema), minimum, maximum, unique)

    override def indexed[A](
        schema: => Value[A],
        minimum: Option[Int],
        maximum: Option[Int],
        unique: Boolean
    ): Collection[Value, Vector[A]] = Indexed(schema = Reference.later(schema), minimum, maximum, unique)

    override def imap[A, B](fa: Collection[Value, A])(f: A => B)(g: B => A): Collection[Value, B] = fa.imap(f)(g)
