package io.taig.otter.openapi

import io.taig.otter as Plain
import cats.Id as Identity

sealed abstract class Schema[A] extends Plain.Schema.Ops[Schema, Schema, A]:
  type M[f[_]] <: Metadata[f]
  def metadata: M[Identity]
  def plain: Plain.Schema[A]

sealed abstract class Value[A] extends Schema[A] with Plain.Value.Ops[Value, Value, A]

sealed abstract class Primitive[A] extends Value[A] with Plain.Primitive.Ops[Primitive, Primitive, A]

object Primitive:
  final case class Required[A](metadata: Metadata.Primitive[Identity], plain: Plain.Primitive.Required[A])
      extends Primitive[A]
      with Plain.Primitive.Ops[Primitive.Required, Primitive, A]:
    override type M[f[_]] = Metadata.Primitive[f]
    override def imap[B](f: A => B)(g: B => A): Primitive.Required[B] = copy(plain = plain.imap(f)(g))
    override def optional: Primitive[Option[A]] = Optional(metadata, plain.optional)

  final case class Optional[A](metadata: Metadata.Primitive[Identity], plain: Plain.Primitive[A]) extends Primitive[A]:
    override type M[f[_]] = Metadata.Primitive[f]
    override def imap[B](f: A => B)(g: B => A): Primitive[B] = copy(plain = plain.imap(f)(g))
    override def optional: Primitive[Option[A]] = copy(plain = plain.optional)

final case class Tuple[A](metadata: Metadata.Tuple[Identity], plain: Plain.Tuple[A])
    extends Schema[A]
    with Plain.Tuple.Ops[Tuple, Tuple, A]:
  override type M[f[_]] = Metadata.Tuple[f]
  override def imap[B](f: A => B)(g: B => A): Tuple[B] = copy(plain = plain.imap(f)(g))
  override def optional: Tuple[Option[A]] = copy(plain = plain.optional)
  override def product[B](tuple: Tuple[B]): Tuple[(A, B)] =
    Tuple(metadata = ???, plain = plain.product(tuple.plain))
