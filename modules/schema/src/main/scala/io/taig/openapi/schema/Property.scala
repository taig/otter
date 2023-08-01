package io.taig.openapi.schema

import io.taig.openapi.validation.Validation

import scala.annotation.targetName

abstract class Property[A, B]:
  def value: B
  def modify(f: B => B): A
  final def apply(b: B): A = modify(_ => b)

object Property:
  abstract class Optional[A, B] extends Property[A, Option[B]]:
    @targetName("as")
    final def apply(b: B): A = apply(Some(b))
    final def clear: A = apply(None)

  object Optional:
    def apply[A, B](b: Option[B], g: (Option[B] => Option[B]) => A): Property.Optional[A, B] = new Optional[A, B]:
      override def value: Option[B] = b
      override def modify(f: Option[B] => Option[B]): A = g(f)

    def apply[F[_], A, B, C](property: Property.Optional[F[A], C], copy: F[A] => F[B]): Property.Optional[F[B], C] =
      new Optional[F[B], C]:
        export property.value
        override def modify(f: Option[C] => Option[C]): F[B] = copy(property.modify(f))

    def apply[F[_], A, B](
        property: Property.Optional[F[A], A],
        copy: F[A] => F[B],
        validation: Validation[A, B],
        g: B => A
    ): Property.Optional[F[B], B] = new Optional[F[B], B]:
      override def value: Option[B] = property.value.flatMap(validation(_).toOption)
      override def modify(f: Option[B] => Option[B]): F[B] =
        copy(property.modify(_.flatMap(validation(_).toOption.map(g))))
