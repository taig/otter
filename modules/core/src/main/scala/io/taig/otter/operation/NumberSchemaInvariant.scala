package io.taig.otter.operation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.validation.Validation
import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.otter.Annotation
import io.taig.otter.Primitive

trait NumberSchemaInvariant[Self[_], +Constraint <: Constraint.Primitive] extends SchemaInvariant[Self]:
  self =>

  def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): Self[JBigDecimal]
  def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): Self[JBigInteger]
  def double(validation: Validation[Constraint.Primitive.Number, Double]): Self[Double]
  def float(validation: Validation[Constraint.Primitive.Number, Float]): Self[Float]
  def int(validation: Validation[Constraint.Primitive.Number, Int]): Self[Int]
  def long(validation: Validation[Constraint.Primitive.Number, Long]): Self[Long]

  extension [A](self: Self[A]) def constraints: Chain[Constraint]

  override def imapK[G[_]](fK: [A] => Self[A] => G[A])(
      gK: [A] => G[A] => Self[A]
  ): NumberSchemaInvariant[G, Constraint] = new NumberSchemaInvariant[G, Constraint]:
    override def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): G[JBigDecimal] =
      fK(self.bigDecimal(validation))

    override def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): G[JBigInteger] =
      fK(self.bigInteger(validation))

    override def double(validation: Validation[Constraint.Primitive.Number, Double]): G[Double] =
      fK(self.double(validation))

    override def float(validation: Validation[Constraint.Primitive.Number, Float]): G[Float] =
      fK(self.float(validation))

    override def int(validation: Validation[Constraint.Primitive.Number, Int]): G[Int] =
      fK(self.int(validation))

    override def long(validation: Validation[Constraint.Primitive.Number, Long]): G[Long] =
      fK(self.long(validation))

    extension [A](ga: G[A])
      override def constraints: Chain[Constraint] = self.constraints(gK(ga))

      override def imap[B](f: A => B)(g: B => A): G[B] = fK(self.imap(gK(ga))(f)(g))

object NumberSchemaInvariant:
  inline def apply[Self[_], Constraint <: Constraint.Primitive](using
      invariant: NumberSchemaInvariant[Self, Constraint]
  ): NumberSchemaInvariant[Self, Constraint] = invariant

  given schema: NumberSchemaInvariant[[a] =>> Annotation[Primitive.Number[a]], Constraint.Primitive.Number] with
    override def bigDecimal(
        validation: Validation[Constraint.Primitive.Number, JBigDecimal]
    ): Annotation[Primitive.Number[JBigDecimal]] =
      Annotation(Primitive.Number.BigDecimal(validation))

    override def bigInteger(
        validation: Validation[Constraint.Primitive.Number, JBigInteger]
    ): Annotation[Primitive.Number[JBigInteger]] =
      Annotation(Primitive.Number.BigInteger(validation))

    override def double(
        validation: Validation[Constraint.Primitive.Number, Double]
    ): Annotation[Primitive.Number[Double]] =
      Annotation(Primitive.Number.Double(validation))

    override def float(
        validation: Validation[Constraint.Primitive.Number, Float]
    ): Annotation[Primitive.Number[Float]] = Annotation(Primitive.Number.Float(validation))

    override def int(validation: Validation[Constraint.Primitive.Number, Int]): Annotation[Primitive.Number[Int]] =
      Annotation(Primitive.Number.Int(validation))

    override def long(validation: Validation[Constraint.Primitive.Number, Long]): Annotation[Primitive.Number[Long]] =
      Annotation(Primitive.Number.Long(validation))

    extension [A](fa: Annotation[Primitive.Number[A]])
      override def constraints: Chain[Constraint.Primitive.Number] = fa.self.constraints

      override def imap[B](f: A => B)(g: B => A): Annotation[Primitive.Number[B]] = fa.map(_.imap(f)(g))
