package io.taig.openapi.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.openapi.schema.{Violations, Void}
import io.taig.openapi.schema.applyValidation
import io.taig.openapi.validation.{Constraint, Validation}

import scala.collection.immutable.VectorMap

sealed abstract class Queries[A]:
  def constraints: Chain[Constraint[OpenApi]]
  final def matches(queries: VectorMap[String, String]): Boolean = matchesWithRemainders(queries)._2
  def matchesWithRemainders(queries: VectorMap[String, String]): (VectorMap[String, String], Boolean)
  final def product[B](queries: Queries[B]): Queries[(A, B)] = Queries.Product(this, queries)
  final transparent inline def zip[B](queries: Queries[B]): Queries[?] = inline (this, queries) match
    case (left: Queries[Void], right) => left.product(right).imap[B] { case (_, b) => b }(b => (Void, b))
    case (left, right: Queries[Void]) => left.product(right).imap[A] { case (a, _) => a }(a => (a, Void))
    case (left: Queries[? *: ?], right) =>
      left.product(right).imap { case (a, b) => a :* b }(ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]))
    case (left, right) => left.product(right)
  final transparent inline def &[B](query: Query[B]): Queries[?] = zip(query.toQueries)
  final def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Queries[C] =
    Queries.Validate(this, validation, g)
  final def imap[B](f: A => B)(g: B => A): Queries[B] = ivalidate(Validation.lift(f))(g)
  final def decode(values: VectorMap[String, String]): Validated[Violations, A] =
    decodeWithRemainders(values).map(_._2)
  def decodeWithRemainders(
      queries: VectorMap[String, String]
  ): Validated[Violations, (VectorMap[String, String], A)]
  def encode(a: A): VectorMap[String, String]

object Queries:
  final private case class Root[A](query: Query[A]) extends Queries[A]:
    override def constraints: Chain[Constraint[OpenApi]] = Chain.empty
    override def matchesWithRemainders(queries: VectorMap[String, String]): (VectorMap[String, String], Boolean) =
      (queries.removed(query.name), query.isOptional || queries.contains(query.name))
    override def decodeWithRemainders(
        queries: VectorMap[String, String]
    ): Validated[Violations, (VectorMap[String, String], A)] = query.decode(queries)
    override def encode(a: A): VectorMap[String, String] = query.encode(a)

  final private case class Product[A, B](left: Queries[A], right: Queries[B]) extends Queries[(A, B)]:
    override def constraints: Chain[Constraint[OpenApi]] = left.constraints ++ right.constraints
    override def matchesWithRemainders(queries: VectorMap[String, String]): (VectorMap[String, String], Boolean) =
      val (remainders1, result1) = left.matchesWithRemainders(queries)
      val (remainders2, result2) = right.matchesWithRemainders(remainders1)
      (remainders2, result1 && result2)
    override def decodeWithRemainders(
        queries: VectorMap[String, String]
    ): Validated[Violations, (VectorMap[String, String], (A, B))] = left.decodeWithRemainders(queries) match
      case Validated.Valid((remainders, a)) => right.decodeWithRemainders(remainders).map(_.tupleLeft(a))
      case Validated.Invalid(violations)    => right.decode(queries).fold(violations merge _, _ => violations).invalid
    override def encode(ab: (A, B)): VectorMap[String, String] = left.encode(ab._1) ++ right.encode(ab._2)

  final private case class Validate[A, B: Encoder, C](
      queries: Queries[A],
      validation: Validation[B, A, A, C],
      g: C => A
  ) extends Queries[C]:
    override def constraints: Chain[Constraint[OpenApi]] =
      queries.constraints ++ validation.constraints.map(_.map(_.asOpenApi))
    override def matchesWithRemainders(queries: VectorMap[String, String]): (VectorMap[String, String], Boolean) =
      this.queries.matchesWithRemainders(queries)
    override def decodeWithRemainders(
        values: VectorMap[String, String]
    ): Validated[Violations, (VectorMap[String, String], C)] = queries
      .decodeWithRemainders(values)
      .andThen(
        _.traverse(
          applyValidation(
            validation,
            a => OpenApi.fromMap((queries.encode(a): Map[String, String]).fmap(OpenApi.fromString))
          )
        )
      )
    override def encode(c: C): VectorMap[String, String] = queries.encode(g(c))

  val Empty: Queries[Void] = new Queries[Void]:
    override def constraints: Chain[Constraint[OpenApi]] = Chain.empty
    override def matchesWithRemainders(queries: VectorMap[String, String]): (VectorMap[String, String], Boolean) =
      (queries, true)
    override def decodeWithRemainders(
        queries: VectorMap[String, String]
    ): Validated[Violations, (VectorMap[String, String], Void)] = (queries, Void).valid
    override def encode(a: Void): VectorMap[String, String] = VectorMap.empty

  def apply[A](query: Query[A]): Queries[A] = Root(query)
