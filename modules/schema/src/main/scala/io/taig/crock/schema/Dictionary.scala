package io.taig.crock.schema

import cats.Eval
import cats.data.Chain
import cats.syntax.all.*
import io.taig.crock.validation.{Constraint, Validation}

import scala.collection.immutable.ListMap

sealed abstract class Dictionary[A] extends Schema[A]:
  override type Self[a] = Dictionary[a]
  def key: Eval[Schema.Value[?]]
  def schema: Eval[Schema[?]]

  final override def optional: Dictionary[Option[A]] = Dictionary.Optional(this)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Dictionary[B] =
    Dictionary.Validate(this, validation, g)

object Dictionary:
  final case class Properties[+A](description: Option[String], example: Option[A])

  object Properties:
    val Empty: Dictionary.Properties[Nothing] = Properties(None, None)

  final case class Root[A, B](
      key: Eval[Schema.Value[A]],
      schema: Eval[Schema[B]],
      properties: Properties[ListMap[A, B]]
  ) extends Dictionary[ListMap[A, B]]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[ListMap[A, B]] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )

//    override def decode(crock: OpenApi.Object): Validated[Violations, SeqMap[A, B]] = crock.toChain
//      .traverse { case (k, v) =>
//        (key.value.parse(k), schema.value.decode(v)).tupled.leftMap(_.modifyHistory(k /: _))
//      }
//      .map(chain => SeqMap.from(chain.iterator))
//    override def encode(abs: SeqMap[A, B]): OpenApi.Object =
//      OpenApi.Object(abs.map { case (k, v) => (key.value.encode(k).render, schema.value.encode(v)) }.to(VectorMap))

  final case class Validate[A, B](self: Dictionary[A], validation: Validation[A, B], g: B => A) extends Dictionary[B]:
    export self.{isOptional, key, schema}
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def description: Property.Optional[String] =
      Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
    override def example: Property.Optional[B] =
      Property.Optional(self, _.example, value => copy(self = self.example(value)), validation, g)

  final case class Optional[A](self: Dictionary[A]) extends Dictionary[Option[A]]:
    export self.{constraints, key, schema}
    override def isOptional: Boolean = false
    override def description: Property.Optional[String] =
      Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
    override def example: Property.Optional[Option[A]] = Property.Optional(
      self.example.value.map(_.some),
      f => copy(self = self.example.modify(example => f(example.map(_.some)).flatten))
    )

  def apply[A, B](key: Eval[Schema.Value[A]], schema: Eval[Schema[B]]): Dictionary[ListMap[A, B]] =
    Root(key, schema, Properties.Empty)
