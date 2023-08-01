package io.taig.crock.schema

import cats.Eval
import cats.data.Chain
import io.taig.crock.validation.{Constraint, Validation}
import io.taig.enumeration.ext.Mapping

sealed abstract class Enumeration[A] extends Schema.Value[A]:
  self =>
  final override type Self[a] = Enumeration[a]

  def schema: Eval[Schema.Value[?]]
  def values[B](encoder: Encoder[Schema.Value, B]): List[B]

  override def ivalidate[B](validation: Validation[A, B])(g: B => A): Enumeration[B] =
    Enumeration.Validate(this, validation, g)

object Enumeration:
  final case class Properties[+A](description: Option[String], example: Option[A])

  object Properties:
    val Empty: Enumeration.Properties[Nothing] = Properties(None, None)

  final case class Root[A, B](
      mapping: Mapping[B, A],
      schema: Eval[Schema.Value[A]],
      properties: Enumeration.Properties[B]
  ) extends Enumeration[B]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def values[C](encoder: Encoder[Schema.Value, C]): List[C] =
      mapping.values.map(b => encoder.encode(schema.value, mapping.inj(b)))
    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[B] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )

//    override def values: Eval[List[OpenApi.Primitive]] =
//      schema.map(schema => mapping.values.map(mapping.inj).map(schema.encode))
//    override def modifyDescription(f: Option[String] => Option[String]): Enumeration[B] =
//      copy(description = f(description))
//    override def modifyExample(f: Option[B] => Option[B]): Enumeration[B] = copy(example = f(example))
//    override def decode(crock: OpenApi.Primitive): Validated[Violations, B] = schema.value
//      .decode(crock)
//      .andThen: key =>
//        mapping.prj(key).toValid {
//          val references = OpenApi.fromList(values.value.toList)
//          val constraint = Constraint("enumeration", references.some)
//          Violations.rootNec(Violation(constraint, schema.value.encode(key)))
//        }
//    override def encode(b: B): OpenApi.Primitive = schema.value.encode(mapping(b))
//    override def parse(value: String): Validated[Violations, B] = schema.value
//      .parse(value)
//      .andThen: a =>
//        mapping.prj(a).toValid {
//          val references = OpenApi.fromList(values.value.toList)
//          Violations.rootNec(Constraint("enumeration", references.some).toViolation(schema.value.encode(a)))
//        }
//
//    override def render(b: B): String = schema.value.render(mapping(b))

  final case class Validate[A, B](
      enumeration: Enumeration[A],
      validation: Validation[A, B],
      g: B => A
  ) extends Enumeration[B]:
    export enumeration.{schema, values}
    override def constraints: Chain[Constraint] = enumeration.constraints ++ validation.constraints
    override def description: Property.Optional[String] =
      Property.Optional(enumeration, _.description, value => copy(enumeration = enumeration.description(value)))
    override def example: Property.Optional[B] =
      Property.Optional(enumeration, _.example, value => copy(enumeration = enumeration.example(value)), validation, g)

  def apply[A, B](schema: Eval[Schema.Value[A]], mapping: Mapping[B, A]): Enumeration[B] =
    Root(mapping, schema, Properties.Empty)
