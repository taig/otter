package io.taig.openapi.schema

import cats.Eval
import cats.Eq
import cats.data.Validated
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.schemas.*
import io.taig.validation.{Constraint, Validation, Violation}
import io.taig.validation.validations

sealed abstract class Branch[A, B](
    val metadata: Branch.Metadata[A],
    val key: Eval[Value[A]],
    val schema: Eval[Schema[B]]
):
  def matches(name: A): Boolean

  object name:
    def value: A = metadata.name
    def set[C: Eq](name: C, key: => Value[C]): Branch[C, B] =
      new Branch[C, B](metadata.copy(name = name), Eval.later(key), schema):
        override def matches(name: C): Boolean = name === metadata.name
    def set(name: String): Branch[String, B] = set(name, string)

  final infix def orElse[C](branch: Branch[A, C]) = ???
  final infix def :+[C](branch: Branch[A, C]) = ???

  final def toSum: Sum[A, B] = Sum(this)

  // TODO imap, ivalidate, etc.

  private def refine[A](tpe: String)(f: OpenApi => Option[A]): Validation[OpenApi, OpenApi, OpenApi, A] =
    validations.refine(tpe)(f).mapReference(OpenApi.fromString)

  final def decode(openapi: OpenApi, discriminator: Discriminator): Validated[Violations, Option[B]] =
    discriminator match
      case Discriminator.Nested(identifier, value) =>
        refine("OpenApi.Object")(_.asObject).run(openapi).leftMap(Violations.root).andThen { obj =>
          Validated
            .fromOption(
              obj.get(identifier), {
                val constraint = Constraint("object.contains", OpenApi.fromString(identifier).some)
                Violations.rootNec(Violation(constraint, obj))
              }
            )
            .andThen(refine("OpenApi.Primitive")(_.asPrimitive).run(_).leftMap(Violations.root))
            .andThen(key.value.decode)
            .leftMap(_.modifyHistory(identifier /: _))
            .andThen { name =>
              if matches(name) then
                Validated
                  .fromOption(
                    obj.get(value), {
                      val constraint = Constraint("object.contains", OpenApi.fromString(value).some)
                      Violations.rootNec(Violation(constraint, obj))
                    }
                  )
                  .andThen(schema.value.decode)
                  .bimap(_.modifyHistory(value /: _), _.some)
              else none[B].valid
            }
        }
      case _ => ??? // TODO

  final def encode(b: B, discriminator: Discriminator): OpenApi = discriminator match
    case Discriminator.Nested(identifier, value) =>
      OpenApi.obj(identifier -> key.value.encode(metadata.name), value -> schema.value.encode(b))
    case Discriminator.Merged(identifier) =>
      schema.value.encode(b).asObject match
        case Some(obj) if obj.contains(identifier) => OpenApi.Object.Empty
        case Some(obj) => obj.deepMerge(OpenApi.obj(identifier -> key.value.encode(metadata.name)))
        case None      => OpenApi.Object.Empty
    case Discriminator.Keyed =>
      OpenApi.obj(key.value.encode(metadata.name).render -> schema.value.encode(b))
    case Discriminator.None => schema.value.encode(b)

object Branch:
  final case class Metadata[A](name: A)

  def apply[A: Eq, B](name: A, key: Eval[Value[A]], schema: Eval[Schema[B]]): Branch[A, B] =
    new Branch[A, B](Metadata(name), key, schema):
      override def matches(name: A): Boolean = name === metadata.name
