package io.taig.openapi.schema

import cats.{Eq, Eval}
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.*
import io.taig.validation.{identifiers, Constraint, Validation, Violation}
import io.taig.validation.syntax.*

sealed abstract class Branch[A, B](
    val name: A,
    val key: Eval[Schema.Of[A, OpenApi.Primitive]],
    val schema: Eval[Schema[B]]
):
  self =>

  def constraints: Chain[Constraint[OpenApi]] = schema.value.constraints

  final def modifySchema[C](f: Schema[B] => Schema[C]): Branch[A, C] = new Branch[A, C](name, key, schema.map(f)):
    export self.matches

  final infix def orElse[C](branch: Branch[A, C]): Sum[A, Either[B, C]] = toSum orElse branch.toSum
  final def :+[C](branch: Branch[A, C]): Sum[A, Either[B, C]] = this orElse branch

  def matches(name: A): Boolean

  final def imap[C](f: B => C)(g: C => B): Branch[A, C] = modifySchema(_.imap(f)(g))
  final def gimap[C](using evidence: Evidence.Sum.Aux[C, B]): Sum[A, C] = imap(evidence.from)(evidence.to).toSum
  final def ivalidate[C: Encoder, D](validation: Validation[C, B, B, D])(g: D => B): Branch[A, D] =
    modifySchema(_.ivalidate(validation)(g))

  final def toSum: Sum[A, B] = Sum.one(this)

  final def decode(openapi: OpenApi, discriminator: Option[Discriminator]): Validated[Violations, Option[B]] =
    discriminator match
      case Some(Discriminator.Nested(identifier, value)) =>
        refine("OpenApi.Object")(_.asObject).run(openapi).leftMap(Violations.root).andThen { obj =>
          Validated
            .fromOption(
              obj.get(identifier), {
                val constraint = identifiers.obj.contains.toConstraint(OpenApi.fromString(identifier).some)
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
                      val constraint = identifiers.obj.contains.toConstraint(OpenApi.fromString(value).some)
                      Violations.rootNec(Violation(constraint, obj))
                    }
                  )
                  .andThen(schema.value.decode)
                  .bimap(_.modifyHistory(value /: _), _.some)
              else none[B].valid
            }
        }
      case Some(Discriminator.Merged(identifier)) =>
        refine("OpenApi.Object")(_.asObject).run(openapi).leftMap(Violations.root).andThen { obj =>
          Validated
            .fromOption(
              obj.get(identifier), {
                val constraint = identifiers.obj.contains.toConstraint(OpenApi.fromString(identifier).some)
                Violations.rootNec(Violation(constraint, obj))
              }
            )
            .andThen(refine("OpenApi.Primitive")(_.asPrimitive).run(_).leftMap(Violations.root))
            .andThen(key.value.decode)
            .leftMap(_.modifyHistory(identifier /: _))
            .andThen { name =>
              if matches(name) then schema.value.decode(obj.remove(identifier)).map(_.some) else none[B].valid
            }
        }
      case Some(Discriminator.Keyed) =>
        refine("OpenApi.Object")(_.asObject).run(openapi).leftMap(Violations.root).andThen { obj =>
          val keyCodec = key.value.encode(name)
          val keyName = keyCodec.print

          Validated
            .fromOption(
              obj.get(keyName), {
                val constraint = identifiers.obj.contains.toConstraint(keyCodec.some)
                Violations.rootNec(Violation(constraint, obj))
              }
            )
            .andThen(refine("OpenApi.Primitive")(_.asPrimitive).run(_).leftMap(Violations.root))
            .andThen(key.value.decode)
            .andThen { name =>
              if matches(name) then schema.value.decode(obj.remove(keyName)).map(_.some) else none[B].valid
            }
            .leftMap(_.modifyHistory(keyName /: _))
        }
      case None => schema.value.decode(openapi).toOption.valid

  final def encode(b: B, discriminator: Option[Discriminator]): OpenApi = discriminator match
    case Some(Discriminator.Nested(identifier, value)) =>
      OpenApi.obj(identifier -> key.value.encode(name), value -> schema.value.encode(b))
    case Some(Discriminator.Merged(identifier)) =>
      schema.value.encode(b).asObject match
        case Some(obj) if obj.contains(identifier) => OpenApi.Object.Empty
        case Some(obj)                             => obj.deepMerge(OpenApi.obj(identifier -> key.value.encode(name)))
        case None                                  => OpenApi.Object.Empty
    case Some(Discriminator.Keyed) =>
      OpenApi.obj(key.value.encode(name).print -> schema.value.encode(b))
    case None => schema.value.encode(b)

object Branch:
  def apply[A: Eq, B](name: A, key: Eval[Schema.Of[A, OpenApi.Primitive]], schema: Eval[Schema[B]]): Branch[A, B] =
    new Branch[A, B](name, key, schema):
      override def matches(name: A): Boolean = this.name === name

  given [A]: InvariantValidation[Branch[A, *]] with
    override def imap[B, C](fa: Branch[A, B])(f: B => C)(g: C => B): Branch[A, C] = fa.imap(f)(g)
    override def ivalidate[B: Encoder, C, D](fa: Branch[A, C])(validation: Validation[B, C, C, D])(
        g: D => C
    ): Branch[A, D] = fa.ivalidate(validation)(g)
