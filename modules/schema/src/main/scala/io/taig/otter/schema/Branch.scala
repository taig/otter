package io.taig.otter.schema

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.OpenApi
import io.taig.otter.syntax.*

sealed abstract class Branch[A]:
  def key: String
  def schema: Schema[A]

  def decode(openapi: Option[OpenApi.Value], discriminator: Discriminator): Validated[Violations, Option[A]]
  def encode(a: A, discriminator: Discriminator): Option[OpenApi.Value]

//  def :+[C, D](other: Branch[C, D]): Coproduct[B + D] = toCoproduct :+ other
//  def +:[C, D](other: Branch[C, D]): Coproduct[D + B] = other +: toCoproduct
  def toCoproduct: Coproduct[A] = Coproduct(this)
  def to[B](using Evidence.Coproduct.Aux[B, A]): Coproduct[B] = toCoproduct.to[B]

object Branch extends ToBranchOps:
  def apply[A, B](name: A, a: => Schema.Value[A], b: => Schema[B]): Branch[B] = new Branch[B]:
    override def key: String = a.print(name).orEmpty
    override def schema: Schema[B] = b
    override def decode(
        openapi: Option[OpenApi.Value],
        discriminator: Discriminator
    ): Validated[Violations, Option[B]] = discriminator match
      case Discriminator.Nested(identifier, value) => ???
      case Discriminator.Merged(identifier)        => ???
      case Discriminator.Keyed                     => ???
      case Discriminator.None =>
        schema.decode(openapi) match
          case Validated.Valid(b)            => b.some.valid
          case Validated.Invalid(violations) => violations.modifyHistory(key /: _).invalid
    override def encode(b: B, discriminator: Discriminator): Option[OpenApi.Value] = discriminator match
      case Discriminator.Nested(identifier, value) => OpenApi.obj(identifier := key, value := schema.encode(b)).some
      case Discriminator.Merged(identifier) =>
        val payload = schema.encode(b).flatMap(_.asObject).getOrElse(OpenApi.Object.Empty)
        (payload ++ OpenApi.obj(identifier := key)).some
      case Discriminator.Keyed => OpenApi.obj(key := schema.encode(b)).some
      case Discriminator.None  => schema.encode(b)

//  def decode(otter: OpenApi, discriminator: Sum.Discriminator): Ior[Violations, Option[B]] = discriminator match
//    case Discriminator.Nested(identifier, value) =>
//      for
//        root <- validations.otter.obj.run(otter).leftMap(Violations.root).toIor
//        discriminator <- root
//          .get(identifier)
//          .toRightIor(Violations.oneNec(History.Root / identifier, Constraint.required.toViolation(OpenApi.Null)))
//        discriminator <- key.value.decode(discriminator).toIor
//        obj <- root
//          .get(value)
//          .toRightIor(Violations.oneNec(History.Root / value, Constraint.required.toViolation(OpenApi.Null)))
//        result <-
//          if renderName === key.value.render(discriminator)
//          then schema.value.decode(obj).bimap(_.modifyHistory(value /: _), _.some).toIor
//          else none[B].rightIor
//      yield result
//    case Discriminator.Merged(identifier) =>
//      for
//        root <- validations.otter.obj.run(otter).leftMap(Violations.root).toIor
//        discriminator <- root
//          .get(identifier)
//          .toRightIor(Violations.oneNec(History.Root / identifier, Constraint.required.toViolation(OpenApi.Null)))
//        discriminator <- key.value.decode(discriminator).toIor
//        result <-
//          if renderName === key.value.render(discriminator)
//          then schema.value.decode(root.remove(identifier)).map(_.some).toIor
//          else none[B].rightIor
//      yield result
//    case Discriminator.Keyed =>
//      validations.otter.obj
//        .run(otter)
//        .leftMap(Violations.root)
//        .toIor
//        .flatMap: root =>
//          root.get(renderName) match
//            case Some(otter) => schema.value.decode(otter).bimap(_.modifyHistory(renderName /: _), _.some).toIor
//            case None          => none[B].rightIor
//    case Discriminator.None =>
//      schema.value.decode(otter) match
//        case Validated.Valid(b)            => b.some.rightIor
//        case Validated.Invalid(violations) => violations.modifyHistory(renderName /: _).leftIor.putRight(none[B])
