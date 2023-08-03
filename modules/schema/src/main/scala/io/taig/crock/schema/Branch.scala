package io.taig.crock.schema

import cats.Eval
import cats.syntax.all.*

final case class Branch[A, B](key: Eval[Schema.Value[A]], name: A, schema: Eval[Schema[B]]):
  def :+[C, D](other: Branch[C, D]): Coproduct[B + D] = toCoproduct :+ other
  def +:[C, D](other: Branch[C, D]): Coproduct[D + B] = other +: toCoproduct

  def toCoproduct: Coproduct[B] = Schema.Coproduct(this)
  def to[C](using Evidence.Coproduct.Aux[C, B]): Coproduct[C] = toCoproduct.to[C]

object Branch:
  extension [A, B <: Matchable](self: Branch[A, B])
    inline def |[C, D <: Matchable](other: Branch[C, D]): Coproduct[B | D] = (self :+ other).imap[B | D] {
      case Left(b)  => b
      case Right(d) => d
    } {
      case b: B => Left(b)
      case d: D => Right(d)
    }

//  def decode(crock: OpenApi, discriminator: Sum.Discriminator): Ior[Violations, Option[B]] = discriminator match
//    case Discriminator.Nested(identifier, value) =>
//      for
//        root <- validations.crock.obj.run(crock).leftMap(Violations.root).toIor
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
//        root <- validations.crock.obj.run(crock).leftMap(Violations.root).toIor
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
//      validations.crock.obj
//        .run(crock)
//        .leftMap(Violations.root)
//        .toIor
//        .flatMap: root =>
//          root.get(renderName) match
//            case Some(crock) => schema.value.decode(crock).bimap(_.modifyHistory(renderName /: _), _.some).toIor
//            case None          => none[B].rightIor
//    case Discriminator.None =>
//      schema.value.decode(crock) match
//        case Validated.Valid(b)            => b.some.rightIor
//        case Validated.Invalid(violations) => violations.modifyHistory(renderName /: _).leftIor.putRight(none[B])
