package io.taig.openapi.schema

import cats.Eval
import cats.data.{Ior, Validated}
import cats.syntax.all.*
import io.taig.openapi.schema.Sum.Discriminator
import io.taig.openapi.{History, OpenApi}
import io.taig.openapi.validation.Constraint

final case class Branch[A, B](name: A, key: Eval[Value[A]], schema: Eval[Schema[B]]):
  def renderName: String = key.value.render(name)

  infix def orElse[C](branch: Branch[A, C]): Sum[A, B + C] = toSum orElse branch.toSum
  infix def :+[C](branch: Branch[A, C]): Sum[A, B + C] = toSum :+ branch

  def toSum: Sum[A, B] = Sum(this)
  def to[C](using Evidence.Sum.Aux[C, B]): Sum[A, C] = toSum.to[C]

  def decode(openapi: OpenApi, discriminator: Sum.Discriminator): Ior[Violations, Option[B]] = discriminator match {
    case Discriminator.Nested(identifier, value) => ???
//      validations.openapi.obj
//        .run(openapi)
//        .leftMap(Violations.root)
//        .andThen: obj =>
//          Validated
//            .fromOption(obj.get(identifier), Violations.rootNec(Constraint.required.toViolation(OpenApi.Null)))
//            .andThen(validations.openapi.primitive.run(_).leftMap(Violations.root))
//            .andThen(key.value.decode)
//            .leftMap(_.modifyHistory(identifier /: _))
//            .andThen: name =>
//              if renderName === key.value.render(name) then
//                Validated
//                  .fromOption(obj.get(value), Violations.rootNec(Constraint.required.toViolation(OpenApi.Null)))
//                  .andThen(schema.value.decode)
//                  .bimap(_.modifyHistory(value /: _), _.some)
//              else none[B].valid
    case Discriminator.Merged(identifier) => ???
    case Discriminator.Keyed              => ???
    case Discriminator.None               => ???
  }

//  def decode(openapi: OpenApi, discriminator: Sum.Discriminator): Validated[Violations, Option[B]] = discriminator match
//    case Sum.Discriminator.Nested(identifier, value) =>
//      validations.openapi.obj
//        .run(openapi)
//        .leftMap(Violations.root)
//        .andThen: obj =>
//          Validated
//            .fromOption(obj.get(identifier), Violations.rootNec(Constraint.required.toViolation(OpenApi.Null)))
//            .andThen(validations.openapi.primitive.run(_).leftMap(Violations.root))
//            .andThen(key.value.decode)
//            .leftMap(_.modifyHistory(identifier /: _))
//            .andThen: name =>
//              if renderName === key.value.render(name) then
//                Validated
//                  .fromOption(obj.get(value), Violations.rootNec(Constraint.required.toViolation(OpenApi.Null)))
//                  .andThen(schema.value.decode)
//                  .bimap(_.modifyHistory(value /: _), _.some)
//              else none[B].valid
//    case Sum.Discriminator.Merged(identifier) =>
//      validations.openapi.obj
//        .run(openapi)
//        .leftMap(Violations.root)
//        .andThen: obj =>
//          Validated
//            .fromOption(obj.get(identifier), Violations.rootNec(Constraint.required.toViolation(obj)))
//            .andThen(validations.openapi.primitive.run(_).leftMap(Violations.root))
//            .andThen(key.value.decode)
//            .leftMap(_.modifyHistory(identifier /: _))
//            .andThen: name =>
//              if renderName === key.value.render(name)
//              then schema.value.decode(obj.remove(identifier)).map(_.some)
//              else none[B].valid
//    case Sum.Discriminator.Keyed =>
//      validations.openapi.obj
//        .run(openapi)
//        .leftMap(Violations.root)
//        .andThen: obj =>
//          Validated
//            .fromOption(obj.get(renderName), Violations.rootNec(Constraint.required.toViolation(obj)))
//            .andThen(validations.openapi.primitive.run(_).leftMap(Violations.root))
//            .andThen(key.value.decode)
//            .andThen: name =>
//              if renderName === key.value.render(name)
//              then schema.value.decode(obj.remove(renderName)).map(_.some)
//              else none[B].valid
//            .leftMap(_.modifyHistory(renderName /: _))
//    case Sum.Discriminator.None => schema.value.decode(openapi).toOption.valid

  def encode(b: B, discriminator: Sum.Discriminator): OpenApi = discriminator match
    case Sum.Discriminator.Nested(identifier, value) =>
      OpenApi.obj(identifier -> key.value.encode(name), value -> schema.value.encode(b))
    case Sum.Discriminator.Merged(identifier) =>
      schema.value.encode(b).asObject match
        case Some(obj) if obj.contains(identifier) => OpenApi.Object.Empty
        case Some(obj)                             => obj.deepMerge(OpenApi.obj(identifier -> key.value.encode(name)))
        case None                                  => OpenApi.Object.Empty
    case Sum.Discriminator.Keyed => OpenApi.obj(renderName -> schema.value.encode(b))
    case Sum.Discriminator.None  => schema.value.encode(b)
