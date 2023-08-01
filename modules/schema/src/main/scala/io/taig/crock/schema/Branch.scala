//package io.taig.crock.schema
//
//import cats.Eval
//import cats.data.{Ior, Validated}
//import cats.syntax.all.*
//import io.taig.crock.schema.Sum.Discriminator
//import io.taig.crock.{History, OpenApi}
//import io.taig.crock.validation.Constraint
//
//final case class Branch[A, B](name: A, key: Eval[Schema.Value[A]], schema: Eval[Schema[B]]):
//  def renderName: String = key.value.render(name)
//
//  infix def orElse[C](branch: Branch[A, C]): Sum[A, B + C] = toSum orElse branch.toSum
//  infix def :+[C](branch: Branch[A, C]): Sum[A, B + C] = toSum :+ branch
//
//  def toSum: Sum[A, B] = Sum(this)
//  def to[C](using Evidence.Sum.Aux[C, B]): Sum[A, C] = toSum.to[C]
//
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
//
//  def encode(b: B, discriminator: Sum.Discriminator): OpenApi = discriminator match
//    case Sum.Discriminator.Nested(identifier, value) =>
//      OpenApi.obj(identifier -> key.value.encode(name), value -> schema.value.encode(b))
//    case Sum.Discriminator.Merged(identifier) =>
//      schema.value.encode(b).asObject match
//        case Some(obj) if obj.contains(identifier) => OpenApi.Object.Empty
//        case Some(obj)                             => obj.deepMerge(OpenApi.obj(identifier -> key.value.encode(name)))
//        case None                                  => OpenApi.Object.Empty
//    case Sum.Discriminator.Keyed => OpenApi.obj(renderName -> schema.value.encode(b))
//    case Sum.Discriminator.None  => schema.value.encode(b)
