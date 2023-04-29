//package io.taig.openapi.schema
//
//import cats.Eval
//import cats.Eq
//import cats.data.Validated
//import cats.syntax.all.*
//import io.taig.openapi.OpenApi
//import io.taig.openapi.schema.schemas.*
//import io.taig.validation.{Constraint, Validation, Violation}
//import io.taig.validation.validations
//
//final case class Branch[A, B](name: A, key: Eval[Value[A]], schema: Eval[Schema[B]]):
//  def renderName: String = key.value.render(name)
//
//  infix def orElse[C](branch: Branch[A, C]) = ???
//  infix def :+[C](branch: Branch[A, C]) = ???
//
//  def imap[C](f: B => C)(g: C => B): Branch[A, C] = Branch(name, key, schema.map(_.imap(f)(g)))
//  // TODO ivalidate, etc.
//
//  def toSum: Sum[A, B] = Sum(this)
//
//  def decode(openapi: OpenApi, discriminator: Discriminator): Validated[Violations, Option[B]] =
//    Branch.decode(this)(openapi, discriminator)
//
//  def encode(b: B, discriminator: Discriminator): OpenApi = Branch.encode(this)(b, discriminator)
//
//object Branch:
//  def decode[A, B](
//      branch: Branch[A, B]
//  )(openapi: OpenApi, discriminator: Discriminator): Validated[Violations, Option[B]] =
//    def refine[A](tpe: String)(f: OpenApi => Option[A]): Validation[OpenApi, OpenApi, OpenApi, A] =
//      validations.refine(tpe)(f).mapReference(OpenApi.fromString)
//
//    discriminator match
//      case Discriminator.Nested(identifier, value) =>
//        refine("OpenApi.Object")(_.asObject).run(openapi).leftMap(Violations.root).andThen { obj =>
//          Validated
//            .fromOption(
//              obj.get(identifier), {
//                val constraint = Constraint("object.contains", OpenApi.fromString(identifier).some)
//                Violations.rootNec(Violation(constraint, obj))
//              }
//            )
//            .andThen(refine("OpenApi.Primitive")(_.asPrimitive).run(_).leftMap(Violations.root))
//            .andThen(branch.key.value.decode)
//            .leftMap(_.modifyHistory(identifier /: _))
//            .andThen { name =>
//              if branch.renderName === branch.key.value.render(name) then
//                Validated
//                  .fromOption(
//                    obj.get(value), {
//                      val constraint = Constraint("object.contains", OpenApi.fromString(value).some)
//                      Violations.rootNec(Violation(constraint, obj))
//                    }
//                  )
//                  .andThen(branch.schema.value.decode)
//                  .bimap(_.modifyHistory(value /: _), _.some)
//              else none[B].valid
//            }
//        }
//      case Discriminator.Merged(identifier) => ???
//      case Discriminator.Keyed              => ???
//      case Discriminator.None               => branch.schema.value.decode(openapi).toOption.valid
//
//  def encode[A, B](branch: Branch[A, B])(b: B, discriminator: Discriminator): OpenApi = discriminator match
//    case Discriminator.Nested(identifier, value) =>
//      OpenApi.obj(identifier -> branch.key.value.encode(branch.name), value -> branch.schema.value.encode(b))
//    case Discriminator.Merged(identifier) =>
//      branch.schema.value.encode(b).asObject match
//        case Some(obj) if obj.contains(identifier) => OpenApi.Object.Empty
//        case Some(obj) => obj.deepMerge(OpenApi.obj(identifier -> branch.key.value.encode(branch.name)))
//        case None      => OpenApi.Object.Empty
//    case Discriminator.Keyed =>
//      OpenApi.obj(branch.renderName -> branch.schema.value.encode(b))
//    case Discriminator.None => branch.schema.value.encode(b)
