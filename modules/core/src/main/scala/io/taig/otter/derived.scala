package io.taig.otter

import cats.Functor
import scala.quoted.*
import io.taig.otter.syntax.all.*

// This actually works but needs improvemnents and cleanups to become useful

extension (self: Functor.type) inline def derived[F[_]]: Functor[F] = ${ Impl.derivedMacro[F] }

object Impl:
  def derivedMacro[F[_]: Type](using Quotes): Expr[Functor[F]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[F]

    val self = tpe.typeSymbol.methodMember("self") match
      case head :: Nil => head
      case _           => report.errorAndAbort(s"Type ${tpe.typeSymbol.name} must have exactly one `self` method")

    // Get the return type of `self: Annotation[Inner[..., A]]` by applying
    // a dummy type parameter to get the full method signature
    val selfType = tpe
      .appliedTo(TypeRepr.of[Any])
      .memberType(self)
      .match
        case MethodType(_, _, returnType) => returnType
        case returnType                   => returnType
      .match
        case ByNameType(underlying) => underlying
        case t                      => t

    // Extract the inner type and build a type lambda
    selfType match
      case AppliedType(annotationTypeConstructor, List(innerType)) =>
        // innerType is Self.Constant.Read[Value.Primitive.Text.Read, A] (with A = Any)
        // We need to find the type parameter position and build a type lambda

        // Get the companion object of F for the apply method
        val companion = tpe.typeSymbol.companionModule
        if companion == Symbol.noSymbol then
          report.errorAndAbort(s"Type ${tpe.typeSymbol.name} must have a companion object with an apply method")

        // Build the type lambda [a] =>> Annotation[Inner[..., a]]
        // We need to replace the last type argument (which is `Any` currently) with a fresh type param
        innerType match
          case AppliedType(innerConstructor, innerArgs) if innerArgs.nonEmpty =>
            // innerArgs.last should be the type that corresponds to A
            // We'll create a type lambda by abstracting over the last type argument

            val innerArgsInit = innerArgs.init

            // Create the type lambda: [a] =>> Annotation[InnerConstructor[innerArgsInit..., a]]
            val annotationTypeLambda = TypeLambda(
              List("a"),
              _ => List(TypeBounds.empty),
              tl =>
                AppliedType(
                  annotationTypeConstructor,
                  List(AppliedType(innerConstructor, innerArgsInit :+ tl.param(0)))
                )
            )

            val companionRef = Ref(companion)

            val applyMethod = companion
              .methodMember("apply")
              .headOption
              .getOrElse(report.errorAndAbort(s"Companion of ${tpe.typeSymbol.name} must have an apply method"))

            annotationTypeLambda.asType match
              case '[type g[a]; g] =>
                // Summon Functor[g] directly
                Expr.summon[Functor[g]] match
                  case Some(gFunctor) =>
                    // Generate: gFunctor.imapK([A] => (annotation: g[A]) => F.apply(annotation))([A] => (value: F[A]) => value.self)
                    '{
                      $gFunctor.imapK([A] =>
                        (annotation: g[A]) =>
                          ${
                            // Call F.apply(annotation)
                            val annotationExpr = '{ annotation }.asTerm
                            Apply(
                              TypeApply(Select(companionRef, applyMethod), List(TypeTree.of[A])),
                              List(annotationExpr)
                            ).asExprOf[F[A]]
                          }
                      )([A] =>
                        (value: F[A]) =>
                          ${
                            // Call value.self
                            val valueExpr = '{ value }.asTerm
                            Select(valueExpr, self).asExprOf[g[A]]
                          }
                      )
                    }
                  case None =>
                    report.errorAndAbort(
                      s"Could not find Functor instance for annotation type: ${annotationTypeLambda.show}"
                    )

          case _ =>
            report.errorAndAbort(
              s"Inner type of Annotation must be an applied type with type arguments, got: $innerType"
            )

      case _ =>
        report.errorAndAbort(s"Return type of `self` must be Annotation[...], got: $selfType")
