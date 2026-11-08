package io.taig.otter

import scala.quoted.*

private[otter] object ConvertMacros:
  def scalaUnion[L: Type, R: Type, B: Type](using Quotes): Expr[Convert[Either[L, R], B]] =
    import quotes.reflect.*

    val either = TypeRepr.of[Either[Any, Any]].typeSymbol

    def shape(tpe: TypeRepr): Expr[Convert.ScalaUnion.Shape] = tpe.dealias match
      case AppliedType(constructor, List(left, right)) if constructor.typeSymbol == either =>
        '{ Convert.ScalaUnion.Shape.Nest(${ shape(left) }, ${ shape(right) }) }
      case member =>
        (member.asType: @unchecked) match
          case '[a] =>
            val test = Expr.summon[Convert.UnionTest[a]].getOrElse(unionTest[a])
            '{ Convert.ScalaUnion.Shape.Member($test) }

    '{ new Convert.ScalaUnion[Either[L, R], B](${ shape(TypeRepr.of[Either[L, R]]) }) }

  def unionTest[A: Type](using Quotes): Expr[Convert.UnionTest[A]] =
    import quotes.reflect.*

    val member = TypeRepr.of[A].dealias
    val rendered = member.show

    def unsupported(reason: String): Nothing =
      report.errorAndAbort(
        s"Cannot automatically derive Convert.UnionTest[$rendered]: $reason. " +
          "Provide Convert.UnionTest.fromTypeTest with a sound test for this union member."
      )

    def validate(tpe: TypeRepr): Unit = tpe match
      case _: Refinement => unsupported("refined types are not checked as union members")
      case _: AndOrType  => unsupported("intersection and union types must be represented by separate schema branches")
      case _: MatchType  => unsupported("unreduced match types cannot be checked as union members")
      case AppliedType(_, args) if args.nonEmpty =>
        unsupported("its type arguments are erased at runtime")
      case TypeBounds(_, _) => unsupported("abstract type members do not have a concrete runtime test")
      case _ if tpe.typeSymbol.flags.is(Flags.Param) =>
        unsupported("abstract type parameters do not have a concrete runtime test")
      case _ if tpe.typeSymbol.flags.is(Flags.Module) || tpe.isSingleton => ()
      case _ if tpe.typeSymbol.isClassDef                                => ()
      case _ => unsupported("Scala cannot provide a sound nominal runtime test")

    validate(member)

    Expr.summon[scala.reflect.TypeTest[Any, A]] match
      case Some(test) => '{ Convert.UnionTest.fromTypeTest[A]($test) }
      case None       => unsupported("Scala has no runtime test for this member")
