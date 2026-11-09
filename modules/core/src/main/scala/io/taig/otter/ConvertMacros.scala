package io.taig.otter

import scala.collection.mutable.ListBuffer
import scala.quoted.*

private[otter] object ConvertMacros:
  def scalaUnion[L: Type, R: Type, B: Type](using Quotes): Expr[Convert[Either[L, R], B]] =
    import quotes.reflect.*

    val either = TypeRepr.of[Either[Any, Any]].typeSymbol
    val definitions = ListBuffer.empty[ValDef]

    // One val per node rather than one nested expression: the compiler recurses into an expression as deep as it is
    // nested, and a hundred members overflow a 1 MB stack.
    def define(shape: Expr[Convert.ScalaUnion.Shape]): Expr[Convert.ScalaUnion.Shape] =
      val symbol = Symbol.newVal(
        parent = Symbol.spliceOwner,
        name = s"shape${definitions.length}",
        tpe = TypeRepr.of[Convert.ScalaUnion.Shape],
        flags = Flags.EmptyFlags,
        privateWithin = Symbol.noSymbol
      )
      definitions += ValDef(symbol, Some(shape.asTerm.changeOwner(symbol)))
      Ref(symbol).asExprOf[Convert.ScalaUnion.Shape]

    def shape(tpe: TypeRepr): Expr[Convert.ScalaUnion.Shape] = tpe.dealias match
      case AppliedType(constructor, List(left, right)) if constructor.typeSymbol == either =>
        val leftShape = shape(left)
        val rightShape = shape(right)
        define('{ Convert.ScalaUnion.Shape.Nest($leftShape, $rightShape) })
      case member =>
        (member.asType: @unchecked) match
          case '[a] =>
            val test = Expr.summon[Convert.UnionTest[a]].getOrElse(unionTest[a])
            define('{ Convert.ScalaUnion.Shape.Member($test) })

    val root = shape(TypeRepr.of[Either[L, R]])
    val result = '{ new Convert.ScalaUnion[Either[L, R], B]($root) }

    Block(definitions.toList, result.asTerm).asExprOf[Convert[Either[L, R], B]]

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
