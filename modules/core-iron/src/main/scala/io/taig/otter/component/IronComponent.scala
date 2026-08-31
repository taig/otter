package io.taig.otter.component

import cats.data.Chain
import io.github.iltotore.iron.*
import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.otter.operation.CollectionOperation
import io.taig.otter.operation.PrimitiveOperation
import io.taig.validation.Validation
import io.taig.validation.iron.DerivedValidation

/** The iron vocabulary: a schema whose type carries the constraint its validation checks.
  *
  * Each member builds the very node its plain counterpart in [[PrimitiveComponent]] and [[CollectionComponent]] builds,
  * out of the [[io.taig.validation.iron.DerivedValidation]] of the demanded constraint, and hands it back refined. Mix
  * it in next to a format's own vocabulary, in a namespace of its own so that the refined and the unrefined member of a
  * name never meet in overload resolution:
  *
  * ```scala
  * object json extends JsonComponent:
  *   object refined
  *       extends IronComponent.Number[Json.Primitive.Number.Schema],
  *         IronComponent.Text[Json.Primitive.Text.Schema],
  *         IronComponent.Collection[Json.Node, Json.Collection.Schema]
  *
  * json.field("title", json.refined.string[MinLength[1] & MaxLength[64]])
  * json.field("pages", json.refined.int[Greater[0]])
  * json.field("tags", json.refined.list[MaxLength[10]](json.string))
  * ```
  *
  * The refinement is a cast, and it is the only thing these members add. The write side never needed one: `B :| A` is a
  * subtype of `B`, which a contravariant slot already accepts. The read side is the claim, and it holds because the
  * validation the node carries is the derivation of `A` itself, so a value that fails `A` never leaves a decoder. Iron
  * erases `B :| A` to `B`, so there is no coercion left to perform at runtime.
  */
object IronComponent:
  /** Only the types a numeric literal inhabits are here. [[io.taig.validation.iron.DerivedValidation]] reflects
    * `Greater[B]` for a `B <: A`, so `Greater[0]` says nothing about a `BigDecimal` and fails to derive at all. Those
    * take a hand written validation through [[PrimitiveComponent.Number]] instead.
    */
  trait Number[F[-_, +_]](using F: PrimitiveOperation.Number[F]):
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    def double[A](using
        validation: DerivedValidation[Constraint.Primitive.Number, Double, A]
    ): F[Double :| A, Double :| A] = F.double(validation).asInstanceOf[F[Double :| A, Double :| A]]

    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    def float[A](using
        validation: DerivedValidation[Constraint.Primitive.Number, Float, A]
    ): F[Float :| A, Float :| A] = F.float(validation).asInstanceOf[F[Float :| A, Float :| A]]

    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    def int[A](using validation: DerivedValidation[Constraint.Primitive.Number, Int, A]): F[Int :| A, Int :| A] =
      F.int(validation).asInstanceOf[F[Int :| A, Int :| A]]

    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    def long[A](using validation: DerivedValidation[Constraint.Primitive.Number, Long, A]): F[Long :| A, Long :| A] =
      F.long(validation).asInstanceOf[F[Long :| A, Long :| A]]

  /** [[text]] takes the constructor rather than naming the carrier, so a text carried as something other than a
    * `String` -- a `CIString`, a wrapper of your own -- refines without this module having to know the type. Anything
    * of the shape `Validation => F[B, B]` fits, [[string]] being that constructor at `String`:
    *
    * ```scala
    * json.refined.text[Match[Email.Pattern] & MaxLength[64]](json.ciString)
    * ```
    *
    * The derivation asks the carrier for a `Count`, and a pattern constraint asks it for a `Matches` and an `Encoder`.
    * `io.taig.validation.cistring` carries all three for `CIString`.
    */
  trait Text[F[-_, +_]](using F: PrimitiveOperation.Text[F]):
    final class text[A]:
      @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
      def apply[B](schema: Validation[Constraint.Primitive.Text, B] => F[B, B])(using
          validation: DerivedValidation[Constraint.Primitive.Text, B, A]
      ): F[B :| A, B :| A] = schema(validation).asInstanceOf[F[B :| A, B :| A]]

    object text:
      def apply[A]: text[A] = new text[A]

    def string[A](using DerivedValidation[Constraint.Primitive.Text, String, A]): F[String :| A, String :| A] =
      text[A](F.string)

  /** Each member is applied twice, because the constraint is given where the element schema is inferred and Scala has
    * no partial type application: `list[MaxLength[10]](json.string)` is `list.apply[MaxLength[10]].apply(json.string)`.
    */
  trait Collection[Bound[-_, +_], F[_[-w, +r] <: Bound[w, r], -_, +_]]:
    final class chain[A]:
      @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
      def apply[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R])(using
          F: CollectionOperation[[w, r] =>> F[S, w, r], S],
          validation: DerivedValidation[Constraint.Collection, Chain[R], A]
      ): F[S, Chain[W] :| A, Chain[R] :| A] =
        F.chained(Reference.later(schema), validation).asInstanceOf[F[S, Chain[W] :| A, Chain[R] :| A]]

    object chain:
      def apply[A]: chain[A] = new chain[A]

    final class vector[A]:
      @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
      def apply[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R])(using
          F: CollectionOperation[[w, r] =>> F[S, w, r], S],
          validation: DerivedValidation[Constraint.Collection, Vector[R], A]
      ): F[S, Vector[W] :| A, Vector[R] :| A] =
        F.indexed(Reference.later(schema), validation).asInstanceOf[F[S, Vector[W] :| A, Vector[R] :| A]]

    object vector:
      def apply[A]: vector[A] = new vector[A]

    final class list[A]:
      @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
      def apply[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R])(using
          F: CollectionOperation[[w, r] =>> F[S, w, r], S],
          validation: DerivedValidation[Constraint.Collection, List[R], A]
      ): F[S, List[W] :| A, List[R] :| A] =
        F.linked(Reference.later(schema), validation).asInstanceOf[F[S, List[W] :| A, List[R] :| A]]

    object list:
      def apply[A]: list[A] = new list[A]
