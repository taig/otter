package io.taig.otter

import cats.data.NonEmptyChain
import cats.data.NonEmptyMap
import cats.implicits.*
import io.taig.validation.Violation
import zio.Scope
import zio.test.*

import scala.collection.immutable.SortedMap

/** Combining two trees, which is the whole of what a `Semigroup[Violations]` is for.
  *
  * Every decoder that accumulates reaches this: a record combines what its members reported, a union what its branches
  * did. The four cases are the four shapes the two operands can have, and the two mixed ones are the ones a `++` gets
  * wrong -- silently, and only when both sides carry the same [[Step]].
  */
object ViolationsTest extends ZIOSpecDefault:
  private def violation(name: String): Violation[Constraint] =
    Violation(constraint = Constraint.Generic.Type(name), actual = io.taig.data.Data.Null, hint = none)

  private def leaf(name: String): Violations = Violations(violation(name))

  private def root(name: String, values: (Step, Violations)*): Violations =
    Violations.Root(SortedMap.from(values), NonEmptyChain.one(violation(name)))

  private def namespace(values: (Step, Violations)*): Violations =
    Violations.Namespace(NonEmptyMap.of(values.head, values.tail*))

  private val step: Step = Step.Field("title")

  /** Every constraint in the tree, so an assertion can say what survived without spelling the shape out. */
  private def constraints(violations: Violations): List[Constraint] = violations match
    case Violations.Root(values, found) =>
      found.toList.map(_.constraint) ++ values.toList.flatMap((_, nested) => constraints(nested))
    case Violations.Namespace(values) => values.toSortedMap.toList.flatMap((_, nested) => constraints(nested))

  private def names(violations: Violations): List[String] = constraints(violations).collect:
    case Constraint.Generic.Type(name) => name

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("ViolationsTest")(
    test("two roots keep both their own violations and both their children"):
      val combined = root("a", step -> leaf("x")).combine(root("b", step -> leaf("y")))

      assertTrue(names(combined).sorted == List("a", "b", "x", "y"))
    ,
    test("two namespaces keep both children"):
      val combined = namespace(step -> leaf("x")).combine(namespace(step -> leaf("y")))

      assertTrue(names(combined).sorted == List("x", "y"))
    ,
    /** The case a `++` loses: the root has a child at `step` and so does the namespace, and one of them used to win
      * outright.
      */
    test("a root combined with a namespace keeps the child they share"):
      val combined = root("a", step -> leaf("x")).combine(namespace(step -> leaf("y")))

      assertTrue(names(combined).sorted == List("a", "x", "y"))
    ,
    test("a namespace combined with a root keeps the child they share"):
      val combined = namespace(step -> leaf("x")).combine(root("a", step -> leaf("y")))

      assertTrue(names(combined).sorted == List("a", "x", "y"))
    ,
    test("a step only one side has is carried through"):
      val other = Step.Field("pages")
      val combined = root("a", step -> leaf("x")).combine(namespace(other -> leaf("y")))

      assertTrue(names(combined).sorted == List("a", "x", "y"))
    ,
    /** Associativity is what a `Semigroup` promises, and it is what the losing cases broke: which subtree survived
      * depended on how the tree was folded.
      */
    test("combining is associative over the mixed shapes"):
      val left = root("a", step -> leaf("x"))
      val middle = namespace(step -> leaf("y"))
      val right = root("b", step -> leaf("z"))

      assertTrue(names(left.combine(middle).combine(right)).sorted == names(left.combine(middle.combine(right))).sorted)
  )
