package consumer

import io.taig.otter.Convert
import zio.Scope
import zio.test.*

/** Outside of `io.taig.otter`, where a macro's expansion only reaches what is public. */
object ConvertConsumerTest extends ZIOSpecDefault:
  final case class First(value: Int)

  final case class Second(value: String)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Convert, from outside of otter")(
    test("a union converts where the macro expands"):
      val convert = Convert[Either[First, Second], First | Second]

      assertTrue(convert.from(Second("x")) == Right(Second("x")), convert.to(Left(First(1))) == First(1))
  )
