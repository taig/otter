package io.taig.otter

import zio.Scope
import zio.test.*

import scala.deriving.Mirror

object ConvertTest extends ZIOSpecDefault:
  enum Three:
    case A, B, C

  /** More members than a generated instance set could ever cover. */
  enum Big:
    case C01, C02, C03, C04, C05, C06, C07, C08, C09, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21, C22,
      C23, C24, C25

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("ConvertTest")(
    test("the nesting matches the association of :+"):
      val convert = Convert[Either[Either[Three.A.type, Three.B.type], Three.C.type], Three]

      assertTrue(
        convert.from(Three.A) == Left(Left(Three.A)),
        convert.from(Three.B) == Left(Right(Three.B)),
        convert.from(Three.C) == Right(Three.C),
        convert.to(Left(Left(Three.A))) == Three.A,
        convert.to(Left(Right(Three.B))) == Three.B,
        convert.to(Right(Three.C)) == Three.C
      )
    ,
    test("a sum of 25 members round trips"):
      val mirror = summon[Mirror.SumOf[Big]]
      val convert = Convert[Convert.Coproduct[mirror.MirroredElemTypes], Big]

      assertTrue(Big.values.forall(value => convert.to(convert.from(value)) == value))
    ,
    test("every member of a 25 member sum lands at its own depth"):
      val mirror = summon[Mirror.SumOf[Big]]
      val convert = Convert[Convert.Coproduct[mirror.MirroredElemTypes], Big]

      def depth(value: Any): Int = value match
        case Left(inner)  => 1 + depth(inner)
        case Right(inner) => 1 + depth(inner)
        case _            => 0

      val depths = Big.values.toList.map(value => depth(convert.from(value)))

      assertTrue(depths == (24 :: (24 to 1 by -1).toList))
  )
