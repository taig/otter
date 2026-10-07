package io.taig.otter

import zio.Scope
import zio.test.*

import scala.compiletime.asMatchable
import scala.compiletime.testing.typeCheckErrors
import scala.deriving.Mirror
import scala.reflect.TypeTest

object ConvertTest extends ZIOSpecDefault:
  enum Three:
    case A, B, C

  /** More members than a generated instance set could ever cover. */
  enum Big:
    case C01, C02, C03, C04, C05, C06, C07, C08, C09, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21, C22,
      C23, C24, C25

  enum Ten:
    case C01, C02, C03, C04, C05, C06, C07, C08, C09, C10

  type TenMembers = Ten.C01.type | Ten.C02.type | Ten.C03.type | Ten.C04.type | Ten.C05.type | Ten.C06.type |
    Ten.C07.type | Ten.C08.type | Ten.C09.type | Ten.C10.type

  /** More members than the compiler's inline depth, exercising non-inline union evidence. */
  enum Wide:
    case C001, C002, C003, C004, C005, C006, C007, C008, C009, C010, C011, C012, C013, C014, C015, C016, C017, C018,
      C019, C020, C021, C022, C023, C024, C025, C026, C027, C028, C029, C030, C031, C032, C033, C034, C035, C036, C037,
      C038, C039, C040, C041, C042, C043, C044, C045, C046, C047, C048, C049, C050, C051, C052, C053, C054, C055, C056,
      C057, C058, C059, C060, C061, C062, C063, C064, C065, C066, C067, C068, C069, C070, C071, C072, C073, C074, C075,
      C076, C077, C078, C079, C080, C081, C082, C083, C084, C085, C086, C087, C088, C089, C090, C091, C092, C093, C094,
      C095, C096, C097, C098, C099, C100

  type WideMembers = Wide.C001.type | Wide.C002.type | Wide.C003.type | Wide.C004.type | Wide.C005.type |
    Wide.C006.type | Wide.C007.type | Wide.C008.type | Wide.C009.type | Wide.C010.type | Wide.C011.type |
    Wide.C012.type | Wide.C013.type | Wide.C014.type | Wide.C015.type | Wide.C016.type | Wide.C017.type |
    Wide.C018.type | Wide.C019.type | Wide.C020.type | Wide.C021.type | Wide.C022.type | Wide.C023.type |
    Wide.C024.type | Wide.C025.type | Wide.C026.type | Wide.C027.type | Wide.C028.type | Wide.C029.type |
    Wide.C030.type | Wide.C031.type | Wide.C032.type | Wide.C033.type | Wide.C034.type | Wide.C035.type |
    Wide.C036.type | Wide.C037.type | Wide.C038.type | Wide.C039.type | Wide.C040.type | Wide.C041.type |
    Wide.C042.type | Wide.C043.type | Wide.C044.type | Wide.C045.type | Wide.C046.type | Wide.C047.type |
    Wide.C048.type | Wide.C049.type | Wide.C050.type | Wide.C051.type | Wide.C052.type | Wide.C053.type |
    Wide.C054.type | Wide.C055.type | Wide.C056.type | Wide.C057.type | Wide.C058.type | Wide.C059.type |
    Wide.C060.type | Wide.C061.type | Wide.C062.type | Wide.C063.type | Wide.C064.type | Wide.C065.type |
    Wide.C066.type | Wide.C067.type | Wide.C068.type | Wide.C069.type | Wide.C070.type | Wide.C071.type |
    Wide.C072.type | Wide.C073.type | Wide.C074.type | Wide.C075.type | Wide.C076.type | Wide.C077.type |
    Wide.C078.type | Wide.C079.type | Wide.C080.type | Wide.C081.type | Wide.C082.type | Wide.C083.type |
    Wide.C084.type | Wide.C085.type | Wide.C086.type | Wide.C087.type | Wide.C088.type | Wide.C089.type |
    Wide.C090.type | Wide.C091.type | Wide.C092.type | Wide.C093.type | Wide.C094.type | Wide.C095.type |
    Wide.C096.type | Wide.C097.type | Wide.C098.type | Wide.C099.type | Wide.C100.type

  final case class Empty()

  final case class TextMember(value: String)
  final case class NumberMember(value: Int)
  final case class FlagMember(value: Boolean)

  trait FirstTag
  trait SecondTag
  final case class BothTags() extends FirstTag, SecondTag

  type ThreeMembers = TextMember | NumberMember | FlagMember
  type TagMembers = FirstTag | SecondTag

  override val spec: Spec[TestEnvironment & Scope, Any] = suite("ConvertTest")(
    test("an enum case declared without parameters converts from and to Unit"):
      val convert = Convert[Unit, Three.A.type]

      assertTrue(convert.to(()) == Three.A, convert.from(Three.A) == ())
    ,
    test("an empty case class converts from and to Unit"):
      val convert = Convert[Unit, Empty]

      assertTrue(convert.to(()) == Empty(), convert.from(Empty()) == ())
    ,
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
    test("two branches, the second carrying nothing, convert from and to Option"):
      val convert = Convert[Either[Int, Unit], Option[Int]]

      assertTrue(
        convert.to(Left(42)) == Some(42),
        convert.to(Right(())) == None,
        convert.from(Some(42)) == Left(42),
        convert.from(None) == Right(())
      )
    ,
    test("a sum of 25 members round trips"):
      val mirror = summon[Mirror.SumOf[Big]]
      val convert = Convert[Convert.Coproduct[mirror.MirroredElemTypes], Big]

      assertTrue(Big.values.forall(value => convert.to(convert.from(value)) == value))
    ,
    test("a three-branch Either converts to a Scala union"):
      val convert = Convert[Either[Either[TextMember, NumberMember], FlagMember], ThreeMembers]
      val values: List[ThreeMembers] = List(TextMember("x"), NumberMember(2), FlagMember(true))

      assertTrue(
        values.map(convert.from) == List(
          Left(Left(TextMember("x"))),
          Left(Right(NumberMember(2))),
          Right(FlagMember(true))
        ),
        values.map(value => convert.to(convert.from(value))) == values
      )
    ,
    test("singletons convert to and from a union"):
      val convert = Convert[Either[Three.A.type, Three.B.type], Three.A.type | Three.B.type]

      assertTrue(
        convert.from(Three.A) == Left(Three.A),
        convert.from(Three.B) == Right(Three.B),
        convert.to(Left(Three.A)) == Three.A,
        convert.to(Right(Three.B)) == Three.B
      )
    ,
    test("the first matching overlapping branch wins"):
      val first = Convert[Either[FirstTag, SecondTag], TagMembers]
      val reversed = Convert[Either[SecondTag, FirstTag], TagMembers]
      val value: TagMembers = BothTags()

      assertTrue(first.from(value) == Left(value), reversed.from(value) == Left(value))
    ,
    test("an explicit TypeTest supports an erased generic member"):
      given intListUnionTest: Convert.UnionTest[List[Int]] = Convert.UnionTest.fromTypeTest(
        new TypeTest[Any, List[Int]]:
          @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
          override def unapply(value: Any): Option[value.type & List[Int]] = value.asMatchable match
            case values: List[?]
                if values.forall(value =>
                  value.asMatchable match
                    case _: Int => true
                    case _      => false
                ) =>
              Some(values.asInstanceOf[value.type & List[Int]])
            case _ => None
      )
      val convert = Convert[Either[List[Int], NumberMember], List[Int] | NumberMember]
      val values: List[List[Int] | NumberMember] = List(List(1, 2), NumberMember(3))

      assertTrue(values.map(value => convert.to(convert.from(value))) == values)
    ,
    test("overlapping explicit generic tests use schema order"):
      given Convert.UnionTest[List[Int]] = Convert.UnionTest.fromTypeTest(
        new TypeTest[Any, List[Int]]:
          @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
          override def unapply(value: Any): Option[value.type & List[Int]] = value.asMatchable match
            case values: List[?]
                if values.forall(value =>
                  value.asMatchable match
                    case _: Int => true
                    case _      => false
                ) =>
              Some(values.asInstanceOf[value.type & List[Int]])
            case _ => None
      )
      given stringListUnionTest: Convert.UnionTest[List[String]] = Convert.UnionTest.fromTypeTest(
        new TypeTest[Any, List[String]]:
          @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
          override def unapply(value: Any): Option[value.type & List[String]] = value.asMatchable match
            case values: List[?]
                if values.forall(value =>
                  value.asMatchable match
                    case _: String => true
                    case _         => false
                ) =>
              Some(values.asInstanceOf[value.type & List[String]])
            case _ => None
      )
      val convert = Convert[Either[List[Int], List[String]], List[Int] | List[String]]
      val empty: List[Int] | List[String] = Nil

      assertTrue(convert.from(empty).isLeft)
    ,
    test("erased generic members report the explicit evidence requirement"):
      val errors = typeCheckErrors("""
        import io.taig.otter.Convert
        Convert[Either[List[Int], String], List[Int] | String]
      """)

      assertTrue(errors.exists(_.message.contains("type arguments are erased")))
    ,
    test("a target with missing branches is rejected"):
      val errors = typeCheckErrors("""
        import io.taig.otter.Convert
        final case class LeftMember()
        final case class RightMember()
        final case class MissingMember()
        Convert[Either[LeftMember, RightMember], LeftMember | MissingMember]
      """)

      assertTrue(errors.nonEmpty)
    ,
    test("a nominal sum with branches out of declaration order stays rejected"):
      val errors = typeCheckErrors("""
        import io.taig.otter.Convert
        import io.taig.otter.ConvertTest.Three
        Convert[Either[Three.B.type, Three.A.type], Three]
      """)

      assertTrue(errors.nonEmpty)
    ,
    test("ten union members match the existing enum sum conversion") {
      val mirror = summon[Mirror.SumOf[Ten]]
      val union = Convert[Convert.Coproduct[mirror.MirroredElemTypes], TenMembers]
      val sum = Convert[Convert.Coproduct[mirror.MirroredElemTypes], Ten]
      val values: List[TenMembers] =
        List(Ten.C01, Ten.C02, Ten.C03, Ten.C04, Ten.C05, Ten.C06, Ten.C07, Ten.C08, Ten.C09, Ten.C10)

      assertTrue(
        values.forall(value => union.to(union.from(value)) == value),
        Ten.values.forall(value => sum.to(sum.from(value)) == value)
      )
    },
    test("hundred member union evidence compiles beyond the inline expansion limit") {
      val mirror = summon[Mirror.SumOf[Wide]]
      val convert = Convert[Convert.Coproduct[mirror.MirroredElemTypes], WideMembers]
      val sumConvert = Convert[Convert.Coproduct[mirror.MirroredElemTypes], Wide]
      val values: List[WideMembers] = List(Wide.C001, Wide.C050, Wide.C100)

      assertTrue(
        values.forall(value => convert.to(convert.from(value)) == value),
        Wide.values.forall(value => sumConvert.to(sumConvert.from(value)) == value)
      )
    },
    test("every member of a 25 member sum lands at its own depth"):
      val mirror = summon[Mirror.SumOf[Big]]
      val convert = Convert[Convert.Coproduct[mirror.MirroredElemTypes], Big]

      def depth(value: Matchable): Int = value match
        case Left(inner)  => 1 + depth(inner.asMatchable)
        case Right(inner) => 1 + depth(inner.asMatchable)
        case _            => 0

      val depths = Big.values.toList.map(value => depth(convert.from(value)))

      assertTrue(depths == (24 :: (24 to 1 by -1).toList))
  )
