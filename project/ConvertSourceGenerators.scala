object ConvertSourceGenerators {
  def sumInstances(pkg: String): String = {
    val instances = (2 to 22)
      .map { n =>
        val types = (1 to n).map(n => s"A$n")
        val values = (1 to n).map(n => s"a$n")
        val eithers = types.tail.foldLeft(types.head)((a, b) => s"Either[$a, $b]")
        val nested: (Int, String) => String = { (i, argument) =>
          val level = n - math.max(i, 1) - 1
          val right = i > 0

          ("Left(" * level) + (if (right) s"Right($argument)" else s"Left($argument)") + (")" * level)
        }

        s"""  given sum$n: [${types.map(tpe => s"$tpe <: B").mkString(", ")}, B] => (
           |    mirror: Mirror.SumOf[B],
           |    evidence: mirror.MirroredElemTypes =:= (${types.mkString(", ")})
           |  ) => Convert[$eithers, B]:
           |      override def to(a: $eithers): B = a match
           |        ${(0 until n).map(i => s"case ${nested(i, "b")} => b").mkString("\n        ")}
           |      override def from(b: B): $eithers =
           |        mirror.ordinal(b) match
           |          ${(0 until n)
            .map(i => s"case $i => ${nested(i, s"b.asInstanceOf[A${i + 1}]")}")
            .mkString("\n          ")}""".stripMargin
      }
      .mkString("\n\n")

    s"""package $pkg
       |
       |import scala.deriving.*
       |
       |trait ConvertInstances:
       |$instances""".stripMargin
  }
}
