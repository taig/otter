object SchemaSourceGenerators {
  def sumInstances(pkg: String): String = {
    val instances = (2 to 22)
      .map { n =>
        val types = (1 to n).map(n => s"B$n")
        val values = (1 to n).map(n => s"b$n")
        val nested: (Int, String) => String = { (i, argument) =>
          val level = n - math.max(i, 1) - 1
          val right = i > 0

          ("Left(" * level) + (if (right) s"Right($argument)" else s"Left($argument)") + (")" * level)
        }

        s"""  given sum$n[A, ${types.map(tpe => s"$tpe <: A").mkString(", ")}](using
           |    mirror: Mirror.SumOf[A],
           |    evidence: mirror.MirroredElemTypes =:= (${types.mkString(", ")})
           |  ): Evidence.Sum.Aux[A, ${types.tail.foldLeft(types.head)((l, r) => s"Either[$l, $r]")}] =
           |    Evidence.Sum.instance[A, ${types.tail.foldLeft(types.head)((l, r) => s"Either[$l, $r]")}] { a =>
           |      mirror.ordinal(a) match
           |        ${(0 until n)
            .map(i => s"case $i => ${nested(i, s"a.asInstanceOf[B${i + 1}]")}")
            .mkString("\n        ")}
           |    } {
           |      ${(0 until n).map(i => s"case ${nested(i, "b")} => b").mkString("\n      ")}
           |    }""".stripMargin
      }
      .mkString("\n\n")

    s"""package $pkg
       |
       |import scala.deriving.*
       |
       |trait SumInstances:
       |$instances""".stripMargin
  }
}
