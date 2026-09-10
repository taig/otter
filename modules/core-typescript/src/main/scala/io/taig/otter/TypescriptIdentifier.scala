package io.taig.otter

/** Portable declaration identifiers; object property names remain quoted verbatim. */
object TypescriptIdentifier:
  def apply(hint: String): String =
    val name = hint.map(character => if part(character) then character else '_')
    if name.isEmpty then "_"
    else if start(name.head) then name
    else "_" + name

  private def start(character: Char): Boolean =
    character >= 'a' && character <= 'z' || character >= 'A' && character <= 'Z' || character == '_' || character == '$'

  private def part(character: Char): Boolean = start(character) || character >= '0' && character <= '9'

  val Reserved: Set[String] = Set(
    "arguments",
    "as",
    "await",
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "enum",
    "eval",
    "export",
    "exports",
    "extends",
    "false",
    "finally",
    "for",
    "function",
    "if",
    "implements",
    "import",
    "in",
    "instanceof",
    "interface",
    "let",
    "new",
    "null",
    "package",
    "private",
    "protected",
    "public",
    "require",
    "return",
    "static",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "var",
    "void",
    "while",
    "with",
    "yield",
    "any",
    "bigint",
    "boolean",
    "intrinsic",
    "never",
    "number",
    "object",
    "string",
    "symbol",
    "undefined",
    "unknown"
  )
