package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter as Self
import io.taig.otter.Side
import io.taig.otter.Typescript
import io.taig.otter.http.Headers
import io.taig.otter.http.Method
import io.taig.otter.http.Parameter
import io.taig.otter.http.Path
import io.taig.otter.http.Queries
import io.taig.otter.http.Request

/** The half of a generated endpoint that is the same whatever library reads its payloads.
  *
  * An envelope is text and nothing more -- which is the statement `http` is built on -- so what a descriptor says about
  * one needs no target vocabulary at all: a path is a list of pieces, a query string and a header set are name and
  * value pairs, and a method is a word. Only the payloads are written in a library's language, and they are the only
  * thing this object leaves to a caller.
  */
object TypescriptEnvelope:
  /** The parameter the generated builders read their input from. */
  val Input: Typescript.Expression = Typescript.Expression.Symbol("input")

  /** What an input's sections are called.
    *
    * Sectioned rather than flat because a path parameter and a query parameter may share a name, and an input that put
    * them side by side would silently keep one of the two.
    */
  object Section:
    val Path: String = "path"
    val Query: String = "query"
    val Headers: String = "headers"
    val Body: String = "body"

  /** The pieces of a path, as the expression that builds them from an input.
    *
    * A literal segment is its own text, and a dynamic one is read out of the input and written through the schema that
    * describes it. What comes back is an array and not a joined string, because how the pieces are separated -- and
    * what they are joined onto -- is the caller's, and a caller that has to split a string apart again has been handed
    * the wrong thing.
    */
  def path(schema: Path.Node[?, ?]): Typescript.Expression =
    Typescript.Expression.Array(
      PathTemplate(schema).toList.map:
        case Left(literal)          => Typescript.Expression.Literal.String(literal)
        case Right((name, segment)) =>
          ParameterTypescriptRenderer.text(segment, TypescriptEnvelope.read(Section.Path, name))
    )

  /** The type of the path section of an input, or nothing when the path spells no holes. */
  def pathType(schema: Path.Node[?, ?]): Option[Typescript.Type] =
    val fields = PathTemplate
      .placeholders(schema)
      .toList
      .map((name, segment) =>
        Typescript.Type.Field(name, ParameterTypescriptRenderer.render(segment), optional = false)
      )

    Option.when(fields.nonEmpty)(Typescript.Type.Object(fields))

  /** A query string or a header set, as the expression that builds it from an input.
    *
    * An object rather than a list of pairs, and `undefined` where a parameter was not given: that is what a caller
    * building a `URLSearchParams` or a `Headers` already has to skip, and saying it as an object is what keeps the
    * generated builder readable.
    */
  def parameters(
      section: String,
      fields: Chain[Self.Field[Parameter.Node, ?, ?]],
      side: Side
  ): Typescript.Expression =
    Typescript.Expression.Object(
      fields.toList.map: field =>
        val read = TypescriptEnvelope.read(section, field.name)
        val written = ParameterTypescriptRenderer.text(field.schema.value, read)

        val value =
          if TypescriptEnvelope.isOptional(field, side) then
            Typescript.Expression.Ternary(
              Typescript.Expression.TripleEqual(read, Typescript.Expression.Undefined),
              Typescript.Expression.Undefined,
              written
            )
          else written

        (field.name, value)
    )

  /** The type of a query string or a header set section of an input. */
  def parametersType(fields: Chain[Self.Field[Parameter.Node, ?, ?]], side: Side): Option[Typescript.Type] =
    val members = fields.toList.map: field =>
      Typescript.Type.Field(
        field.name,
        ParameterTypescriptRenderer.render(field.schema.value),
        optional = TypescriptEnvelope.isOptional(field, side)
      )

    Option.when(members.nonEmpty)(Typescript.Type.Object(members))

  /** Whether a caller may leave a field out.
    *
    * Side dependent, and this is the case where the two sides of a schema genuinely differ: a field holding a default
    * is always written and may be absent when read, so a writer owes it and a reader does not. Optionality proper is
    * the same either way.
    */
  def isOptional(field: Self.Field[?, ?, ?], side: Side): Boolean = field match
    case Self.Field.Root(_, _)         => false
    case Self.Field.Modify(self, _, _) => TypescriptEnvelope.isOptional(self, side)
    case Self.Field.Optional(_)        => true
    case Self.Field.Default(_, _)      =>
      side match
        case Side.Read  => true
        case Side.Write => false

  /** The name an endpoint is generated under when it does not say. `getReportsId` for `GET /reports/{id}`. */
  def name(method: Method, schema: Path.Node[?, ?]): String =
    val pieces = PathTemplate(schema).toList.map:
      case Left(literal)    => literal
      case Right((name, _)) => name

    (method.name.toLowerCase(java.util.Locale.ROOT) :: pieces.map(TypescriptEnvelope.capitalised)).mkString

  /** How an operation is named in an issue, which is the only name every endpoint has. */
  def operation(schema: Request.Schema[?, ?, ?]): String =
    s"${schema.method.name} ${PathTemplate.render(schema.path.value)}"

  private def read(section: String, name: String): Typescript.Expression =
    Typescript.Expression.Index(
      Typescript.Expression.Index(TypescriptEnvelope.Input, Typescript.Expression.Literal.String(section)),
      Typescript.Expression.Literal.String(name)
    )

  /** Only the first character is touched, and only when it is a letter: a segment is whatever the path said it was, and
    * a generated name that dropped characters out of it would stop being traceable back to the endpoint it names.
    */
  private def capitalised(value: String): String =
    value.headOption.fold(value)(head => head.toUpper.toString ++ value.drop(1)).filter(_.isLetterOrDigit)

  /** Every parameter a request reads, by section. */
  def queries(schema: Request.Schema[?, ?, ?]): Chain[Self.Field[Parameter.Node, ?, ?]] =
    Chain.fromOption(schema.queries).flatMap(reference => Queries.fields(reference.value))

  def headers(schema: Request.Schema[?, ?, ?]): Chain[Self.Field[Parameter.Node, ?, ?]] =
    Chain.fromOption(schema.headers).flatMap(reference => Headers.fields(reference.value))
