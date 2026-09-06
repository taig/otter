package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Metadata
import io.taig.otter.Side
import io.taig.otter.Typescript
import io.taig.otter.TypescriptEffect
import io.taig.otter.codec.JsonTypescriptContext
import io.taig.otter.codec.JsonTypescriptDefinition
import io.taig.otter.http.Bodies
import io.taig.otter.http.Body
import io.taig.otter.http.Code
import io.taig.otter.http.Endpoint
import io.taig.otter.http.HttpTypescriptEffect
import io.taig.otter.http.HttpTypescriptKeys
import io.taig.otter.http.MediaType
import io.taig.otter.http.Multipart
import io.taig.otter.http.Request
import io.taig.otter.http.Result
import io.taig.otter.http.Results
import io.taig.otter.http.TypescriptIssue
import io.taig.otter.http.TypescriptModule

import java.math.BigDecimal as JBigDecimal
import scala.compiletime.asMatchable

/** Turns endpoints into TypeScript descriptors.
  *
  * A descriptor and not a client, which is the one decision the rest of this file follows from. A generated function
  * that fetched and decoded before it returned would never let its caller hold the response as the serialisable thing
  * it arrived as, and a cache that requires serialisable values cannot keep what a schema decoded into a `Date`. So
  * what is generated is everything a caller needs to make the call and to read the answer, and never the call: the
  * builders that write a request out of an input, the `Schema` for each body, and both the decoded and the encoded type
  * of every answer. When to decode is the caller's, and the encoded type is what they name their cache at.
  *
  * The two sides are given rather than assumed, for the reason [[OpenApiRenderer]] gives: a caller *writes* the request
  * and *reads* the response, and the two sides of a schema genuinely differ wherever a field is optional or holds a
  * default. [[TypescriptEndpointRenderer.client]] is that pairing and is the one that makes sense here.
  *
  * Payload schemas are rendered by [[TypescriptPayload]], so an alphabet this module has never heard of contributes its
  * own renderer instead of being named here; one it does not recognise is reported and the body is still listed by its
  * media type.
  */
final class TypescriptEndpointRenderer(
    payload: TypescriptPayload,
    request: Side,
    response: Side,
    namespaces: NonEmptyList[Metadata.Namespace]
):
  import TypescriptEndpointRenderer.Collected
  import TypescriptEndpointRenderer.Step

  def render(endpoints: Chain[Endpoint.Node]): TypescriptModule =
    val empty = (Chain.empty[Typescript.Statement], Chain.empty[TypescriptIssue], Set.empty[String])

    val program = endpoints.toList.foldLeft(State.pure[JsonTypescriptContext, Collected](empty)):
      (accumulated, value) =>
        for
          collected <- accumulated
          (statements, issues, names) = collected
          rendered <- endpoint(value, names)
        yield rendered match
          case Left(issue)                    => (statements, issues :+ issue, names)
          case Right((name, declared, found)) => (statements ++ declared, issues ++ found, names + name)

    val (context, (statements, issues, _)) = program.run(JsonTypescriptContext.Empty).value

    TypescriptModule(
      TypescriptEndpointRenderer.Import :: context.declarations ++ statements.toList,
      issues.toList
    )

  /** One endpoint: the type of its input, the two types of its answer, and the descriptor itself. */
  private def endpoint(
      endpoint: Endpoint.Node,
      taken: Set[String]
  ): State[
    JsonTypescriptContext,
    Either[TypescriptIssue, (String, Chain[Typescript.Statement], Chain[TypescriptIssue])]
  ] =
    val schema = endpoint.request
    val operation = TypescriptEnvelope.operation(schema)
    val name = attr(endpoint.self.metadata, HttpTypescriptKeys.operationId)
      .getOrElse(TypescriptEnvelope.name(schema.method, schema.path.value))

    if taken.contains(name) then State.pure(Left(TypescriptIssue.Duplicate(operation, name)))
    else
      val declared = TypescriptEndpointRenderer.capitalised(name)

      for
        (body, requested) <- this.body(operation, declared, schema)
        (results, answered) <- this.results(operation, declared, endpoint.responses)
      yield
        val input = this.input(schema, body.map(_._2))
        val statements =
          Chain(
            Typescript.Statement.Declaration.Type(exported = true, declared ++ "Input", input),
            Typescript.Statement.Declaration
              .Type(exported = true, declared ++ "Output", this.answer(results, TypescriptEffect.inferred)),
            Typescript.Statement.Declaration
              .Type(exported = true, declared ++ "Encoded", this.answer(results, TypescriptEffect.encoded)),
            Typescript.Statement.Declaration.Constant(
              exported = true,
              name,
              tpe = None,
              value = Typescript.Expression.AsConst(descriptor(schema, declared, body.map(_._1), results))
            )
          )

        Right((name, statements, requested ++ answered))

  /** The descriptor: what a caller needs to write a request, and what they need to read an answer. */
  private def descriptor(
      schema: Request.Schema[?, ?, ?],
      declared: String,
      body: Option[Typescript.Expression],
      results: List[TypescriptEndpointRenderer.Answer]
  ): Typescript.Expression =
    val input = Typescript.Type.Symbol(declared ++ "Input", parameters = Nil)
    val queries = TypescriptEnvelope.queries(schema)
    val headers = TypescriptEnvelope.headers(schema)

    def builder(body: Typescript.Expression): Typescript.Expression =
      Typescript.Expression.Function(List(("input", input)), body)

    val fields = List(
      Some(("method", Typescript.Expression.Literal.String(schema.method.name))),
      Some(("path", builder(TypescriptEnvelope.path(schema.path.value)))),
      Option.when(queries.nonEmpty)(
        ("query", builder(TypescriptEnvelope.parameters(TypescriptEnvelope.Section.Query, queries, request)))
      ),
      Option.when(headers.nonEmpty)(
        ("headers", builder(TypescriptEnvelope.parameters(TypescriptEnvelope.Section.Headers, headers, request)))
      ),
      Some(("body", body.getOrElse(Typescript.Expression.Undefined))),
      Some(
        (
          "results",
          Typescript.Expression.Object(
            results.map(answer => (answer.code.value.toString, TypescriptEndpointRenderer.entries(answer.bodies)))
          )
        )
      )
    ).flatten

    Typescript.Expression.Object(fields)

  /** The type of an input, one member per section the endpoint actually has. */
  private def input(schema: Request.Schema[?, ?, ?], body: Option[Typescript.Type]): Typescript.Type =
    val sections = List(
      TypescriptEnvelope.pathType(schema.path.value).map((TypescriptEnvelope.Section.Path, _)),
      TypescriptEnvelope
        .parametersType(TypescriptEnvelope.queries(schema), request)
        .map((TypescriptEnvelope.Section.Query, _)),
      TypescriptEnvelope
        .parametersType(TypescriptEnvelope.headers(schema), request)
        .map((TypescriptEnvelope.Section.Headers, _)),
      body.map((TypescriptEnvelope.Section.Body, _))
    ).flatten

    Typescript.Type.Object(sections.map((name, tpe) => Typescript.Type.Field(name, tpe, optional = false)))

  /** The answer, as the union of what each status code carries.
    *
    * Rendered twice, once through each of [[TypescriptEffect.inferred]] and [[TypescriptEffect.encoded]]: the decoded
    * union is what a caller ends up with and the encoded one is what arrived, and it is the encoded one that survives
    * being cached.
    */
  private def answer(
      results: List[TypescriptEndpointRenderer.Answer],
      shape: Typescript.Type => Typescript.Type
  ): Typescript.Type =
    val members = results.map: answer =>
      val status = Typescript.Type.Field(
        "status",
        Typescript.Type.Literal.Number(new JBigDecimal(answer.code.value)),
        optional = false
      )

      val body = TypescriptEndpointRenderer
        .union(answer.bodies.map(TypescriptEndpointRenderer.shaped(shape)))
        .map(tpe => Typescript.Type.Field("body", tpe, optional = false))

      Typescript.Type.Object(status :: body.toList)

    /* An endpoint that answers under no status at all describes nothing a caller can be handed, and `never` is what
     * says so -- not an empty union, which is not a type TypeScript spells. */
    TypescriptEndpointRenderer.union(members).getOrElse(Typescript.Type.Symbol("never", parameters = Nil))

  private def results(
      operation: String,
      declared: String,
      schema: Results.Schema[?, ?, ?]
  ): Step[List[TypescriptEndpointRenderer.Answer]] =
    Results
      .branches(schema)
      .toList
      .traverse(result => this.result(operation, declared, result))
      .map(answers => (answers.map(_._1), Chain.fromSeq(answers).flatMap(_._2)))

  private def result(
      operation: String,
      declared: String,
      schema: Result.Schema[?, ?, ?]
  ): State[JsonTypescriptContext, (TypescriptEndpointRenderer.Answer, Chain[TypescriptIssue])] =
    val code = schema.code
    val hint = declared ++ "Response" ++ code.value.toString

    val whole = schema.bodies
      .map(reference => bodies(operation, hint, response, reference.value))
      .getOrElse(State.pure((Nil, Chain.empty)))

    val streamed = Chain.fromOption(
      schema.streamed.map(reference => TypescriptIssue.Streamed(operation, reference.value.mediaType.render))
    )

    whole.map((bodies, issues) => (TypescriptEndpointRenderer.Answer(code, bodies), issues ++ streamed))

  private def body(
      operation: String,
      declared: String,
      schema: Request.Schema[?, ?, ?]
  ): Step[Option[(Typescript.Expression, Typescript.Type)]] =
    val streamed = Chain.fromOption(
      schema.streamed.map(reference => TypescriptIssue.Streamed(operation, reference.value.mediaType.render))
    )

    schema.bodies match
      case None            => State.pure((None, streamed))
      case Some(reference) =>
        bodies(operation, declared ++ "Request", request, reference.value).map: (bodies, issues) =>
          val shaped =
            TypescriptEndpointRenderer.union(bodies.map(TypescriptEndpointRenderer.shaped(TypescriptEffect.inferred)))

          (
            shaped.map(tpe => (TypescriptEndpointRenderer.entries(bodies), tpe)),
            issues ++ streamed
          )

  /** Every alternative a body offers, each keyed by the media type it arrives as. */
  private def bodies(
      operation: String,
      hint: String,
      side: Side,
      schema: Bodies.Node[?, ?]
  ): Step[List[TypescriptEndpointRenderer.Alternative]] =
    Bodies
      .branches(schema)
      .toList
      .traverse(body => this.alternative(operation, hint, side, body))
      .map(alternatives => (alternatives.map(_._1), Chain.fromSeq(alternatives).flatMap(_._2)))

  private def alternative(
      operation: String,
      hint: String,
      side: Side,
      schema: Body.Schema[?, ?, ?]
  ): State[JsonTypescriptContext, (TypescriptEndpointRenderer.Alternative, Chain[TypescriptIssue])] =
    val media = schema.mediaType

    TypescriptEndpointRenderer.value(schema.self.self) match
      case Body.Value.Binary(_) =>
        State.pure((TypescriptEndpointRenderer.Alternative(media, None, TypescriptEndpointRenderer.Blob), Chain.empty))
      case Body.Value.Streamed(_, _, _) =>
        State.pure(
          (
            TypescriptEndpointRenderer.Alternative(media, None, TypescriptEndpointRenderer.Unknown),
            Chain.one(TypescriptIssue.Streamed(operation, media.render))
          )
        )
      case Body.Value.Whole(_, content) =>
        content.value.asMatchable match
          case _: Multipart.Node[?, ?] @unchecked =>
            State.pure(
              (
                TypescriptEndpointRenderer.Alternative(media, None, TypescriptEndpointRenderer.Unknown),
                Chain.one(TypescriptIssue.Multipart(operation))
              )
            )
          case document => this.document(operation, hint, side, media, document)
      case Body.Value.Modify(_, _, _) =>
        State.pure(
          (TypescriptEndpointRenderer.Alternative(media, None, TypescriptEndpointRenderer.Unknown), Chain.empty)
        )

  /** A payload, handed to whichever renderer knows its alphabet.
    *
    * An anonymous payload is declared under a name derived from the endpoint rather than written where it stands. A
    * descriptor has to be able to *name* the schema it carries -- `Schema.Schema.Type<typeof X>` needs an `X` -- and an
    * expression written inline has no name for the type alias to point at.
    */
  private def document(
      operation: String,
      hint: String,
      side: Side,
      media: MediaType,
      content: Any
  ): State[JsonTypescriptContext, (TypescriptEndpointRenderer.Alternative, Chain[TypescriptIssue])] =
    payload.render(side, content) match
      case None =>
        State.pure(
          (
            TypescriptEndpointRenderer.Alternative(media, None, TypescriptEndpointRenderer.Unknown),
            Chain.one(TypescriptIssue.Undescribed(operation, media.render))
          )
        )
      case Some(rendered) =>
        rendered.flatMap: expression =>
          TypescriptEndpointRenderer
            .named(hint, expression)
            .map: symbol =>
              (
                TypescriptEndpointRenderer.Alternative(media, Some(symbol), Typescript.Type.TypeOf(symbol)),
                Chain.empty
              )

  private def attr[A](metadata: Metadata, key: Metadata.Key[A]): Option[A] =
    metadata.get(namespaces.head, namespaces.tail*)(key)

object TypescriptEndpointRenderer:
  /** The descriptors a caller sees: it writes the request and reads the response. */
  def client(payload: TypescriptPayload): TypescriptEndpointRenderer =
    new TypescriptEndpointRenderer(payload, Side.Write, Side.Read, HttpTypescriptEffect.Namespaces)

  /** The same endpoints as the side answering them sees, which is what a generated server stub would want. */
  def server(payload: TypescriptPayload): TypescriptEndpointRenderer =
    new TypescriptEndpointRenderer(payload, Side.Read, Side.Write, HttpTypescriptEffect.Namespaces)

  val Import: Typescript.Statement =
    Typescript.Statement.Import(NonEmptyList.one("Schema"), HttpTypescriptEffect.Module)

  private val Blob: Typescript.Type = Typescript.Type.Symbol("Blob", parameters = Nil)

  private val Unknown: Typescript.Type = Typescript.Type.Symbol("unknown", parameters = Nil)

  private type Collected = (Chain[Typescript.Statement], Chain[TypescriptIssue], Set[String])

  private type Step[A] = State[JsonTypescriptContext, (A, Chain[TypescriptIssue])]

  /** One alternative of a body: the media type it arrives as, the schema that reads it where there is one, and the
    * TypeScript type of what it holds.
    */
  final private case class Alternative(media: MediaType, schema: Option[Typescript.Expression], tpe: Typescript.Type)

  /** One answer: the status it comes under, and the alternatives it may carry. */
  final private case class Answer(code: Code, bodies: List[TypescriptEndpointRenderer.Alternative])

  /** The alternatives of a body, keyed by media type, with `undefined` where nothing describes one. */
  private def entries(bodies: List[TypescriptEndpointRenderer.Alternative]): Typescript.Expression =
    Typescript.Expression.Object(
      bodies.map(body => (body.media.render, body.schema.getOrElse(Typescript.Expression.Undefined)))
    )

  /** The union of some types, collapsed to the one member where there is only one.
    *
    * The collapse is the same one [[TypescriptEffect.union]] makes, and for the same reason: a union of one is the
    * thing itself, and writing it with a bar in front says there is a choice where there is none.
    */
  private def union(types: List[Typescript.Type]): Option[Typescript.Type] = types match
    case Nil          => None
    case tpe :: Nil   => Some(tpe)
    case head :: tail => Some(Typescript.Type.Union(NonEmptyList(head, tail)))

  /** What an alternative holds, decoded or encoded. A body nothing describes is `unknown` either way -- there is no
    * second shape to tell apart when there was never a first.
    */
  private def shaped(shape: Typescript.Type => Typescript.Type)(
      body: TypescriptEndpointRenderer.Alternative
  ): Typescript.Type = body.schema.fold(body.tpe)(_ => shape(body.tpe))

  /** A name for an expression, declaring it when it does not already have one. */
  private def named(
      hint: String,
      expression: Typescript.Expression
  ): State[JsonTypescriptContext, Typescript.Expression] =
    expression match
      case symbol: Typescript.Expression.Symbol => State.pure(symbol)
      case expression                           =>
        State: context =>
          val symbol = Typescript.Expression.Symbol(hint)

          val definition = JsonTypescriptDefinition(
            tpe = TypescriptEffect.inferred(Typescript.Type.TypeOf(symbol)),
            annotation = None,
            expression = expression
          )

          (context.updated(hint, definition), symbol)

  private def value(schema: Body.Value[?, ?, ?]): Body.Value[?, ?, ?] = schema match
    case Body.Value.Modify(self, _, _) => TypescriptEndpointRenderer.value(self)
    case schema                        => schema

  /** Only the first character is touched: a name is whatever the endpoint said it was. */
  private def capitalised(value: String): String =
    value.headOption.fold(value)(head => head.toUpper.toString ++ value.drop(1))
