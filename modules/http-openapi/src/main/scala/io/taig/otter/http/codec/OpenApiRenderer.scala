package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.otter as Self
import io.taig.otter.JsonSchema
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.Keys
import io.taig.otter.Metadata
import io.taig.otter.Side
import io.taig.otter.codec.JsonSchemaAnnotation
import io.taig.otter.http.Body
import io.taig.otter.http.Code
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Header
import io.taig.otter.http.HttpKeys
import io.taig.otter.http.MediaType
import io.taig.otter.http.Multipart
import io.taig.otter.http.OpenApi
import io.taig.otter.http.OpenApiDocument
import io.taig.otter.http.OpenApiIssue
import io.taig.otter.http.OpenApiKeys
import io.taig.otter.http.Parameter
import io.taig.otter.http.Part
import io.taig.otter.http.Path
import io.taig.otter.http.Query
import io.taig.otter.http.Request
import io.taig.otter.http.Result
import io.taig.otter.http.Results
import io.taig.otter.http.Segment

import scala.collection.immutable.ListMap
import scala.compiletime.asMatchable

/** Turns endpoints into an OpenAPI document.
  *
  * The two sides are given rather than assumed. A document a server publishes describes the request as that server
  * *reads* it and the response as it *writes* it, and a document generated for a client is the same schemas the other
  * way round -- which matters wherever a field is optional or holds a default, because there the two sides of a schema
  * genuinely differ. [[OpenApiRenderer.server]] and [[OpenApiRenderer.client]] are the two pairings.
  *
  * Payload documents are rendered by [[OpenApiPayload]], so an alphabet this module has never heard of contributes its
  * own renderer instead of being added here. [[Multipart]] is the exception, and deliberately: it is not a document
  * language but a structure of bodies, so it belongs to the renderer that already knows what a body is.
  */
final class OpenApiRenderer(
    profile: JsonSchemaProfile,
    payload: OpenApiPayload,
    request: Side,
    response: Side,
    namespaces: NonEmptyList[Metadata.Namespace]
):
  private val parameter = OpenApiParameterRenderer(profile, namespaces)

  def render(info: OpenApi.Info, endpoints: Chain[Endpoint.Node]): OpenApiDocument =
    val (paths, collected) = endpoints.foldLeft((ListMap.empty[String, ListMap[String, CirceJson]], Collected.Empty)):
      (accumulated, endpoint) =>
        val (paths, collected) = accumulated
        val (template, method, rendered, found) = operation(endpoint)
        val operations = paths.getOrElse(template, ListMap.empty)

        if operations.contains(method)
        then (paths, collected ++ found ++ Collected.issue(OpenApiIssue.Duplicate(s"$method $template")))
        else (paths.updated(template, operations.updated(method, rendered)), collected ++ found)

    val described = info.description.map(value => "description" -> CirceJson.fromString(value)).toList

    val components = Option
      .when(collected.definitions.nonEmpty):
        "components" -> OpenApi.obj("schemas" -> CirceJson.obj(collected.definitions.toList*))
      .toList

    val document = JsonSchema.merge(
      OpenApi.obj(
        "openapi" -> CirceJson.fromString(OpenApi.Version),
        "info" -> JsonSchema.merge(
          OpenApi.obj(
            "title" -> CirceJson.fromString(info.title),
            "version" -> CirceJson.fromString(info.version)
          ),
          described*
        ),
        "paths" -> CirceJson.obj(
          paths.toList.map((template, operations) => template -> CirceJson.obj(operations.toList*))*
        )
      ),
      components*
    )

    OpenApiDocument(document, collected.issues.toList)

  /** One operation, and the path and method it is filed under. */
  private def operation(endpoint: Endpoint.Node): (String, String, CirceJson, Collected) =
    val schema = endpoint.request
    val name = s"${schema.method.name} ${OpenApiRenderer.template(schema.path.value)}"

    val (parameters, found) = this.parameters(name, schema)
    val (body, requested) = this.requestBody(name, schema)
    val (responses, answered) = this.responses(name, endpoint.responses)

    def attr[A](key: Metadata.Key[A]): Option[A] =
      OpenApiRenderer.attr(namespaces, endpoint.self.metadata, key)

    val labels = List(
      attr(OpenApiKeys.summary).map(value => "summary" -> CirceJson.fromString(value)),
      attr(Keys.description).map(value => "description" -> CirceJson.fromString(value)),
      attr(OpenApiKeys.tags).map(values => "tags" -> CirceJson.fromValues(values.map(CirceJson.fromString)))
    ).flatten

    val rendered = JsonSchema.merge(
      JsonSchema.merge(
        OpenApi.obj("operationId" -> CirceJson.fromString(attr(OpenApiKeys.operationId).getOrElse(name))),
        labels*
      ),
      List(
        Option.when(parameters.nonEmpty)("parameters" -> CirceJson.fromValues(parameters.toList)),
        body.map("requestBody" -> _),
        Some("responses" -> CirceJson.obj(responses.toList*))
      ).flatten*
    )

    (
      OpenApiRenderer.template(schema.path.value),
      schema.method.name.toLowerCase(java.util.Locale.ROOT),
      rendered,
      found ++ requested ++ answered
    )

  /** Every parameter an operation reads, in the order OpenAPI lists them: path, then query, then header. */
  private def parameters(operation: String, schema: Request.Schema[?, ?, ?]): (Chain[CirceJson], Collected) =
    val path = OpenApiRenderer.placeholders(schema.path.value).map((name, value) => (OpenApi.InPath, name, value, true))

    val queries = Chain
      .fromOption(schema.queries)
      .flatMap(reference =>
        OpenApiRenderer.fields(reference.value.self.self).map { field =>
          (OpenApi.InQuery, field.name, field.schema.value, OpenApiParameterRenderer.required(field))
        }
      )

    val headers = Chain
      .fromOption(schema.headers)
      .flatMap(reference =>
        OpenApiRenderer.headers(reference.value.self.self).map { field =>
          (OpenApi.InHeader, field.name, field.schema.value, OpenApiParameterRenderer.required(field))
        }
      )

    (path ++ queries ++ headers).foldLeft((Chain.empty[CirceJson], Collected.Empty)): (accumulated, described) =>
      val (rendered, collected) = accumulated
      val (in, name, value, required) = described
      val document = parameter.render(value)
      val issues = Chain.fromSeq(document.issues).map(OpenApiIssue.Parameter(operation, name, _))

      (rendered :+ OpenApi.parameter(name, in, required, document.value), collected ++ Collected(issues, ListMap.empty))

  /** The one entity an operation reads, if it reads one. */
  private def requestBody(operation: String, schema: Request.Schema[?, ?, ?]): (Option[CirceJson], Collected) =
    val (whole, described) = schema.bodies
      .map(reference => this.content(operation, request, reference.value.self.self))
      .getOrElse((Nil, Collected.Empty))

    val (streamed, framed) = schema.streamed
      .map(reference => this.streamed(operation, request, reference.value))
      .map((entry, collected) => (List(entry), collected))
      .getOrElse((Nil, Collected.Empty))

    val entries = whole ++ streamed

    val rendered = Option.when(entries.nonEmpty):
      OpenApi.obj("required" -> CirceJson.True, "content" -> OpenApi.content(entries))

    (rendered, described ++ framed)

  private def responses(operation: String, schema: Results.Schema[?, ?, ?]): (ListMap[String, CirceJson], Collected) =
    OpenApiRenderer
      .results(schema.self.self)
      .foldLeft((ListMap.empty[String, CirceJson], Collected.Empty)): (accumulated, result) =>
        val (responses, collected) = accumulated
        val (rendered, found) = this.result(operation, result)

        (responses.updated(result.code.value.toString, rendered), collected ++ found)

  private def result(operation: String, schema: Result.Schema[?, ?, ?]): (CirceJson, Collected) =
    val (whole, described) = schema.bodies
      .map(reference => this.content(operation, response, reference.value.self.self))
      .getOrElse((Nil, Collected.Empty))

    val (streamed, framed) = schema.streamed
      .map(reference => this.streamed(operation, response, reference.value))
      .map((entry, collected) => (List(entry), collected))
      .getOrElse((Nil, Collected.Empty))

    val (headers, reported) = schema.headers
      .map(reference => this.headers(operation, reference.value.self.self))
      .getOrElse((ListMap.empty, Collected.Empty))

    val entries = whole ++ streamed

    /* A description is required by the specification and there is no honest way to omit it, so a result that says
     * nothing gets the phrase its own code carries. */
    val description = OpenApiRenderer
      .attr(namespaces, schema.self.metadata, Keys.description)
      .orElse(Code.reason(schema.code))
      .getOrElse(schema.code.value.toString)

    val rendered = JsonSchema.merge(
      OpenApi.obj("description" -> CirceJson.fromString(description)),
      List(
        Option.when(headers.nonEmpty)("headers" -> CirceJson.obj(headers.toList*)),
        Option.when(entries.nonEmpty)("content" -> OpenApi.content(entries))
      ).flatten*
    )

    (rendered, described ++ framed ++ reported)

  /** The headers a result writes, which OpenAPI keys by name rather than listing as parameters. */
  private def headers(
      operation: String,
      schema: Self.Record[Header.Node, ?, ?]
  ): (ListMap[String, CirceJson], Collected) =
    OpenApiRenderer
      .headers(schema)
      .foldLeft((ListMap.empty[String, CirceJson], Collected.Empty)): (accumulated, field) =>
        val (headers, collected) = accumulated
        val document = parameter.render(field.schema.value)
        val issues = Chain.fromSeq(document.issues).map(OpenApiIssue.Parameter(operation, field.name, _))

        val rendered = OpenApi.obj(
          "required" -> CirceJson.fromBoolean(OpenApiParameterRenderer.required(field)),
          "schema" -> document.value
        )

        (headers.updated(field.name, rendered), collected ++ Collected(issues, ListMap.empty))

  /** One media type entry per alternative a body offers. */
  private def content(
      operation: String,
      side: Side,
      schema: Self.Union[Body.Node, ?, ?]
  ): (List[(String, CirceJson)], Collected) =
    OpenApiRenderer
      .bodies(schema)
      .foldLeft((List.empty[(String, CirceJson)], Collected.Empty)): (accumulated, body) =>
        val (entries, collected) = accumulated
        val (entry, found) = this.entity(operation, side, body)

        (entries :+ entry, collected ++ found)

  private def entity(operation: String, side: Side, schema: Body.Schema[?, ?, ?]): ((String, CirceJson), Collected) =
    val (rendered, collected) = this.value(operation, side, schema.self.self)

    ((schema.mediaType.render, JsonSchemaAnnotation(namespaces, schema.self.metadata, rendered)), collected)

  private def value(operation: String, side: Side, schema: Body.Value[?, ?, ?]): (CirceJson, Collected) =
    schema match
      case Body.Value.Modify(self, _, _) => this.value(operation, side, self)
      case Body.Value.Binary(media)      =>
        /* OpenAPI 3.1 dropped `format: binary` in favour of saying what the bytes are, which is what the body already
         * carries: a string whose content is this media type. */
        (
          JsonSchema.merge(
            JsonSchema.typed("string"),
            "contentMediaType" -> CirceJson.fromString(media.essence.render)
          ),
          Collected.Empty
        )
      case Body.Value.Streamed(media, _, element) =>
        val (rendered, collected) = this.document(operation, side, element.value)

        (rendered, collected ++ Collected.issue(OpenApiIssue.Framed(operation, media.render)))
      case Body.Value.Whole(media, content) =>
        /* The one type test in the module, and the only kind available: a payload's alphabet is existential by
         * construction, so asking whether this one is a set of parts is a runtime question. `@unchecked` because the
         * type arguments are erased and irrelevant -- what is being asked is whether this is a `Multipart.Schema` at
         * all, which the class tag answers exactly, and every one of them is walked the same way whatever it holds. */
        content.value.asMatchable match
          case parts: Multipart.Node[?, ?] @unchecked => this.parts(operation, side, parts.self.self)
          case _                                      => this.document(operation, side, content.value, media)

  /** A multipart body, which OpenAPI spells as an object of properties with an `encoding` map beside it.
    *
    * The `encoding` entry is the only place a part's own content type can be said, and saying it is the whole reason a
    * part carries a body rather than a bare schema.
    */
  private def parts(
      operation: String,
      side: Side,
      schema: Self.Record[Part.Node, ?, ?]
  ): (CirceJson, Collected) =
    val (properties, encoding, required, collected) = OpenApiRenderer
      .parts(schema)
      .foldLeft(
        (
          ListMap.empty[String, CirceJson],
          ListMap.empty[String, CirceJson],
          Chain.empty[String],
          Collected.Empty
        )
      ): (accumulated, part) =>
        val (properties, encoding, required, collected) = accumulated
        val (field, metadata) = part
        val ((media, rendered), found) = this.entity(operation, side, field.schema.value)

        val disposition = OpenApiRenderer
          .attr(namespaces, metadata, HttpKeys.filename)
          .map(value => "contentDisposition" -> CirceJson.fromString(s"""form-data; filename="$value""""))
          .toList

        (
          properties.updated(field.name, rendered),
          encoding.updated(
            field.name,
            JsonSchema.merge(OpenApi.obj("contentType" -> CirceJson.fromString(media)), disposition*)
          ),
          if OpenApiParameterRenderer.required(field) then required :+ field.name else required,
          collected ++ found
        )

    val rendered = JsonSchema.merge(
      JsonSchema.merge(JsonSchema.typed("object"), "properties" -> CirceJson.obj(properties.toList*)),
      List(
        Option.when(required.nonEmpty)("required" -> CirceJson.fromValues(required.toList.map(CirceJson.fromString))),
        Option.when(encoding.nonEmpty)("encoding" -> CirceJson.obj(encoding.toList*))
      ).flatten*
    )

    (rendered, collected)

  private def streamed(
      operation: String,
      side: Side,
      schema: Body.Streamed.Schema[?, ?, ?]
  ): ((String, CirceJson), Collected) =
    val (rendered, collected) = this.document(operation, side, schema.self.self.element.value)

    (
      (schema.mediaType.render, JsonSchemaAnnotation(namespaces, schema.self.metadata, rendered)),
      collected ++ Collected.issue(OpenApiIssue.Framed(operation, schema.mediaType.render))
    )

  /** A payload, handed to whichever renderer knows its alphabet. */
  private def document(operation: String, side: Side, content: Any): (CirceJson, Collected) =
    this.document(operation, side, content, MediaType.Json)

  private def document(operation: String, side: Side, content: Any, media: MediaType): (CirceJson, Collected) =
    payload.render(side, content) match
      case Some(document) =>
        val (rendered, definitions) = OpenApiRenderer.extracted(document.value)
        val issues = Chain.fromSeq(document.issues).map(OpenApiIssue.Payload(operation, _))

        payload.name(content) match
          case Some(name) =>
            val reference = JsonSchema.ref(OpenApi.Definitions, name)

            /* A payload that refers to itself comes back as that reference already, with the body beside it in the
             * definitions -- there is no other way to write a recursive schema down, so the renderer below could not
             * inline it. Declaring `rendered` under the name in that case would replace the body with a reference to
             * itself. Only a root that came back inline needs hoisting. */
            val declared = if rendered == reference then definitions else definitions.updated(name, rendered)

            (reference, Collected(issues, declared))
          case None => (rendered, Collected(issues, definitions))
      case None =>
        (JsonSchema.Anything, Collected.issue(OpenApiIssue.Undescribed(operation, media.render)))

  /** What a render collected on its way: the shared schemas it declared, and where it fell short. */
  final private case class Collected(issues: Chain[OpenApiIssue], definitions: ListMap[String, CirceJson]):
    /** The first declaration of a name wins, and a second one that disagrees with it is reported. Merging the other way
      * round would let whichever operation happened to be rendered last decide what a shared name means, which is the
      * kind of difference nobody notices until a generated client stops compiling.
      */
    def ++(that: Collected): Collected =
      val conflicts = that.definitions.toList.collect:
        case (name, schema) if definitions.get(name).exists(_ != schema) => OpenApiIssue.Conflict(name)

      val added = that.definitions.filterNot((name, _) => definitions.contains(name))

      Collected(issues ++ that.issues ++ Chain.fromSeq(conflicts), definitions ++ added)

  private object Collected:
    val Empty: Collected = Collected(Chain.empty, ListMap.empty)

    def issue(value: OpenApiIssue): Collected = Collected(Chain.one(value), ListMap.empty)

object OpenApiRenderer:
  /** The document a server publishes: it reads the request and writes the response. */
  def server(profile: JsonSchemaProfile, payload: OpenApiPayload): OpenApiRenderer =
    new OpenApiRenderer(profile, payload, Side.Read, Side.Write, OpenApi.Namespaces)

  /** The same endpoints as a caller sees them: it writes the request and reads the response. */
  def client(profile: JsonSchemaProfile, payload: OpenApiPayload): OpenApiRenderer =
    new OpenApiRenderer(profile, payload, Side.Write, Side.Read, OpenApi.Namespaces)

  /** The path, with a placeholder where every dynamic segment stands. */
  private def template(schema: Path.Node[?, ?]): String =
    val pieces = OpenApiRenderer.segments(schema.self.self).map {
      case Left(literal)    => literal
      case Right((name, _)) => s"{$name}"
    }

    "/" ++ pieces.toList.mkString("/")

  /** The dynamic segments, in the order they appear. */
  private def placeholders(schema: Path.Node[?, ?]): Chain[(String, Parameter.Node[?, ?])] =
    OpenApiRenderer.segments(schema.self.self).collect { case Right(placeholder) => placeholder }

  /** Every segment, as either the literal it spells or the name and value it stands for. */
  private def segments(
      schema: Self.Tuple[Segment.Node, ?, ?]
  ): Chain[Either[String, (String, Parameter.Node[?, ?])]] = schema match
    case Self.Tuple.Empty                => Chain.empty
    case Self.Tuple.Modify(self, _, _)   => OpenApiRenderer.segments(self)
    case Self.Tuple.Optional(self)       => OpenApiRenderer.segments(self)
    case Self.Tuple.Default(self, _)     => OpenApiRenderer.segments(self)
    case Self.Tuple.Product(left, right) => OpenApiRenderer.segments(left) ++ OpenApiRenderer.segments(right)
    case Self.Tuple.Root(schema)         =>
      schema.value match
        case Segment.Static.Schema(node)  => Chain.one(OpenApiRenderer.literal(node.self).asLeft)
        case Segment.Dynamic.Schema(node) =>
          Chain.one((node.self.name, node.self.schema.value: Parameter.Node[?, ?]).asRight)

  /** The text a static segment spells, pushed back through the schema that writes it. */
  private def literal(schema: Self.Constant[Parameter.Primitive.Node, ?, ?]): String = schema match
    case Self.Constant.Modify(self, _, _)        => OpenApiRenderer.literal(self)
    case Self.Constant.Root(reference, value, _) => ParameterPrimitiveEncoder.encode(reference.value, value.value)

  private def fields(schema: Self.Record[Query.Node, ?, ?]): Chain[Self.Field[Parameter.Node, ?, ?]] = schema match
    case Self.Record.Empty                => Chain.empty
    case Self.Record.Modify(self, _, _)   => OpenApiRenderer.fields(self)
    case Self.Record.Product(left, right) => OpenApiRenderer.fields(left) ++ OpenApiRenderer.fields(right)
    case Self.Record.Root(field)          => Chain.one(field.value.self.self)

  private def headers(schema: Self.Record[Header.Node, ?, ?]): Chain[Self.Field[Parameter.Node, ?, ?]] = schema match
    case Self.Record.Empty                => Chain.empty
    case Self.Record.Modify(self, _, _)   => OpenApiRenderer.headers(self)
    case Self.Record.Product(left, right) => OpenApiRenderer.headers(left) ++ OpenApiRenderer.headers(right)
    case Self.Record.Root(field)          => Chain.one(field.value.self.self)

  /** Every part, with the metadata it carries of its own -- which is where a filename lives, and is not the same
    * metadata as the body it holds carries.
    */
  private def parts(
      schema: Self.Record[Part.Node, ?, ?]
  ): Chain[(Self.Field[Body.Node, ?, ?], Metadata)] = schema match
    case Self.Record.Empty                => Chain.empty
    case Self.Record.Modify(self, _, _)   => OpenApiRenderer.parts(self)
    case Self.Record.Product(left, right) => OpenApiRenderer.parts(left) ++ OpenApiRenderer.parts(right)
    case Self.Record.Root(field)          => Chain.one((field.value.self.self, field.value.self.metadata))

  private def bodies(schema: Self.Union[Body.Node, ?, ?]): Chain[Body.Schema[?, ?, ?]] = schema match
    case Self.Union.Modify(self, _, _)     => OpenApiRenderer.bodies(self)
    case Self.Union.Coproduct(left, right) => OpenApiRenderer.bodies(left) ++ OpenApiRenderer.bodies(right)
    case Self.Union.Root(branch)           => Chain.one(branch.value)

  private def results(schema: Self.Union[Result.Node, ?, ?]): Chain[Result.Schema[?, ?, ?]] = schema match
    case Self.Union.Modify(self, _, _)     => OpenApiRenderer.results(self)
    case Self.Union.Coproduct(left, right) => OpenApiRenderer.results(left) ++ OpenApiRenderer.results(right)
    case Self.Union.Root(branch)           => Chain.one(branch.value)

  /** The shared schemas a payload document declared, lifted out of it.
    *
    * [[OpenApiProfile]] points its references at `components/schemas`, so [[io.taig.otter.codec.JsonSchemaRenderer]]
    * declares them under a key of that name. A document has no top level key by that name -- they belong under
    * `components` -- so they are taken off here and nested once, for every payload, at the document's root.
    */
  private def extracted(document: CirceJson): (CirceJson, ListMap[String, CirceJson]) =
    val definitions = document.asObject
      .flatMap(_(OpenApi.Definitions))
      .flatMap(_.asObject)
      .map(fields => ListMap.from(fields.toList))
      .getOrElse(ListMap.empty)

    val stripped =
      document.asObject.fold(document)(fields => CirceJson.fromJsonObject(fields.remove(OpenApi.Definitions)))

    (stripped, definitions)

  private def attr[A](
      namespaces: NonEmptyList[Metadata.Namespace],
      metadata: Metadata,
      key: Metadata.Key[A]
  ): Option[A] = metadata.get(namespaces.head, namespaces.tail*)(key)
