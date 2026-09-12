package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.NonEmptyList
import io.circe.Json as CirceJson
import io.taig.otter as Self
import io.taig.otter.JsonSchema
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.Keys
import io.taig.otter.Metadata
import io.taig.otter.Side
import io.taig.otter.codec.JsonSchemaAnnotation
import io.taig.otter.http.Bodies
import io.taig.otter.http.Body
import io.taig.otter.http.Code
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Headers
import io.taig.otter.http.HttpKeys
import io.taig.otter.http.MediaType
import io.taig.otter.http.Multipart
import io.taig.otter.http.OpenApi
import io.taig.otter.http.OpenApiDocument
import io.taig.otter.http.OpenApiIssue
import io.taig.otter.http.OpenApiKeys
import io.taig.otter.http.Queries
import io.taig.otter.http.Request
import io.taig.otter.http.Result
import io.taig.otter.http.Results
import io.taig.otter.http.component.MediaTypeComponent

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
    val name = s"${schema.method.name} ${PathTemplate.render(schema.path.value)}"

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
      PathTemplate.render(schema.path.value),
      schema.method.name.toLowerCase(java.util.Locale.ROOT),
      rendered,
      found ++ requested ++ answered
    )

  /** Every parameter an operation reads, in the order OpenAPI lists them: path, then query, then header. */
  private def parameters(operation: String, schema: Request.Schema[?, ?, ?]): (Chain[CirceJson], Collected) =
    val path = PathTemplate.placeholders(schema.path.value).map((name, value) => (OpenApi.InPath, name, value, true))

    val queries = Chain
      .fromOption(schema.queries)
      .flatMap(reference =>
        Queries.fields(reference.value).map { field =>
          (OpenApi.InQuery, field.name, field.schema.value, OpenApiParameterRenderer.required(field))
        }
      )

    val headers = Chain
      .fromOption(schema.headers)
      .flatMap(reference =>
        Headers.fields(reference.value).map { field =>
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
      .map(reference => this.content(operation, request, reference.value))
      .getOrElse((Nil, Collected.Empty))

    val (streamed, framed) = schema.streamed
      .map(reference => this.streamed(operation, request, reference.value))
      .map((entry, collected) => (List(entry), collected))
      .getOrElse((Nil, Collected.Empty))

    val entries = whole ++ streamed

    val rendered = Option.when(entries.nonEmpty):
      OpenApi.obj("required" -> CirceJson.fromBoolean(schema.required), "content" -> OpenApi.content(entries))

    (rendered, described ++ framed ++ this.encodingAlternatives(operation, entries))

  private def responses(operation: String, schema: Results.Schema[?, ?, ?]): (ListMap[String, CirceJson], Collected) =
    val (groups, collected) = Results
      .branches(schema)
      .foldLeft((ListMap.empty[Int, List[Response]], Collected.Empty)): (accumulated, result) =>
        val (responses, collected) = accumulated
        val (rendered, found) = this.result(operation, result)
        val code = result.code.value

        (responses.updated(code, responses.getOrElse(code, Nil) :+ rendered), collected ++ found)

    groups.toList.foldLeft((ListMap.empty[String, CirceJson], collected)):
      case ((responses, collected), (code, alternatives)) =>
        val (rendered, found) = this.mergedResponse(operation, code, alternatives)

        (responses.updated(code.toString, rendered), collected ++ found)

  private def result(operation: String, schema: Result.Schema[?, ?, ?]): (Response, Collected) =
    val (whole, described) = schema.bodies
      .map(reference => this.content(operation, response, reference.value))
      .getOrElse((Nil, Collected.Empty))

    val (streamed, framed) = schema.streamed
      .map(reference => this.streamed(operation, response, reference.value))
      .map((entry, collected) => (List(entry), collected))
      .getOrElse((Nil, Collected.Empty))

    val (headers, reported) = schema.headers
      .map(reference => this.headers(operation, reference.value))
      .getOrElse((ListMap.empty, Collected.Empty))

    val entries = whole ++ streamed

    /* A description is required by the specification and there is no honest way to omit it, so a result that says
     * nothing gets the phrase its own code carries. */
    val description = OpenApiRenderer
      .attr(namespaces, schema.self.metadata, Keys.description)
      .orElse(Code.reason(schema.code))
      .getOrElse(schema.code.value.toString)

    val unsupported = entries.collect:
      case (media, content) if content.encoding.nonEmpty => OpenApiIssue.Encoding(operation, media)

    (
      Response(description, entries.map((media, content) => media -> content.copy(encoding = ListMap.empty)), headers),
      described ++ framed ++ reported ++ Collected(Chain.fromSeq(unsupported), ListMap.empty)
    )

  private def mergedResponse(operation: String, code: Int, alternatives: List[Response]): (CirceJson, Collected) =
    val entries = alternatives.flatMap(_.content)
    val names = alternatives
      .flatMap(_.headers.keys)
      .foldLeft(List.empty[String]): (names, name) =>
        if names.exists(_.equalsIgnoreCase(name)) then names else names :+ name
    val headers = names.map: name =>
      val variants = alternatives.flatMap(_.headers.toList.filter(_._1.equalsIgnoreCase(name)).map(_._2))
      val schemas = variants.map(_.hcursor.downField("schema").focus.getOrElse(JsonSchema.Anything)).distinct
      val required =
        variants.size == alternatives.size && variants.forall(_.hcursor.get[Boolean]("required").contains(true))

      name -> OpenApi.obj(
        "required" -> CirceJson.fromBoolean(required),
        "schema" -> JsonSchema.anyOf(NonEmptyList.fromListUnsafe(schemas))
      )
    val descriptions = alternatives.map(_.description).distinct.mkString("\n\n")
    val loss = alternatives.map(_.headers).distinct.size > 1 || alternatives.map(_.content.isEmpty).distinct.size > 1
    val issues = if loss then Collected.issue(OpenApiIssue.ResponseAlternatives(operation, code)) else Collected.Empty
    val rendered = JsonSchema.merge(
      OpenApi.obj("description" -> CirceJson.fromString(descriptions)),
      List(
        Option.when(headers.nonEmpty)("headers" -> CirceJson.obj(headers*)),
        Option.when(entries.nonEmpty)("content" -> OpenApi.content(entries))
      ).flatten*
    )

    (rendered, issues)

  final private case class Response(
      description: String,
      content: List[(String, OpenApi.Content)],
      headers: ListMap[String, CirceJson]
  )

  /** The headers a result writes, which OpenAPI keys by name rather than listing as parameters. */
  private def headers(
      operation: String,
      schema: Headers.Node[?, ?]
  ): (ListMap[String, CirceJson], Collected) =
    Headers
      .fields(schema)
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
      schema: Bodies.Node[?, ?]
  ): (List[(String, OpenApi.Content)], Collected) =
    Bodies
      .branches(schema)
      .foldLeft((List.empty[(String, OpenApi.Content)], Collected.Empty)): (accumulated, body) =>
        val (entries, collected) = accumulated
        val (entry, found) = this.entity(operation, side, body)

        (entries :+ entry, collected ++ found)

  private def entity(
      operation: String,
      side: Side,
      schema: Body.Schema[?, ?, ?]
  ): ((String, OpenApi.Content), Collected) =
    val (rendered, collected) = this.value(operation, side, schema.self.self)

    (
      (
        schema.mediaType.render,
        rendered.copy(schema = JsonSchemaAnnotation(namespaces, schema.self.metadata, rendered.schema))
      ),
      collected
    )

  private def value(operation: String, side: Side, schema: Body.Value[?, ?, ?]): (OpenApi.Content, Collected) =
    schema match
      case Body.Value.Modify(self, _, _) => this.value(operation, side, self)
      case Body.Value.Binary(media)      =>
        /* OpenAPI 3.1 dropped `format: binary` in favour of saying what the bytes are, which is what the body already
         * carries: a string whose content is this media type. */
        (
          OpenApi.Content(
            JsonSchema.merge(
              JsonSchema.typed("string"),
              "contentMediaType" -> CirceJson.fromString(media.essence.render)
            )
          ),
          Collected.Empty
        )
      case Body.Value.Streamed(media, _, element) =>
        val (rendered, collected) = this.document(operation, side, element.value)

        (OpenApi.Content(rendered), collected ++ Collected.issue(OpenApiIssue.Framed(operation, media.render)))
      case Body.Value.Whole(media, content) =>
        /* The one type test in the module, and the only kind available: a payload's alphabet is existential by
         * construction, so asking whether this one is a set of parts is a runtime question. `@unchecked` because the
         * type arguments are erased and irrelevant -- what is being asked is whether this is a `Multipart.Schema` at
         * all, which the class tag answers exactly, and every one of them is walked the same way whatever it holds. */
        content.value.asMatchable match
          case parts: Multipart.Node[?, ?] @unchecked =>
            val (rendered, collected) = this.parts(operation, side, parts)
            if media.primary.equalsIgnoreCase("multipart") then (rendered, collected)
            else
              (
                rendered.copy(encoding = ListMap.empty),
                collected ++ Collected.issue(OpenApiIssue.Encoding(operation, media.render))
              )
          case _ =>
            val (rendered, collected) = this.document(operation, side, content.value, media)
            (OpenApi.Content(rendered), collected)

  /** A multipart body, which OpenAPI spells as an object of properties with an `encoding` map beside it.
    *
    * The `encoding` entry is the only place a part's own content type can be said, and saying it is the whole reason a
    * part carries a body rather than a bare schema.
    */
  private def parts(
      operation: String,
      side: Side,
      schema: Multipart.Node[?, ?]
  ): (OpenApi.Content, Collected) =
    val (properties, encoding, required, collected) = Multipart
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
          .map: value =>
            "headers" -> OpenApi.obj(
              "Content-Disposition" -> OpenApi.obj(
                "schema" -> JsonSchema.typed("string"),
                "example" -> CirceJson.fromString(
                  s"form-data; name=${OpenApiRenderer.quoted(field.name)}; filename=${OpenApiRenderer.quoted(value)}"
                )
              )
            )
          .toList

        (
          properties.updated(field.name, rendered.schema),
          encoding.updated(
            field.name,
            JsonSchema.merge(OpenApi.obj("contentType" -> CirceJson.fromString(media)), disposition*)
          ),
          if OpenApiParameterRenderer.required(field) then required :+ field.name else required,
          collected ++ found ++ (
            if rendered.encoding.nonEmpty then Collected.issue(OpenApiIssue.Encoding(operation, media))
            else Collected.Empty
          )
        )

    val rendered = JsonSchema.merge(
      JsonSchema.merge(JsonSchema.typed("object"), "properties" -> CirceJson.obj(properties.toList*)),
      List(
        Option.when(required.nonEmpty)("required" -> CirceJson.fromValues(required.toList.map(CirceJson.fromString)))
      ).flatten*
    )

    (OpenApi.Content(rendered, encoding), collected)

  private def encodingAlternatives(operation: String, entries: List[(String, OpenApi.Content)]): Collected =
    val issues = entries
      .foldLeft(ListMap.empty[String, List[ListMap[String, CirceJson]]]):
        case (groups, (media, content)) => groups.updated(media, groups.getOrElse(media, Nil) :+ content.encoding)
      .toList
      .collect:
        case (media, alternatives) if alternatives.distinct.size > 1 => OpenApiIssue.Encoding(operation, media)

    Collected(Chain.fromSeq(issues), ListMap.empty)

  private def streamed(
      operation: String,
      side: Side,
      schema: Body.Streamed.Schema[?, ?, ?]
  ): ((String, OpenApi.Content), Collected) =
    val (rendered, collected) = this.document(operation, side, schema.self.self.element.value)

    (
      (schema.mediaType.render, OpenApi.Content(JsonSchemaAnnotation(namespaces, schema.self.metadata, rendered))),
      collected ++ Collected.issue(OpenApiIssue.Framed(operation, schema.mediaType.render))
    )

  /** A payload, handed to whichever renderer knows its alphabet. */
  private def document(operation: String, side: Side, content: Any): (CirceJson, Collected) =
    this.document(operation, side, content, MediaTypeComponent.json)

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
  private def quoted(value: String): String =
    val escaped = value.flatMap:
      case '\\'                              => "\\\\"
      case '"'                               => "\\\""
      case char if char < ' ' || char == 127 => f"%%${char.toInt}%02X"
      case char                              => char.toString

    s"\"$escaped\""

  /** The document a server publishes: it reads the request and writes the response. */
  def server(profile: JsonSchemaProfile, payload: OpenApiPayload): OpenApiRenderer =
    new OpenApiRenderer(profile, payload, Side.Read, Side.Write, OpenApi.Namespaces)

  /** The same endpoints as a caller sees them: it writes the request and reads the response. */
  def client(profile: JsonSchemaProfile, payload: OpenApiPayload): OpenApiRenderer =
    new OpenApiRenderer(profile, payload, Side.Write, Side.Read, OpenApi.Namespaces)

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
