package io.taig.otter.openapi

import cats.data.Chain
import cats.syntax.all.*
import io.circe.syntax.*
import io.circe.{Json, JsonObject}
import io.taig.otter.*
import io.taig.otter.syntax.*
import io.taig.otter.http.{Endpoint, Method, Request, Routes}

import scala.annotation.tailrec

def toOpenApi[F[_]](
    routes: Routes[F],
    title: String = "API Specification",
    description: Option[String] = None,
    version: String = "0",
    servers: Chain[Extended[Server]] = Chain.nil,
    tags: Chain[Extended[Tag]] = Chain.nil,
    securitySchemes: Json = Json.Null
): OpenApi = OpenApi(
  openapi = "3.1.0",
  Info(
    title = title,
    description = description,
    version = version
  ),
  servers = servers,
  tags = tags,
  paths = toPaths(routes),
  components = JsonObject("securitySchemes" := securitySchemes).dropNullValues
)

def toPaths[F[_]](routes: Routes[F]): Paths = Paths.fromIterableOnce:
  routes.toChain.map(_.endpoint).groupBy(_.request.url.print).map { case (path, endpoints) =>
    (path, toPathItem(endpoints.filter(!_.hidden)))
  }

def toPathItem(endpoints: Chain[Endpoint[?, ?, ?]]): PathItem = PathItem(
  get = endpoints.find(_.request.method === Method.Get).map(toOperation),
  put = endpoints.find(_.request.method === Method.Put).map(toOperation),
  post = endpoints.find(_.request.method === Method.Post).map(toOperation),
  delete = endpoints.find(_.request.method === Method.Delete).map(toOperation),
  options = endpoints.find(_.request.method === Method.Options).map(toOperation),
  head = endpoints.find(_.request.method === Method.Head).map(toOperation),
  patch = endpoints.find(_.request.method === Method.Patch).map(toOperation),
  trace = endpoints.find(_.request.method === Method.Trace).map(toOperation)
)

def toOperation(endpoint: Endpoint[?, ?, ?]): Operation = Operation(
  tags = endpoint.tags,
  summary = endpoint.summary,
  description = endpoint.description,
  deprecated = endpoint.deprecated,
  requestBody = Option.when(!endpoint.request.body.isEmpty)(toRequestBody(endpoint.request.body))
)

def toRequestBody(request: Request.Body[?]): RequestBody = RequestBody(???)

val toEndpointSchema: Endpoint[?, ?, ?] => JsonObject = endpoint =>
  val isGetOrHeadOrDelete = (method: Method) =>
    method === Method.Get || method === Method.Head || method === Method.Delete

  JsonObject(
    "summary" := endpoint.summary,
    "description" := endpoint.description,
    "operationId" := endpoint.operationId,
    "requestBody" := {
      if isGetOrHeadOrDelete(endpoint.request.method)
      then Json.Null
      else request(endpoint.request).toJson
    },
    "tags" := endpoint.tags
  ).dropNullValues

val request: Request[?] => JsonObject = request =>
  JsonObject(
    "description" := request.description,
    "content" := JsonObject()
  ).dropNullValues

val schema: Schema[?] => JsonObject =
  case schema: Primitive[?] => primitive(schema)
//    case schema: Collection[?, ?] => collection(schema)
//    case schema: Enumeration[?]   => enumeration(schema)
//    case schema: Record[?]        => record(schema)
//    case schema: Product[?]       => product(schema)
//    case schema: Dictionary[?]    => dictionary(schema)
//    case schema: Coproduct[?]     => coproduct(schema)

def primitive(schema: Primitive[?]): JsonObject =
  val format = schema.format.fold(JsonObject.empty)(format => JsonObject("format" := format))
  val description = schema.description.fold(JsonObject.empty)(description => JsonObject("description" := description))
  // constraints(schema.tpe)(schema.constraints)
  //      .deepMerge(format)
  format
    .deepMerge(description)
    .deepMerge(
      JsonObject(
        "type" := typeOf(schema.tpe),
        "nullable" := schema.isOptional
        //          "example" := schema.example.value.flatMap(schema.encode).map(toJson).getOrElse(Json.Null)
      )
    )

//  def collection(schema: Collection[?, ?]): JsonObject = constraints(schema).deepMerge(
//    JsonObject(
//      "type" := "array",
//      "items" := self.schema(schema.Value),
//      "nullable" := schema.isOptional
//    )
//  )
//
//  def enumeration(schema: Enumeration[?]): JsonObject = JsonObject(
//    "type" := typeOf(schema.schema),
//    "enum" := Values.map(toJson),
//    "nullable" := schema.isOptional
//  )
//
//  def record(schema: Record[?]): JsonObject =
//    val properties = schema.toChain.toList.map(field => field.key := self.schema(field.schema))
//
//    val required = schema.toChain
//      .filterNot(_.schema.isOptional)
//      .map(_.key)
//      .pipe(required => if required.isEmpty then JsonObject.empty else JsonObject("required" := required))
//
//    required.deepMerge(
//      JsonObject(
//        "type" := "object",
//        "properties" := Json.fromFields(properties),
//        "nullable" := schema.isOptional
//      )
//    )
//
//  def product(schema: Product[?]): JsonObject = JsonObject(
//    "type" := "array",
//    "prefixItems" := schema.toChain.map(schema => self.schema(schema)),
//    "minItems" := schema.toChain.length,
//    "maxItems" := schema.toChain.length,
//    "additionalItems" := false
//  )
//
//  def dictionary(schema: Dictionary[?]): JsonObject = JsonObject(
//    "type" := "object",
//    "additionalProperties" := self.schema(schema.schema)
//  )
//
//  def coproduct(schema: Coproduct[?]): JsonObject = ???
//
//  def constraints(tpe: Type[?]): Chain[Constraint] => JsonObject =
//    _.foldLeft(JsonObject.empty)((result, current) => constraint(tpe)(current).deepMerge(result))
//
//  def constraints(schema: Collection[?, ?]): JsonObject =
//    schema.constraints.foldLeft(JsonObject.empty)((result, current) => constraint.array(current).deepMerge(result))
//
//  object constraint:
//    def apply(tpe: Type[?]): Constraint => JsonObject = constraint =>
//      tpe match
//        case Type.Int | Type.BigInt | Type.BigDecimal | Type.Double | Type.Float | Type.Long => numeric(constraint)
//        case Type.String                                                                     => string(constraint)
//        case Type.Boolean                                                                    => JsonObject.empty
//
//    val numeric: Constraint => JsonObject =
//      case Constraint.Minimum(reference, exclusive) =>
//        JsonObject("minimum" := reference, "exclusiveMinimum" := exclusive)
//      case Constraint.Maximum(reference, exclusive) =>
//        JsonObject("maximum" := reference, "exclusiveMaximum" := exclusive)
//      case Constraint.Multiple(of) => JsonObject("multipleOf" := of)
//      case _                       => JsonObject.empty
//
//    val array: Constraint => JsonObject =
//      case Constraint.MinItems(reference) => JsonObject("minItems" := reference)
//      case Constraint.MaxItems(reference) => JsonObject("maxItems" := reference)
//      case Constraint.UniqueItems         => JsonObject("uniqueItems" := true)
//      case _                              => JsonObject.empty
//
//    val string: Constraint => JsonObject =
//      case Constraint.MinLength(reference) => JsonObject("minLength" := reference)
//      case Constraint.MaxLength(reference) => JsonObject("maxLength" := reference)
//      case Constraint.Matches(pattern)     => JsonObject("pattern" := pattern.pattern())
//      case _                               => JsonObject.empty

@tailrec
def typeOf(schema: Schema.Value[?]): String = schema match
  case schema: Enumeration[?] => typeOf(schema.schema)
  case schema: Primitive[?]   => typeOf(schema.tpe)

val typeOf: Type[?] => String =
  case Type.Double | Type.Float | Type.BigDecimal => "number"
  case Type.Int | Type.Long | Type.BigInt         => "integer"
  case Type.Boolean                               => "boolean"
  case Type.String                                => "string"

extension (self: JsonObject)
  def dropNullValues: JsonObject = self.filter { case (_, value) => !value.isNull }
  def toJson: Json = Json.fromJsonObject(self)
