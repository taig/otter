package io.taig.otter.http.openapi

import cats.data.{Chain, NonEmptyMap}
import cats.syntax.all.*
import io.taig.otter.{Codec, Data}
import io.taig.otter.http.{Endpoint, Method, Request}
import io.taig.otter.openapi.*

def toOpenApi(
    endpoints: Chain[Endpoint[?, ?]],
    title: String = "API Specification",
    description: Option[String] = None,
    version: String = "0",
    servers: Chain[Extended[Server]] = Chain.nil,
    tags: Chain[Extended[Tag]] = Chain.nil,
    securitySchemes: Data = Data.Null
): OpenApi = OpenApi(
  openapi = "3.1.0",
  Info(
    title = title,
    description = description,
    version = version
  ),
  servers = servers,
  tags = tags,
  paths = toPaths(endpoints),
  components = Data.Object.of("securitySchemes" -> securitySchemes)
)

def toPaths(endpoints: Chain[Endpoint[?, ?]]): Paths = Paths.fromIterableOnce:
  endpoints.groupBy(_.request.url.print).map { case (path, endpoints) =>
    (path, toPathItem(endpoints.filter(!_.hidden)))
  }

def toPathItem(endpoints: Chain[Endpoint[?, ?]]): PathItem = PathItem(
  get = endpoints.find(_.request.method === Method.Get).map(toOperation),
  put = endpoints.find(_.request.method === Method.Put).map(toOperation),
  post = endpoints.find(_.request.method === Method.Post).map(toOperation),
  delete = endpoints.find(_.request.method === Method.Delete).map(toOperation),
  options = endpoints.find(_.request.method === Method.Options).map(toOperation),
  head = endpoints.find(_.request.method === Method.Head).map(toOperation),
  patch = endpoints.find(_.request.method === Method.Patch).map(toOperation),
  trace = endpoints.find(_.request.method === Method.Trace).map(toOperation)
)

def toOperation(endpoint: Endpoint[?, ?]): Operation = Operation(
  tags = endpoint.tags,
  summary = endpoint.summary,
  description = endpoint.description,
  deprecated = endpoint.deprecated,
  requestBody = endpoint.request.body.codec.map(toRequestBody(endpoint.request.body, _))
)

def toRequestBody(request: Request.Body[?], codec: Codec[?]): RequestBody = RequestBody(
  NonEmptyMap.of("application/json" -> MediaType(toSchema(codec))),
  required = !codec.isOptional
)

//  def constraints(tpe: Type[?]): Chain[Constraint] => JsonObject =
//    _.foldLeft(JsonObject.empty)((result, current) => constraint(tpe)(current).deepMerge(result))
//
//  def constraints(codec: Collection[?, ?]): JsonObject =
//    codec.constraints.foldLeft(JsonObject.empty)((result, current) => constraint.array(current).deepMerge(result))
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
