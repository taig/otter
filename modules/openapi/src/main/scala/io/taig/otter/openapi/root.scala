package io.taig.otter.openapi

import cats.data.{Chain, NonEmptyMap}
import cats.syntax.all.*
import io.circe.syntax.*
import io.circe.{Json, JsonObject}
import io.taig.otter.{Schema as RootSchema, *}
import io.taig.otter.http.{Endpoint, Method, Request}

import scala.annotation.tailrec

def toOpenApi(
    endpoints: Chain[Endpoint[?, ?]],
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
  paths = toPaths(endpoints),
  components = JsonObject("securitySchemes" := securitySchemes).dropNullValues
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

def toRequestBody(request: Request.Body[?], codec: RootSchema[?]): RequestBody = RequestBody(
  NonEmptyMap.of("application/json" -> MediaType(toSchema(codec))),
  required = !codec.isOptional
)

val toSchema: RootSchema[?] => Schema =
  case codec: Collection[?]  => toSchema(codec)
  case codec: Coproduct[?]   => toSchema(codec)
  case codec: Enumeration[?] => toSchema(codec)
  case codec: Primitive[?]   => toSchema(codec)
  case codec: Record[?]      => toSchema(codec)
  case codec: Union[?]       => toSchema(codec)
//  case _                      => ???

def toSchema(codec: Coproduct[?]): Schema = Schema.OneOf(
  codec.toNonEmptyChain.map(branch => toSchema(branch.codec)).toChain
)

def toSchema(codec: Primitive[?]): Schema = Schema.Value(
  tpe = typeOf(codec.tpe),
  format = codec.format,
  description = codec.description
)

def toSchema(codec: Collection[?]): Schema =
  Schema.Array(items = toSchema(codec.codec))

def toSchema(codec: Enumeration[?]): Schema = Schema.Enumeration(
  tpe = typeOf(codec.codec),
  enums = codec.values
)

//  def enumeration(codec: Enumeration[?]): JsonObject = JsonObject(
//    "type" := typeOf(codec.codec),
//    "enum" := Values.map(toJson),
//    "nullable" := codec.isOptional
//  )

def toSchema(codec: Record[?]): Schema = Schema.Object(
  description = codec.description,
  properties = codec.toChain.map(field => field.name -> toSchema(field.codec))
)

def toSchema(codec: Union[?]): Schema = Schema.OneOf(
  codecs = codec.toNonEmptyChain.map(toSchema).toChain
)

//  def product(codec: Product[?]): JsonObject = JsonObject(
//    "type" := "array",
//    "prefixItems" := codec.toChain.map(codec => self.codec(codec)),
//    "minItems" := codec.toChain.length,
//    "maxItems" := codec.toChain.length,
//    "additionalItems" := false
//  )
//
//  def dictionary(codec: Dictionary[?]): JsonObject = JsonObject(
//    "type" := "object",
//    "additionalProperties" := self.codec(codec.codec)
//  )
//
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

@tailrec
def typeOf(codec: Value[?]): String = codec match
  case codec: Enumeration[?] => typeOf(codec.codec)
  case codec: Primitive[?]   => typeOf(codec.tpe)

val typeOf: Type[?] => String =
  case Type.Double | Type.Float | Type.BigDecimal => "number"
  case Type.Int | Type.Long | Type.BigInt         => "integer"
  case Type.Boolean                               => "boolean"
  case Type.String                                => "string"

extension (self: JsonObject)
  def dropNullValues: JsonObject = self.filter { case (_, value) => !value.isNull }
  def toJson: Json = Json.fromJsonObject(self)
