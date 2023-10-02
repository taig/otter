package io.taig.otter.openapi

import cats.data.{Chain, NonEmptyMap}
import cats.syntax.all.*
import io.circe.syntax.*
import io.circe.{Json, JsonObject}
import io.taig.otter.{Schema as RootSchema, *}
import io.taig.otter.http.{Endpoint, Method, Request}

import scala.annotation.tailrec

def toOpenApi[F[_]](
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
  requestBody = endpoint.request.body.schema.map(toRequestBody(endpoint.request.body, _))
)

def toRequestBody(request: Request.Body[?], schema: RootSchema[?]): RequestBody = RequestBody(
  NonEmptyMap.of("application/json" -> MediaType(toSchema(schema))),
  required = !schema.isOptional
)

val toSchema: RootSchema[?] => Schema =
  case schema: Collection[?] => toSchema(schema)
  case schema: Coproduct[?]  => toSchema(schema)
  case schema: Primitive[?]  => toSchema(schema)
  case schema: Record[?]     => toSchema(schema)
  case schema: Union[?]      => toSchema(schema)
  case _                     => Schema.Value(tpe = "object")
//    case schema: Collection[?, ?] => collection(schema)
//    case schema: Enumeration[?]   => enumeration(schema)
//    case schema: Product[?]       => product(schema)
//    case schema: Dictionary[?]    => dictionary(schema)

def toSchema(schema: Coproduct[?]): Schema = Schema.OneOf(
  schema.toNonEmptyChain.map(branch => toSchema(branch.schema)).toChain
)

def toSchema(schema: Primitive[?]): Schema = Schema.Value(
  tpe = typeOf(schema.tpe),
  format = schema.format,
  description = schema.description
)

def toSchema(schema: Collection[?]): Schema =
  Schema.Array(items = toSchema(schema.schema))

//  def enumeration(schema: Enumeration[?]): JsonObject = JsonObject(
//    "type" := typeOf(schema.schema),
//    "enum" := Values.map(toJson),
//    "nullable" := schema.isOptional
//  )

def toSchema(schema: Record[?]): Schema = Schema.Object(
  description = schema.description,
  properties = schema.toChain.map(field => field.name -> toSchema(field.schema))
)

def toSchema(schema: Union[?]): Schema = Schema.OneOf(
  schemas = schema.toNonEmptyChain.map(toSchema).toChain
)

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
def typeOf(schema: Value[?]): String = schema match
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
