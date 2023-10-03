package io.taig.otter.http.openapi

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.http.Response.Body
import io.taig.otter.{
  Codec,
  Collection,
  Coproduct,
  Data,
  Dictionary,
  Dynamic,
  Enumeration,
  Primitive,
  Product,
  Record,
  Union
}
import io.taig.otter.http.{Endpoint, Method, Request, Response as OtterResponse, Result, Segment}
import io.taig.otter.openapi.*

def toOpenApi(
    endpoints: Chain[Endpoint[?, ?]],
    title: String = "API Specification",
    description: Option[String] = None,
    version: String = "0",
    servers: Chain[Extended[Server]] = Chain.nil,
    tags: Chain[Extended[Tag]] = Chain.nil,
    securitySchemes: Chain[(String, Extended[Data.Object] | Reference)] = Chain.empty
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
  components = toComponents(endpoints, securitySchemes)
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
  parameters = toParameters(endpoint.request),
  requestBody = endpoint.request.body.codec.map(toRequestBody(endpoint.request.body, _)),
  responses = toResponses(endpoint.response)
)

def toParameters(request: Request[?]): Chain[Extended[Parameter]] = request.url.path.toChain
  .collect { case segment: Segment.Parameter[?] => segment }
  .map: parameter =>
    Parameter(
      in = "path",
      name = parameter.name,
      description = parameter.description,
      required = true,
      schema = Some(toSchema(parameter.codec))
    )

def toRequestBody(request: Request.Body[?], codec: Codec[?]): RequestBody = RequestBody(
  content = Chain("application/json" -> MediaType(toSchemaOrReference(codec))),
  required = !codec.isOptional
)

def toResponses(response: OtterResponse[?]): Responses = Responses(
  values = toCodeAndResponses(response.results.toNonEmptyChain.toChain :+ response.violations)
)

def toCodeAndResponses(result: Chain[Result[?]]): Chain[(Int, Extended[Response])] = result.map(toCodeAndResponse)

def toCodeAndResponse(result: Result[?]): (Int, Extended[Response]) = result.code.toInt -> Response(
  description = result.description.orElse(result.code.toMessage).getOrElse(result.code.toString),
  content = Chain.fromOption(toMediaType(result.body))
)

def toMediaType(body: OtterResponse.Body[?]): Option[(String, Extended[MediaType])] = body match
  case body: OtterResponse.Body.Strict.Payload[?] => Some(body.mediaType.print -> MediaType(toSchema(body.codec)))
  case _: OtterResponse.Body.Strict.Empty[?]      => None

def toComponents(
    endpoints: Chain[Endpoint[?, ?]],
    securitySchemes: Chain[(String, Extended[Data.Object] | Reference)]
): Components =
  val schemas = endpoints.flatMap(toCodecs).foldLeft(Map.empty[String, Schema]) { (result, codec) =>
    codec.name match
      case Some(name) =>
        result.updatedWith(name) {
          case None    => Some(toSchema(codec))
          case current => current
        }
      case None => result
  }

  Components(
    schemas = Chain.fromIterableOnce(schemas),
    securitySchemes = securitySchemes
  )

def toCodecs(endpoint: Endpoint[?, ?]): Chain[Codec[?]] = (
  endpoint.request.url.path.toChain.collect { case segment: Segment.Parameter[?] => segment.codec } ++
    endpoint.request.url.queries.toChain.map(_.codec) ++
    endpoint.request.headers.toChain.map(_.codec) ++
    Chain.fromOption(endpoint.request.body.codec) ++
    (endpoint.response.results.toNonEmptyChain :+ endpoint.response.violations).toChain.flatMap: result =>
      Chain.one(result.body).collect { case body: OtterResponse.Body.Strict.Payload[?] => body.codec } ++
        result.headers.toChain.map(_.codec)
).flatMap(toCodecs)

def toCodecs(codec: Codec[?]): Chain[Codec[?]] = codec match
  case codec: Collection[?]  => codec +: toCodecs(codec.codec)
  case codec: Coproduct[?]   => codec +: codec.toNonEmptyChain.toChain.map(_.codec).flatMap(toCodecs)
  case codec: Dictionary[?]  => codec +: toCodecs(codec.codec)
  case codec: Dynamic[?]     => Chain.one(codec)
  case codec: Enumeration[?] => codec +: toCodecs(codec.codec)
  case codec: Primitive[?]   => Chain.one(codec)
  case codec: Product[?]     => codec +: codec.toChain.flatMap(toCodecs)
  case codec: Record[?]      => codec +: codec.toChain.map(_.codec).flatMap(toCodecs)
  case codec: Union[?]       => codec +: codec.toNonEmptyChain.toChain.flatMap(toCodecs)

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
