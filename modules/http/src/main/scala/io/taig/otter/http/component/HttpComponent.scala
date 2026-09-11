package io.taig.otter.http.component

import io.taig.otter.Reference
import io.taig.otter.component.CoerceComponent
import io.taig.otter.component.CollectionComponent
import io.taig.otter.component.ConstantComponent
import io.taig.otter.component.EnumerationComponent
import io.taig.otter.component.PrimitiveComponent
import io.taig.otter.component.RecordComponent
import io.taig.otter.http.Body
import io.taig.otter.http.Code
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Header
import io.taig.otter.http.Headers
import io.taig.otter.http.Method
import io.taig.otter.http.Parameter
import io.taig.otter.http.Part
import io.taig.otter.http.Path
import io.taig.otter.http.Queries
import io.taig.otter.http.Query
import io.taig.otter.http.Request
import io.taig.otter.http.Result
import io.taig.otter.http.Results
import io.taig.otter.http.Segment
import io.taig.otter.http.codec.ParameterPrimitiveEncoder
import io.taig.otter.http.syntax.EndpointSyntax
import io.taig.otter.http.syntax.HttpSyntax
import io.taig.otter.http.syntax.PathSyntax
import io.taig.otter.operation.RecordOperation
import io.taig.otter.syntax.AllSyntax

/** The user facing vocabulary for defining HTTP endpoints.
  *
  * Each component is applied to a node's general form, so what a combinator holds is inferred at the call site:
  * `query("page", int)` is a `Query.Of[Parameter.Primitive.Number.Schema, Int]` and carries the fact that its value is
  * a number, which is what lets a position that only accepts text refuse it.
  *
  * The empty query string and the empty header set are spelled out rather than inherited from [[RecordComponent]],
  * which would collide: both are records, and one `RNil` cannot be both. The empty path is spelled out for a different
  * reason, which [[__]] gives.
  */
trait HttpComponent
    extends AllSyntax,
      EndpointSyntax,
      HttpSyntax,
      PathSyntax,
      PrimitiveComponent.Boolean[Parameter.Primitive.Boolean.Schema],
      PrimitiveComponent.Number[Parameter.Primitive.Number.Schema],
      PrimitiveComponent.Text[Parameter.Primitive.Text.Schema]:
  /** [[io.taig.otter.http.Path.Root]] under the name a URL starts with, so `__ / "users"` is `/users`.
    *
    * It is the one root spelled as its wire syntax rather than after the product it is, because a path is the one tier
    * whose wire syntax has an operator of its own -- the same reason [[io.taig.otter.http.syntax.PathSyntax]] gives it
    * `/`. `QNil` and `HNil` keep the family spelling, having no such syntax to be named after.
    *
    * It names a place to start and not a requirement: `__ :* segment("health")` is `/health`, and so is
    * `segment("health")` on its own.
    */
  export Path.Root as __

  /** The empty query string, which asks for nothing. */
  def QNil(using
      F: RecordOperation[[w, r] =>> Queries.Schema[Nothing, w, r], [w, r] =>> Query.Schema[Nothing, w, r]]
  ): Queries.Schema[Nothing, Unit, Unit] = F.empty

  /** The empty header set, which asks for nothing. */
  def HNil(using
      F: RecordOperation[[w, r] =>> Headers.Schema[Nothing, w, r], [w, r] =>> Header.Schema[Nothing, w, r]]
  ): Headers.Schema[Nothing, Unit, Unit] = F.empty

  /** A request with nothing but a method and a path. Its parts are added from there. */
  def request[W, R](method: Method, path: => Path.Node[W, R]): Request.Schema[Nothing, W, R] =
    Request.Schema(Request.Value.Root(method, Reference.later(path)))

  /** One answer, with nothing but a status code. Its parts are added from there. */
  def result(code: Code): Result.Schema[Nothing, Unit, Unit] = Result.Schema(Result.Value.Root(code))

  /** An endpoint: what it takes, and what it may answer.
    *
    * The payload types of the request and the responses are unioned rather than forced to agree, so an endpoint taking
    * a multipart upload and answering with JSON is one endpoint and its type says both.
    */
  def endpoint[S1[-w, +r], S2[-w, +r], AW, AR, BW, BR](
      request: Request.Schema[S1, AW, AR],
      responses: Results.Schema[S2, BW, BR]
  ): Endpoint.Schema[Body.Or[S1, S2], AW, AR, BW, BR] =
    Endpoint.Schema(Endpoint.Value[Body.Or[S1, S2], AW, AR, BW, BR](request, responses))

  object segment extends SegmentComponent

  /** A `val` rather than an `object`, so a consumer that layers a format-specific vocabulary on top -- JSON's `json`
    * and `ndjson`, say -- can override it with one that mixes that vocabulary in beside [[BodyComponent]]'s own. An
    * `object` member cannot be overridden this way; a `val` of the component's own type can.
    */
  val body: BodyComponent = new BodyComponent {}

  object part
      extends RecordComponent.Field[Body.Node, Parameter.Primitive.Text.Node, Part.Schema](using
        ParameterPrimitiveEncoder
      )

  object query
      extends RecordComponent.Field[Parameter.Node, Parameter.Primitive.Text.Node, Query.Schema](using
        ParameterPrimitiveEncoder
      )

  object header
      extends RecordComponent.Field[Parameter.Node, Parameter.Primitive.Text.Node, Header.Schema](using
        ParameterPrimitiveEncoder
      )

  object collection extends CollectionComponent[Parameter.Value.Node, Parameter.Collection.Schema]

  object constant extends ConstantComponent[Parameter.Primitive.Node, Parameter.Constant.Schema]

  object enumeration extends EnumerationComponent[Parameter.Primitive.Node, Parameter.Enumeration.Schema]

  object coerce extends CoerceComponent[Parameter.Primitive.Node, Parameter.Coerce.Schema]

object HttpComponent extends HttpComponent
