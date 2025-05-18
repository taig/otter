package io.taig.otter.http.syntax

trait AllHttpSyntax
    extends AppSyntax,
      BodySyntax,
      CodeSyntax,
      EndpointSyntax,
      FormDataBodySyntax,
      HeaderSyntax,
      MediaTypeSyntax,
      MethodSyntax,
      ParameterSyntax,
      QuerySyntax,
      ResponseSyntax,
      ResultSyntax

object AllHttpSyntax extends AllHttpSyntax
