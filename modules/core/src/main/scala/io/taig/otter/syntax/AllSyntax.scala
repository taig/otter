package io.taig.otter.syntax

trait AllSyntax
    extends CatsSyntax,
      CoerceableSyntax,
      CoerceSyntax,
      CollectionSyntax,
      ConstantSyntax,
      DictionarySyntax,
      EnumerationSyntax,
      NullishSyntax,
      RecordSyntax,
      TupleSyntax,
      UnionSyntax

object AllSyntax extends AllSyntax
