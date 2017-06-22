

# Module jsg_jsonschema #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

This module contains functions for parsing a JSON schema.

Copyright (c) 2013 Ángel Herranz, Lars-Ake Fredlund, Sergio Gil

__Authors:__ [`Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund  (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)`](mailto:Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund
  (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#additionalItems-1">additionalItems/1</a></td><td></td></tr><tr><td valign="top"><a href="#additionalProperties-1">additionalProperties/1</a></td><td></td></tr><tr><td valign="top"><a href="#allOf-1">allOf/1</a></td><td></td></tr><tr><td valign="top"><a href="#anyOf-1">anyOf/1</a></td><td></td></tr><tr><td valign="top"><a href="#enumerated-1">enumerated/1</a></td><td></td></tr><tr><td valign="top"><a href="#hasType-1">hasType/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_object-1">is_object/1</a></td><td></td></tr><tr><td valign="top"><a href="#items-1">items/1</a></td><td></td></tr><tr><td valign="top"><a href="#keyword-2">keyword/2</a></td><td></td></tr><tr><td valign="top"><a href="#keyword-3">keyword/3</a></td><td></td></tr><tr><td valign="top"><a href="#links-1">links/1</a></td><td></td></tr><tr><td valign="top"><a href="#maxProperties-1">maxProperties/1</a></td><td></td></tr><tr><td valign="top"><a href="#maxProperties-2">maxProperties/2</a></td><td></td></tr><tr><td valign="top"><a href="#minProperties-2">minProperties/2</a></td><td></td></tr><tr><td valign="top"><a href="#notKeyword-1">notKeyword/1</a></td><td></td></tr><tr><td valign="top"><a href="#oneOf-1">oneOf/1</a></td><td></td></tr><tr><td valign="top"><a href="#patternProperties-1">patternProperties/1</a></td><td></td></tr><tr><td valign="top"><a href="#properties-1">properties/1</a></td><td></td></tr><tr><td valign="top"><a href="#propertyValue-2">propertyValue/2</a></td><td></td></tr><tr><td valign="top"><a href="#read_schema-1">read_schema/1</a></td><td>
Reads a JSON schema in textual format, converting it into
a mochijson2 Erlang term.</td></tr><tr><td valign="top"><a href="#schemaType-1">schemaType/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_type-2">set_type/2</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="additionalItems-1"></a>

### additionalItems/1 ###

`additionalItems(X1) -> any()`

<a name="additionalProperties-1"></a>

### additionalProperties/1 ###

`additionalProperties(X1) -> any()`

<a name="allOf-1"></a>

### allOf/1 ###

`allOf(Schema) -> any()`

<a name="anyOf-1"></a>

### anyOf/1 ###

`anyOf(Schema) -> any()`

<a name="enumerated-1"></a>

### enumerated/1 ###

`enumerated(Schema) -> any()`

<a name="hasType-1"></a>

### hasType/1 ###

`hasType(Schema) -> any()`

<a name="is_object-1"></a>

### is_object/1 ###

`is_object(X1) -> any()`

<a name="items-1"></a>

### items/1 ###

`items(Schema) -> any()`

<a name="keyword-2"></a>

### keyword/2 ###

`keyword(Schema, KeyWord) -> any()`

<a name="keyword-3"></a>

### keyword/3 ###

`keyword(Schema, KeyWord, DefaultValue) -> any()`

<a name="links-1"></a>

### links/1 ###

`links(X1) -> any()`

<a name="maxProperties-1"></a>

### maxProperties/1 ###

`maxProperties(Schema) -> any()`

<a name="maxProperties-2"></a>

### maxProperties/2 ###

`maxProperties(Schema, Def) -> any()`

<a name="minProperties-2"></a>

### minProperties/2 ###

`minProperties(Schema, Def) -> any()`

<a name="notKeyword-1"></a>

### notKeyword/1 ###

`notKeyword(Schema) -> any()`

<a name="oneOf-1"></a>

### oneOf/1 ###

`oneOf(Schema) -> any()`

<a name="patternProperties-1"></a>

### patternProperties/1 ###

`patternProperties(X1) -> any()`

<a name="properties-1"></a>

### properties/1 ###

`properties(Schema) -> any()`

<a name="propertyValue-2"></a>

### propertyValue/2 ###

`propertyValue(X1, PropertyName) -> any()`

<a name="read_schema-1"></a>

### read_schema/1 ###

<pre><code>
read_schema(URL::string()) -&gt; {ok, <a href="jsg_json.md#type-json_term">jsg_json:json_term()</a>} | {error, any()}
</code></pre>
<br />

Reads a JSON schema in textual format, converting it into
a mochijson2 Erlang term.
The function argument can either
be on the form "http:...", "file:..." or a filename.

<a name="schemaType-1"></a>

### schemaType/1 ###

`schemaType(Schema) -> any()`

<a name="set_type-2"></a>

### set_type/2 ###

`set_type(Schema, Type) -> any()`

<a name="type-1"></a>

### type/1 ###

`type(Schema) -> any()`

