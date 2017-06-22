

# Module jsongen #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

This module translates a JSON Schema into
an Erlang QuickCheck generator.

Copyright (c) 2013 Ángel Herranz, Lars-Ake Fredlund, Sergio Gil

__Authors:__ [`Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund  (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)`](mailto:Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund
  (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#json-1">json/1</a></td><td>
Translates a JSON schema into an Erlang QuickCheck generator.</td></tr><tr><td valign="top"><a href="#version-0">version/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="json-1"></a>

### json/1 ###

<pre><code>
json(Schema::<a href="jsg_json.md#type-json_term">jsg_json:json_term()</a>) -&gt; <a href="eqc_gen.md#type-gen">eqc_gen:gen</a>(<a href="jsg_json.md#type-json_term">jsg_json:json_term()</a>)
</code></pre>
<br />

Translates a JSON schema into an Erlang QuickCheck generator.

<a name="version-0"></a>

### version/0 ###

`version() -> any()`

