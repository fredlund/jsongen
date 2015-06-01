

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#read_schema-1">read_schema/1</a></td><td>
Reads a JSON schema in textual format, converting it into
a mochijson2 Erlang term.</td></tr></table>


<a name="functions"></a>

## Function Details ##

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
