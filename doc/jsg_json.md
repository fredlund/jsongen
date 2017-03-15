

# Module jsg_json #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This module contains functions for transforming
JSON data from text to a mochijson2 Erlang term, and back.

Copyright (c) 2013 Ángel Herranz, Lars-Ake Fredlund, Sergio Gil

__Authors:__ [`Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund  (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)`](mailto:Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund
  (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)).

<a name="types"></a>

## Data Types ##




### <a name="type-json_array">json_array()</a> ###


<pre><code>
json_array() = [<a href="#type-json_term">json_term()</a>]
</code></pre>




### <a name="type-json_dict">json_dict()</a> ###


<pre><code>
json_dict() = [{binary() | atom(), <a href="#type-json_term">json_term()</a>}]
</code></pre>




### <a name="type-json_object">json_object()</a> ###


<pre><code>
json_object() = {struct, <a href="#type-json_dict">json_dict()</a>}
</code></pre>




### <a name="type-json_term">json_term()</a> ###


<pre><code>
json_term() = null | true | false | binary() | atom() | number() | <a href="#type-json_object">json_object()</a> | <a href="#type-json_array">json_array()</a>
</code></pre>




### <a name="type-json_text">json_text()</a> ###


<pre><code>
json_text() = iolist()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>Translates a JSON value (as text) into a mochijson2 Erlang term
representing the value.</td></tr><tr><td valign="top"><a href="#decode_url-1">decode_url/1</a></td><td>
Reads a JSON schema in textual format, converting it into
a mochijson2 Erlang term.</td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>Translates a mochijson2 Erlang term representing a JSON value
into its textual representation.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(JsonString::<a href="#type-json_text">json_text()</a>) -&gt; <a href="#type-json_term">json_term()</a>
</code></pre>
<br />

Translates a JSON value (as text) into a mochijson2 Erlang term
representing the value.

<a name="decode_url-1"></a>

### decode_url/1 ###

<pre><code>
decode_url(URL::string()) -&gt; {ok, <a href="#type-json_term">json_term()</a>} | {error, any()}
</code></pre>
<br />

Reads a JSON schema in textual format, converting it into
a mochijson2 Erlang term.
The function argument can either
be on the form "http:...", "file:..." or a filename.

<a name="encode-1"></a>

### encode/1 ###

<pre><code>
encode(JsonErlang::<a href="#type-json_term">json_term()</a>) -&gt; <a href="#type-json_text">json_text()</a>
</code></pre>
<br />

Translates a mochijson2 Erlang term representing a JSON value
into its textual representation.

