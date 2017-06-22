

# Module jsg_links #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Módulo con funciones básicas para operar sobre los Links y Schemas de jsongen.

Copyright (c) 2013 Ángel Herranz, Lars-Ake Fredlund, Sergio Gil

__Authors:__ [`Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund  (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)`](mailto:Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund
  (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)).

<a name="types"></a>

## Data Types ##




### <a name="type-header">header()</a> ###


<pre><code>
header() = {struct, {Prop::binary(), Value::binary()}}
</code></pre>




### <a name="type-link">link()</a> ###


<pre><code>
link() = {link, [{type, <a href="#type-link_type">link_type()</a>} | {calculated_href, iodata()} | {link, non_neg_integer()} | {schema, {struct, [iodata()]}}]}
</code></pre>




### <a name="type-link_def">link_def()</a> ###


<pre><code>
link_def() = {struct, [{iodata(), iodata()} | {iodata(), <a href="#type-header">header()</a>} | {iodata(), {struct, [{iodata(), iodata()}]}}]}
</code></pre>




### <a name="type-link_type">link_type()</a> ###


<pre><code>
link_type() = static | dynamic
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#collect_headers-1">collect_headers/1</a></td><td></td></tr><tr><td valign="top"><a href="#extract_dynamic_links-3">extract_dynamic_links/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_schema-1">get_schema/1</a></td><td>
Obtiene el esquema dado un objeto json de mochijson.</td></tr><tr><td valign="top"><a href="#intern_object-1">intern_object/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_parent_relative-1">is_parent_relative/1</a></td><td></td></tr><tr><td valign="top"><a href="#link_calculated_href-1">link_calculated_href/1</a></td><td>
Dado un Link, devuelve el href (URI) del Schema.</td></tr><tr><td valign="top"><a href="#link_def-1">link_def/1</a></td><td>
Dado un link, devuelve el json (mochijson) que lo define.</td></tr><tr><td valign="top"><a href="#link_history-1">link_history/1</a></td><td>
Obtiene el valor history del link.</td></tr><tr><td valign="top"><a href="#link_href-1">link_href/1</a></td><td>
Obtiene el valor href del link.</td></tr><tr><td valign="top"><a href="#link_request_type-1">link_request_type/1</a></td><td>
Obtiene el valor href del link.</td></tr><tr><td valign="top"><a href="#link_schema-1">link_schema/1</a></td><td>
Obtiene el valor schema del link.</td></tr><tr><td valign="top"><a href="#link_targetSchema-1">link_targetSchema/1</a></td><td>
Obtiene el valor targetSchema del link.</td></tr><tr><td valign="top"><a href="#link_title-1">link_title/1</a></td><td>
Dado un link, devuelve el título de dicho link.</td></tr><tr><td valign="top"><a href="#link_type-1">link_type/1</a></td><td>
Conocer el tipo del link.</td></tr><tr><td valign="top"><a href="#make_schema-2">make_schema/2</a></td><td></td></tr><tr><td valign="top"><a href="#print_link-1">print_link/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="collect_headers-1"></a>

### collect_headers/1 ###

`collect_headers(X1) -> any()`

<a name="extract_dynamic_links-3"></a>

### extract_dynamic_links/3 ###

`extract_dynamic_links(Link, Term, Object) -> any()`

<a name="get_schema-1"></a>

### get_schema/1 ###

`get_schema(Value) -> any()`

Obtiene el esquema dado un objeto json de mochijson.

<a name="intern_object-1"></a>

### intern_object/1 ###

`intern_object(Term) -> any()`

<a name="is_parent_relative-1"></a>

### is_parent_relative/1 ###

`is_parent_relative(X1) -> any()`

<a name="link_calculated_href-1"></a>

### link_calculated_href/1 ###

<pre><code>
link_calculated_href(Link) -&gt; binary()
</code></pre>
<br />

Dado un Link, devuelve el href (URI) del Schema.
Además, si el href está especificado con un recurso en base a un generador,
aplicará la transformación necesaria para generarlo.

<a name="link_def-1"></a>

### link_def/1 ###

<pre><code>
link_def(Link::<a href="#type-link">link()</a>) -&gt; <a href="#type-link_def">link_def()</a>
</code></pre>
<br />

Dado un link, devuelve el json (mochijson) que lo define.

Quizá esto no especifica de la mejor forma cómo es un link. Pese a
que en link_def() los atributos aparecen como opcionales o "alternatividad", es
necesario para el correcto funcionamiento que el link posea estas 4
características mínimas.

<a name="link_history-1"></a>

### link_history/1 ###

<pre><code>
link_history(Link::<a href="#type-link">link()</a>) -&gt; iodata() | undefined
</code></pre>
<br />

Obtiene el valor history del link.

<a name="link_href-1"></a>

### link_href/1 ###

<pre><code>
link_href(Link::<a href="#type-link">link()</a>) -&gt; iodata() | undefined
</code></pre>
<br />

Obtiene el valor href del link.

<a name="link_request_type-1"></a>

### link_request_type/1 ###

<pre><code>
link_request_type(Link::<a href="#type-link">link()</a>) -&gt; atom()
</code></pre>
<br />

Obtiene el valor href del link.

<a name="link_schema-1"></a>

### link_schema/1 ###

<pre><code>
link_schema(Link::<a href="#type-link">link()</a>) -&gt; iodata() | undefined
</code></pre>
<br />

Obtiene el valor schema del link.

<a name="link_targetSchema-1"></a>

### link_targetSchema/1 ###

<pre><code>
link_targetSchema(Link::<a href="#type-link">link()</a>) -&gt; iodata() | undefined
</code></pre>
<br />

Obtiene el valor targetSchema del link.

<a name="link_title-1"></a>

### link_title/1 ###

<pre><code>
link_title(Link::<a href="#type-link">link()</a>) -&gt; string() | undefined
</code></pre>
<br />

Dado un link, devuelve el título de dicho link.

spec link_title(Link :: link()) -> string() | undefined

<a name="link_type-1"></a>

### link_type/1 ###

<pre><code>
link_type(Link) -&gt; static | dynamic
</code></pre>
<br />

Conocer el tipo del link.

<a name="make_schema-2"></a>

### make_schema/2 ###

`make_schema(Schema, Parent) -> any()`

<a name="print_link-1"></a>

### print_link/1 ###

`print_link(Link) -> any()`

