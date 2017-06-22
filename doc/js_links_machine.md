

# Module js_links_machine #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This is a module.

Copyright (c) 2013 Ángel Herranz, Lars-Ake Fredlund, Sergio Gil

__Authors:__ [`Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund  (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)`](mailto:Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund
  (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)).

<a name="types"></a>

## Data Types ##




### <a name="type-filename">filename()</a> ###


<pre><code>
filename() = string()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#collect_links-1">collect_links/1</a></td><td></td></tr><tr><td valign="top"><a href="#collect_schema_links-2">collect_schema_links/2</a></td><td></td></tr><tr><td valign="top"><a href="#format_http_call-1">format_http_call/1</a></td><td></td></tr><tr><td valign="top"><a href="#http_error-1">http_error/1</a></td><td></td></tr><tr><td valign="top"><a href="#http_result_code-1">http_result_code/1</a></td><td></td></tr><tr><td valign="top"><a href="#run_statem-1">run_statem/1</a></td><td>Punto de entrada a la librería para ejecutar los tests con la
ejecución del test de jsongen.</td></tr><tr><td valign="top"><a href="#run_statem-2">run_statem/2</a></td><td>Ejecución del test de jsongen pero sobreescribiendo las
funciones de Quickcheck con el módulo indicado.</td></tr><tr><td valign="top"><a href="#run_statem-3">run_statem/3</a></td><td>Ejecución de los tests de jsongen con módulo y opciones.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="collect_links-1"></a>

### collect_links/1 ###

`collect_links(Files) -> any()`

<a name="collect_schema_links-2"></a>

### collect_schema_links/2 ###

`collect_schema_links(RawSchema, DependsOnObject) -> any()`

<a name="format_http_call-1"></a>

### format_http_call/1 ###

`format_http_call(X1) -> any()`

<a name="http_error-1"></a>

### http_error/1 ###

`http_error(X1) -> any()`

<a name="http_result_code-1"></a>

### http_result_code/1 ###

`http_result_code(Result) -> any()`

<a name="run_statem-1"></a>

### run_statem/1 ###

<pre><code>
run_statem(Files::[<a href="#type-filename">filename()</a>]) -&gt; ok
</code></pre>
<br />

`Files`: Lista de ficheros que
  formarán el conjunto de links iniciales (links estáticos).  Los
  ficheros deberán estar en el path para que puedan leerse.<br />

Punto de entrada a la librería para ejecutar los tests con la
ejecución del test de jsongen.

<a name="run_statem-2"></a>

### run_statem/2 ###

<pre><code>
run_statem(PrivateModule::atom(), Files::[<a href="#type-filename">filename()</a>]) -&gt; ok
</code></pre>
<br />

`PrivateModule`: módulo erlang (sin terminación .erl)
  implementado por el usuario que contiene una o más funciones que
  sustituirán a las que usa Quickcheck para la máquina de estados.<br />

Ejecución del test de jsongen pero sobreescribiendo las
funciones de Quickcheck con el módulo indicado.

Si hay alguna función en el `PrivateModule` especificado que sobreescriba la
función de Quickcheck para la máquina de estados, se ejecutará
dicha función en lugar de la implementada por defecto en JSONgen

<a name="run_statem-3"></a>

### run_statem/3 ###

<pre><code>
run_statem(PrivateModule::atom(), Files::[<a href="#type-filename">filename()</a>], Options::[<a href="#type-option">option()</a>]) -&gt; ok
</code></pre>

<ul class="definitions"><li><code><a name="type-option">option()</a> = {cookies, boolean()} | {user, string()} | {password, string()} | {timeout, integer()} | {simulation_mode, boolean()} | {show_http_timing, boolean()} | {show_http_result, boolean()} | {show_uri, boolean()} | {validator, atom()}</code></li></ul>

`Options`: lista de tuplas {Opción,Valor}.<br />

Ejecución de los tests de jsongen con módulo y opciones.
En cso de que no se quiera usar ningún módulo auxiliar pero sí las opciones, se deberá indicar
que el módulo es `void`.

