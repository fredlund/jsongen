

# Module jsg_jsonref #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-jsonpointer">jsonpointer()</a> ###


<pre><code>
jsonpointer() = [string()]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#deref-2">deref/2</a></td><td>Evaluates the json pointer Pointer in the json value JsonTerm.</td></tr><tr><td valign="top"><a href="#deref_relative_pointer-4">deref_relative_pointer/4</a></td><td></td></tr><tr><td valign="top"><a href="#gen-1">gen/1</a></td><td>Generates all valid pairs {P,V} where P is a JSON Pointer and
V is its derreferenced value in a given JSON value.</td></tr><tr><td valign="top"><a href="#list_is_integer-1">list_is_integer/1</a></td><td>Decides if a list represent an integer.</td></tr><tr><td valign="top"><a href="#subst-3">subst/3</a></td><td>Evaluates the json pointer Pointer in the json value JsonTerm and
substitutes the json value by a new value.</td></tr><tr><td valign="top"><a href="#substl-4">substl/4</a></td><td></td></tr><tr><td valign="top"><a href="#unref-2">unref/2</a></td><td>Returns the json value indicated by the JsonRef using
RootJsonTerm as root document in case no URL part exists in the
URI.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="deref-2"></a>

### deref/2 ###

<pre><code>
deref(Pointer::<a href="#type-jsonpointer">jsonpointer()</a>, JsonTerm::<a href="jsg_json.md#type-json_term">jsg_json:json_term()</a>) -&gt; {ok, <a href="jsg_json.md#type-json_term">jsg_json:json_term()</a>} | false
</code></pre>
<br />

Evaluates the json pointer Pointer in the json value JsonTerm.

<a name="deref_relative_pointer-4"></a>

### deref_relative_pointer/4 ###

`deref_relative_pointer(LevelsUp, Continuation, Term, CurrentPointer) -> any()`

<a name="gen-1"></a>

### gen/1 ###

<pre><code>
gen(JsonTerm::<a href="jsg_jsg_json.md#type-json_term">jsg_jsg_json:json_term()</a>) -&gt; [{<a href="#type-jsonpointer">jsonpointer()</a>, <a href="jsg_jsg_json.md#type-json_term">jsg_jsg_json:json_term()</a>}]
</code></pre>
<br />

Generates all valid pairs {P,V} where P is a JSON Pointer and
V is its derreferenced value in a given JSON value.

<a name="list_is_integer-1"></a>

### list_is_integer/1 ###

<pre><code>
list_is_integer(S::string()) -&gt; boolean()
</code></pre>
<br />

Decides if a list represent an integer.

<a name="subst-3"></a>

### subst/3 ###

<pre><code>
subst(Pointer::<a href="#type-jsonpointer">jsonpointer()</a>, JsonTerm::<a href="jsg_jsg_json.md#type-json_term">jsg_jsg_json:json_term()</a>, NewValue::<a href="jsg_jsg_json.md#type-json_term">jsg_jsg_json:json_term()</a>) -&gt; <a href="jsg_jsg_json.md#type-json_term">jsg_jsg_json:json_term()</a>
</code></pre>
<br />

Evaluates the json pointer Pointer in the json value JsonTerm and
substitutes the json value by a new value

<a name="substl-4"></a>

### substl/4 ###

`substl(Pos, Pointer, T, NewValue) -> any()`

<a name="unref-2"></a>

### unref/2 ###

<pre><code>
unref(JsonRef::<a href="jsg_json.md#type-jsonterm">jsg_json:jsonterm()</a>, RootJsonTerm::<a href="jsg_json.md#type-jsonterm">jsg_json:jsonterm()</a>) -&gt; <a href="jsg_json.md#type-json_term">jsg_json:json_term()</a>
</code></pre>
<br />

Returns the json value indicated by the JsonRef using
RootJsonTerm as root document in case no URL part exists in the
URI.

