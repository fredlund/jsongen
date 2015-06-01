

# Module jsg_json_validate #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


This module validates JSON data against JSON Schemas.
Copyright (c) 2013 Ángel Herranz, Lars-Ake Fredlund, Sergio Gil

__Authors:__ [`Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund  (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)`](mailto:Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund
  (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#validate-2">validate/2</a></td><td>
Validates a JSON value against a JSON Schema.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="validate-2"></a>

### validate/2 ###


<pre><code>
validate(Data::<a href="jsg_json.md#type-json_term">jsg_json:json_term()</a>, Schema::<a href="jsg_json.md#type-json_term">jsg_json:json_term()</a>) -&gt; boolean() | maybe
</code></pre>
<br />


Validates a JSON value against a JSON Schema.
WARNING. The function is not yet finished.
Returns true if the schema validates, false if it does not,
of maybe (due to unfinished implementation), if the validation
status is unknown.
