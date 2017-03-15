

# Welcome to the jsongen library. #

__Authors:__ [`Lars-Ake Fredlund  (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com), Angel Herranz (aherranz@fi.upm.es)`](mailto:Lars-Ake Fredlund  (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com), Angel Herranz (aherranz@fi.upm.es)).

Jsongen is a library for generating QuickCheck (quviq.com) generators from descriptions of JSON data using JSON schemas.


## Build, Test, and Generate Markdown Docs ##

Jsongen requires [rebar3](http://www.rebar3.org) for
building and testing.  See [here](http://www.rebar3.org/v3.0/docs/getting-started) for
getting started with rebar3.

To compile, execute the following command:<br />

```
$ rebar3 compile
```


After compilation Erlang beam files will be left in the
directory _build/default/lib/jsongen/ebin/.

Should you wish to install the Jsongen library in the standard
Erlang library structure, the following commands can be used:<br />
```
$ make install
```
</p>

<p>
To generate Markdown docs, execute the following command:<br/>
```
$ env ERL_LIBS=$PWD/_build/default/lib/edown rebar3 edoc
```




## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="java_validator.md" class="module">java_validator</a></td></tr>
<tr><td><a href="jesse_validator.md" class="module">jesse_validator</a></td></tr>
<tr><td><a href="js_links_machine.md" class="module">js_links_machine</a></td></tr>
<tr><td><a href="jsg_gen_string_from_regexp.md" class="module">jsg_gen_string_from_regexp</a></td></tr>
<tr><td><a href="jsg_json.md" class="module">jsg_json</a></td></tr>
<tr><td><a href="jsg_json_validate.md" class="module">jsg_json_validate</a></td></tr>
<tr><td><a href="jsg_jsonref.md" class="module">jsg_jsonref</a></td></tr>
<tr><td><a href="jsg_jsonschema.md" class="module">jsg_jsonschema</a></td></tr>
<tr><td><a href="jsg_links.md" class="module">jsg_links</a></td></tr>
<tr><td><a href="jsg_links_utils.md" class="module">jsg_links_utils</a></td></tr>
<tr><td><a href="jsg_main.md" class="module">jsg_main</a></td></tr>
<tr><td><a href="jsg_regexp_parse.md" class="module">jsg_regexp_parse</a></td></tr>
<tr><td><a href="jsg_regexp_parser.md" class="module">jsg_regexp_parser</a></td></tr>
<tr><td><a href="jsg_regexp_scan.md" class="module">jsg_regexp_scan</a></td></tr>
<tr><td><a href="jsg_rtest.md" class="module">jsg_rtest</a></td></tr>
<tr><td><a href="jsg_store.md" class="module">jsg_store</a></td></tr>
<tr><td><a href="jsg_urigen.md" class="module">jsg_urigen</a></td></tr>
<tr><td><a href="jsg_utils.md" class="module">jsg_utils</a></td></tr>
<tr><td><a href="jsl_dynamic_links.md" class="module">jsl_dynamic_links</a></td></tr>
<tr><td><a href="jsongen.md" class="module">jsongen</a></td></tr>
<tr><td><a href="jsongen_app.md" class="module">jsongen_app</a></td></tr>
<tr><td><a href="jsongen_install.md" class="module">jsongen_install</a></td></tr>
<tr><td><a href="jsongen_sup.md" class="module">jsongen_sup</a></td></tr>
<tr><td><a href="jsongen_validator.md" class="module">jsongen_validator</a></td></tr>
<tr><td><a href="uri_template.md" class="module">uri_template</a></td></tr></table>

