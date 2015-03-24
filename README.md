mod_ginger_oai_pmh
==================

A Zotonic module for harvesting [OAI-PMH](http://www.openarchives.org/pmh/) endpoints 
and files.

Usage
-----

### Register an observer

When starting an import, an `oai_pmh_import` notification will be sent for each 
record. So, to process the OAI-PMH data, register one or more observers:
 
```erlang
observe_oai_pmh_import({oai_pmh_import, Record}, Context) ->
    SomeField = xmerl_xpath:string("//title", Record),
    %% do more processing...
```

Then go ahead and either import an endpoint or a file as explained below.

### Import records from an endpoint

To import records directly from an OAI-PMH endpoint:

```erlang
oai_pmh:import("http://example.com/oai-pmh", Context).
```

You can supply a tuple list of URL params as second argument:

```erlang
oai_pmh:import("http://example.com/oai-pmh", [{apiKey, "some_api_key"}], Context).
```

### Import records from a file

If you have already downloaded an XML file that contains a full OAI-PMH 
collection, you can import that, too:

```erlang
oai_pmh:import_file("your/file.xml", Context).
```
