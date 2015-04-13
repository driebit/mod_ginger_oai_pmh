-module(oai_pmh).

-export([
    import_file/2,
    import/2,
    import/3
]).

-include("zotonic.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% @doc Send a notification for each record in a OAI-PMH XML file
import_file(File, Context) ->
    {Root, _} = xmerl_scan:file(File, [{space, normalize}]),
    Records = parse_records(Root),
    [z_notifier:notify({oai_pmh_import, R}, Context) || R <- Records].

%% @doc Send a notification for each record retrieved from an OAI-PMH endpoint
import(Endpoint, Context) ->
    import(Endpoint, [], Context).

%% @doc Send a notification for each record retrieved from an OAI-PMH endpoint
import(Endpoint, UrlParams, Context) ->
    import([], Endpoint, UrlParams, Context).

import([], Endpoint, UrlParams, Context) ->
    {Records, ResumptionToken} = list_records(Endpoint, UrlParams),
    import(Records, Endpoint, UrlParams, ResumptionToken, Context).

import([], _Endpoint, _UrlParams, undefined, _Context) ->
    %% Empty resumption token, so reached end of data
    ok;
    
import([], Endpoint, UrlParams, ResumptionToken, Context) ->
    {Records, NewResumptionToken} = list_records(Endpoint, UrlParams, ResumptionToken),
    import(Records, Endpoint, UrlParams, NewResumptionToken, Context);

import(Records, Endpoint, UrlParams, ResumptionToken, Context) ->
    [z_notifier:notify({oai_pmh_import, R}, Context) || R <- Records],
    import([], Endpoint, UrlParams, ResumptionToken, Context).

%% @doc Execute ListRecords call on endpoint
list_records(Endpoint, UrlParams) ->
    Response = request(Endpoint, [{verb, "ListRecords"}] ++ UrlParams),
    {XmlRoot, _} = xmerl_scan:string(Response, [{space, normalize}]),
    
    %% Retrieve resumption token
    ResumptionToken = ginger_xml:get_value("//resumptionToken", XmlRoot),
    {parse_records(XmlRoot), ResumptionToken}.

%% @doc Execute resume request
list_records(Endpoint, UrlParams, ResumptionToken) ->
    %% Remove params that OAI-PMH does not allow in resume requests  
    ResumeParams = proplists:delete(metadataPrefix, 
        proplists:delete(set, UrlParams)
    ) ++ [{resumptionToken, ResumptionToken}],
    list_records(Endpoint, ResumeParams).

%% @doc Execute request to OAI-PMH endpoint
request(Endpoint, UrlParams) ->
    Url = http_utils:urlencode(Endpoint, UrlParams),
    case httpc:request(Url) of
        {ok, {
            {_HTTP, 200, _OK},
            _Headers,
            Body
        }} ->
            Body;
        Response ->
            Response
    end.
    
parse_records(XmlRoot) ->
    xmerl_xpath:string("//record", XmlRoot).
