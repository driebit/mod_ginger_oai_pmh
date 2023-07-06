-module(oai_pmh).

-export([
    import_file/2,
    import/2,
    import/3
]).

-include("zotonic.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% Total request timeout
-define(HTTPC_TIMEOUT, 20000).

%% Connect timeout, server has to respond before this
-define(HTTPC_TIMEOUT_CONNECT, 10000).


-record(import, {endpoint = undefined,
                 context = undefined,
                 resumption_token = undefined,
                 records = undefined,
                 url_params = []}).

%%-----------------------------------------------------------------------------
%% Module API
%%-----------------------------------------------------------------------------

%% @doc Send a notification for each record in a OAI-PMH XML file
import_file(File, Context) ->
    {Root, []} = xmerl_scan:file(File, [{space, normalize}]),
    Records = parse_records(Root),
    lists:foreach(fun(R) -> z_notifier:notify_sync({oai_pmh_import, R}, Context) end,
                  Records).

%% @doc Send a notification for each record retrieved from an OAI-PMH endpoint
import(Endpoint, Context) ->
    import(Endpoint, [], Context).

%% @doc Send a notification for each record retrieved from an OAI-PMH endpoint
import(Endpoint, UrlParams, Context) ->
    {Records, Token} = list_records(Endpoint, UrlParams),
    import(#import{records = Records,
                   resumption_token = Token,
                   endpoint = Endpoint,
                   context = Context,
                   url_params = UrlParams}).

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------

import(#import{records = [], resumption_token = undefined}) ->
    ok;
import(#import{records = []} = Args) ->
    {Records, NewToken} = list_records(Args#import.endpoint,
                                       Args#import.url_params,
                                       Args#import.resumption_token),
    import(Args#import{records = Records, resumption_token = NewToken});
import(#import{} = Args) ->
    lists:foreach(
        fun(R) ->
            z_notifier:notify_sync({oai_pmh_import, R,
                #{rdf_property:dcterms(<<"source">>) => Args#import.endpoint}},
                Args#import.context)
        end,
        Args#import.records
     ),
    import(Args#import{records = []}).


%% @doc Execute ListRecords call on endpoint
list_records(Endpoint, UrlParams) ->
    case request(Endpoint, [{verb, "ListRecords"}] ++ UrlParams) of
        {ok, Response} ->
            {XmlRoot, _} = xmerl_scan:string(Response, [{space, normalize}]),
            %% Retrieve resumption token
            ResumptionToken = ginger_xml:get_value("//resumptionToken", XmlRoot),
            Records = parse_records(XmlRoot),
            lager:info("OAI-PMH fetch from ~s returned ~p records", [ Endpoint, length(Records) ]),
            {Records, ResumptionToken};
        _Other ->
            % Error was logged before
            {[], undefined}
    end.

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
    HttpOptions = [
        {relaxed, true},
        {timeout, ?HTTPC_TIMEOUT},
        {connect_timeout, ?HTTPC_TIMEOUT_CONNECT}
    ],
    Options = [
        {body_format, string}
    ],
    case httpc:request(get, {Url, []}, HttpOptions, Options) of
        {ok, {{_HTTP, 200, _OK}, _Headers, Body }} ->
            lager:info("OAI-PMH fetch from ~s was ok", [ Url ]),
            {ok, Body};
        Response ->
            lager:error("OAI-PMH fetch from ~s returned ~p", [ Url, Response ]),
            Response
    end.

parse_records(XmlRoot) ->
    xmerl_xpath:string("//record", XmlRoot).
