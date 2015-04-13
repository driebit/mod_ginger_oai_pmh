%% @author Driebit <tech@driebit.nl>
%% @copyright 2015

-module(mod_ginger_oai_pmh).
-author("Driebit <tech@driebit.nl>").

-mod_title("Ginger OAI-PMH import").
-mod_description("Import OAI-PMH files and endpoints").
-mod_prio(500).
-mod_depends([mod_ginger_xml]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).
-export([import/1, import/2]).

-include_lib("zotonic.hrl").
-record(state, {context}).

start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    z_context:lager_md(Context),
    {ok, #state{context=z_context:new(Context)}}.

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast({import, Endpoint, UrlParams}, State=#state{context=Context}) ->
    oai_pmh:import(Endpoint, UrlParams, Context),
    {noreply, State};

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

import(Endpoint) ->
    import(Endpoint, []).

import(Endpoint, UrlParams) ->
    gen_server:cast(?MODULE, {import, Endpoint, UrlParams}).
