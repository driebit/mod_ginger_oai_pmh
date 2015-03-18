-module(http_utils).

-export([
    urlencode/1,
    urlencode/2
]).

-include("zotonic.hrl").

urlencode(QueryStringParams) ->
    RevPairs = lists:foldl(
        fun ({K, V}, Acc) ->
            [[mochiweb_util:quote_plus(K), $=, mochiweb_util:quote_plus(V)] | Acc]
        end, 
        [], 
        QueryStringParams
    ),
    lists:flatten(revjoin(RevPairs, "&", [])).
    
urlencode(Url, QueryStringParams) ->
    Url ++ "?" ++ urlencode(QueryStringParams).

revjoin([], _Separator, Acc) ->
    Acc;
revjoin([S | Rest], Separator, []) ->
    revjoin(Rest, Separator, [S]);
revjoin([S | Rest], Separator, Acc) ->
    revjoin(Rest, Separator, [S, Separator | Acc]).
