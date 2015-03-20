-module(xml_utils).

-export([
    get_node_text/1,
    get_value/2,
    collapse_text/1
]).

-include("zotonic.hrl").
-include_lib("xmerl/include/xmerl.hrl").

get_node_text(Node) ->
    lists:concat(
        lists:map(
            fun(XmlText) -> 
                #xmlText{value=TextValue} = XmlText, 
                TextValue 
            end, 
            xmerl_xpath:string("/text()", Node)
        )
    ).

collapse_text(Node) when is_record(Node, xmlText) ->
    Node#xmlText.value;
collapse_text(Node) when is_record(Node, xmlElement) ->
    collapse_text(Node#xmlElement.content);
collapse_text(Node) when is_list(Node) ->
    [collapse_text(X) || X <- Node].

%% Get value from a node based on XPath
get_value(Xpath, Node) ->
    case xmerl_xpath:string(Xpath, Node) of
        [] -> undefined;
        [#xmlElement{content=[]}] ->
            %% Empty XML node
            undefined;
        [#xmlElement{content=[#xmlText{value=Value}]}] ->
            Value;
        Values ->
            %% List of text tuples
            collapse_text(Values)
    end.
