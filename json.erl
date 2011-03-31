-module(json).
-export([term_to_json/1]).


to_string(S) when is_list(S) ->
    S;
to_string(S) when is_binary(S) ->
    binary_to_list(S).


%% Quote
% backspace
quote_char($\\) -> [$\\, $\\];
% quote
quote_char(34) -> [$\\, 34];
% new line
quote_char($\n) -> [$\\, $n];
% backspace
quote_char($\b) -> [$\\, $b];
% form feed
quote_char($\f) -> [$\\, $f];
% carriage return
quote_char($\r) -> [$\\, $r];
% tab
quote_char($\t) -> [$\\, $t];
% other
quote_char(I) ->
    if
        32 =< I andalso I =< 126 ->
            [I];
        true -> Hex = hex:int_to_hex(I),
                true = string:len(Hex) =< 4,
                [[$\\, $u] ++ string:right(Hex, 4, $0)]
    end.

quote(S) ->
    "\"" ++ lists:flatten(lists:map(fun(X) -> quote_char(X) end, S)) ++ "\"".


%% Term to JSON
term_to_json(true) ->
    "true";

term_to_json(false) ->
    "false";

term_to_json(null) ->
    "null";

term_to_json(I) when is_integer(I) ->
    integer_to_list(I);

term_to_json(F) when is_float(F) ->
    float_to_list(F);

term_to_json(B) when is_binary(B) ->
    quote(binary_to_list(B));

term_to_json(L) when is_list(L) ->
    Vals = lists:map(fun(X) -> term_to_json(X) end, L),
    "[" ++ string:join(Vals, ",") ++ "]";

term_to_json(D) when is_tuple(D) ->
    Lst = dict:to_list(D),
    Vals = lists:map(fun({Key, Val}) ->
                             quote(to_string(Key)) ++ ":" ++ term_to_json(Val) end, Lst),
    "{" ++ string:join(Vals, ",") ++ "}".



%% JSON to Term

