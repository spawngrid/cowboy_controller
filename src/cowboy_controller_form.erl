-module(cowboy_controller_form).
-export([parse/1]).
-include_lib("cowboy/include/http.hrl").

parse(#http_req{} = Req) ->
    {Form, Req1} = cowboy_http_req:body_qs(Req),
    {parse(Form), Req1};

parse(Form) ->
    parse_1(Form, []).

parse_1([], Acc) ->
    Acc;
parse_1([{Key, Value}|T], Acc) ->
    Keys = binary:split(Key,[<<$[>>,<<$]>>],[global,trim]),
    Acc1 = parse_field(Keys, Value, Acc),
    parse_1(T, Acc1).

parse_field([], Value, _Acc) ->
    Value;
parse_field([<<>>|T], Value, Acc) ->
    parse_field(T, Value, Acc);
parse_field(L, Value, Acc) when not is_list(Acc) ->
    parse_field(L, Value, []);
parse_field([H|T], Value, Acc) ->
    Dict = proplists:get_value(H, Acc, []),
    Dict1 = parse_field(T, Value, Dict),
    lists:keystore(H, 1, Acc, {H, Dict1}).
