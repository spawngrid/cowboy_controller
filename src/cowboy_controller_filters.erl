-module(cowboy_controller_filters).
-export([fetch/2, tuple/2, app_env/2]).

fetch(PropList, Key) when is_list(PropList) ->
  proplists:get_value(Key, PropList, proplists:get_value(binary_to_atom(Key, latin1), PropList)).

tuple(T, Element) when is_tuple(T) ->
  element(Element, T).

app_env(AppB, EnvB) ->
   App = coerce_to_atom(AppB),
   Env = coerce_to_atom(EnvB),
   case application:get_env(App, Env) of
   	undefined -> undefined;
   	{ok, Value} -> Value
   end.

%%

coerce_to_atom(A) when is_atom(A) ->
	A;
coerce_to_atom(B) when is_binary(B) ->
    binary_to_atom(B, latin1).