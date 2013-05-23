%%%-------------------------------------------------------------------
%%% Модуль содержит основные функции для реализации поведения монад State,
%%% Error языка Haskell.
%%%-------------------------------------------------------------------
-module(erroico).

-import(lists, [map/2]).
-export([do/1, bind/3]).

do(Ex) ->
	Ex.

bind(Fun, Args, PrevResult) ->
    case PrevResult of
        {ok, nothing} = Nothing ->
            Nothing;
        {ok, _Value} ->
            apply(Fun, map(fun extract_result/1, Args));
        {error, _Why} = Error ->
            Error
	end.

extract_result({ok, Value}) ->
    Value;
extract_result(Value) ->
    Value.
