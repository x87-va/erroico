%%%-------------------------------------------------------------------
%%% Трансформер блока do(begin ... end).
%%% 
%%% Выводит отладочную информацию после преобразования.
%%%
%%%-------------------------------------------------------------------
-module(erroico_pt_debug).

-import(lists, [map/2, reverse/1]).

-export([parse_transform/2]).

parse_transform(Forms, Options) ->
	NewForms = erroico_pt:parse_transform(Forms, Options),
    io:fwrite("NewForms = ~p~n", [NewForms]),
    NewForms.
