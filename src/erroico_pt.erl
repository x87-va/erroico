%%%-------------------------------------------------------------------
%%% Трансформер блока do(begin ... end).
%%% Обёртывает вызовы функцией erroico:bind/3
%%%
%%%
%%%-------------------------------------------------------------------
-module(erroico_pt).

-import(lists, [map/2, reverse/1]).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
	NewForms = map(fun transform/1, Forms),
    % io:fwrite("NewForms = ~p~n", [NewForms]),
    NewForms.

transform({function, Line, Name, Num, Clauses}) ->
    NewClauses = map(fun transform_clause/1, Clauses),
    {function, Line, Name, Num, NewClauses};

transform(Form) ->
	Form.

transform_clause({clause, Line, Args, Guards, Body}) ->     
    NewBody = map(fun transform_body/1, Body),
    {clause, Line, Args, Guards, NewBody}.

transform_body(Form) ->
    case Form of
        %% plain_call()
        {call, _, {atom, _, do}, [{block, BlockLine, DoBlock}]} ->
            {block, BlockLine, bind_calls(DoBlock)};
        %% Match = call()
        {match, MatchLine, VarSpec, 
        {call, _, {atom, _, do}, [{block, BlockLine, DoBlock}]}} ->
            {match, MatchLine, VarSpec, 
                {block, BlockLine, bind_calls(DoBlock)}};
        %% fun()
        {'fun', Line, {clauses, Clauses}} ->
            {'fun', Line, {clauses, map(fun transform_clause/1, Clauses)}};
        %% Match = fun()
        {match, MatchLine, VarSpec, {'fun', Line, {clauses, Clauses}}} ->
            {match, MatchLine, VarSpec, 
            {'fun', Line, {clauses, map(fun transform_clause/1, Clauses)}}};
        %% call(fun(), Args)
        {call, CallLine, CallSpec, Args} ->
            {call, CallLine, CallSpec, map(fun transform_body/1, Args)};
        %% Match = call(fun(), Args)
        {match, MatchLine, VarSpec, {call, CallLine, CallSpec, Args}} ->
            {match, MatchLine, VarSpec, {call, CallLine, CallSpec, map(fun transform_body/1, Args)}};
        _Other ->
            Form
    end.

bind_calls([FirstMCall|RestMCalls]) ->
    {match, LineNum, 
        {var, LineNum, VarName}, Call} = FirstMCall,
    FirstResVarName = result_var_name(VarName),
    ModifiedFirstDoCall = {match, LineNum, 
                            {var, LineNum, FirstResVarName}, Call},
    BoundRestDoCalls = bind_calls(RestMCalls, [], [FirstResVarName]),
    % io:fwrite("Rest calls = ~p~n", [BoundRestDoCalls]),
    [ModifiedFirstDoCall|BoundRestDoCalls].

bind_calls([], BoundCalls, _) ->
    reverse(BoundCalls);

bind_calls([DoCall|RestDoCalls], BoundCalls, PrevResVarNames) ->
    case DoCall of
        {call, _, {atom, _, _}, _} when length(RestDoCalls) =:= 0 ->
            bind_calls([], [bind_call(DoCall, PrevResVarNames)|BoundCalls], 
                none);
        {match, LineNum, {var, LineNum, VarName}, Call} ->
            ResultVarName = result_var_name(VarName),
            BoundCall = bind_call(Call, PrevResVarNames),
            BoundMCall = {match, LineNum, {var, LineNum, ResultVarName},
                BoundCall},
            bind_calls(RestDoCalls, [BoundMCall|BoundCalls], 
                [ResultVarName|PrevResVarNames])
    end.

bind_call(Call, PrevResVarNames) ->
    case Call of
        %% Local call
        {call, LineNum, {atom, LineNum, Function}, Args} ->
            NewArgs = rename_args(Args, PrevResVarNames),
            {call, LineNum, {atom, LineNum, bind},
            [{'fun', LineNum, {function, Function, length(Args)}},
                build_cons_expr(NewArgs, LineNum),
             {var, LineNum, hd(PrevResVarNames)}]};
        %% Remote call
        {call, LineNum, {remote, LineNum,
        {atom, LineNum, Module}, {atom, LineNum, Function}}, Args} ->
            NewArgs = rename_args(Args, PrevResVarNames),
            {call, LineNum, {atom, LineNum, bind},
            [{'fun', LineNum, {function, {atom, LineNum, Module}, 
            {atom, LineNum, Function}, {integer, LineNum, length(Args)}}},
            build_cons_expr(NewArgs, LineNum),
            {var, LineNum, hd(PrevResVarNames)}]}
    end.

result_var_name(VarName) ->
    list_to_atom(atom_to_list(VarName) ++ "R").

rename_args(Args, PrevResVarNames) ->
    map(fun({var, ArgLine, ArgName} = Var) ->
                NewVarName = result_var_name(ArgName),
                case lists:member(NewVarName, PrevResVarNames) of
                    true ->
                        {var, ArgLine, NewVarName};
                    false ->
                        Var
                end;
            (Form) ->
                Form
        end, Args).

build_cons_expr([], Line) ->
    {nil, Line};
build_cons_expr([Element|Rest], Line) when is_integer(Element) ->
    {cons, Line, {integer, Line, Element}, build_cons_expr(Rest, Line)};

build_cons_expr([Element|Rest], Line) ->
    {cons, Line, Element, build_cons_expr(Rest, Line)}.
