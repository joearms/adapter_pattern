-module(params).
-compile(export_all).

abc(X, Y) ->
    {abcCalled, X, Y}.

%% test of parameterised modules calling sequences

%% {params,1,2,3,4,5}:abc(123)
%%   => {abcCalled, 123, {foo,1,2,3,4,5}
