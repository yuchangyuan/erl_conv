
-module(conv_gbk).
-export([start/0, init/0, to_ucs2/1, from_ucs2/1, to_utf8/1]).

start() ->
    register(?MODULE, spawn(?MODULE, init, [])).


init() ->
    {G2U, U2G} = conv_gbk_:init(),
    loop(G2U, U2G).

loop(G2U, U2G) ->
    receive
        {g2u, Gbk, Pid} ->
            case ets:lookup(G2U, Gbk) of
                [{_Gbk, Ucs2}] ->
                    Pid ! {ok, Ucs2};
                _ ->
                    Pid ! {error}
            end,
            loop(G2U, U2G);

        {u2g, Ucs2, Pid} ->
            case ets:lookup(U2G, Ucs2) of
                [{_Ucs2, Gbk}] ->
                    Pid ! {ok, Gbk};
                _ ->
                    Pid ! {error}
            end,
            loop(G2U, U2G);

        {stop} ->
            unregister(?MODULE),
            ok;

        _ ->
            loop(G2U, U2G)
    end.


rl2b(L) ->
    list_to_binary(lists:reverse(L)).

%% convert gbk to ucs2
to_ucs2(G) ->
    to_ucs2([], G).


%% return
to_ucs2(Ar, <<>>) ->
    {ok, rl2b(Ar)};

%% 1 byte
to_ucs2(Ar, <<2#0:1, D:7, L/binary>>) ->
    Ar1 = [ <<2#0:9, D:7>> | Ar],
    to_ucs2(Ar1, L);

%% 2 bytes
to_ucs2(Ar, <<2#1:1, D:15, L/binary>>) ->
    ?MODULE ! {g2u, <<2#1:1, D:15>>, self()},
    receive
        {ok, Ucs2} ->
            Ar1 = [ Ucs2 | Ar ],
            to_ucs2(Ar1, L);
        _ ->
            {error, rl2b(Ar), L}
    end;

to_ucs2(Ar, L) ->
    {error, rl2b(Ar), L}.



%% convert ucs2 to gbk
from_ucs2(Ucs2) ->
    from_ucs2([], Ucs2).

%% return
from_ucs2(Ar, <<>>) ->
    {ok, rl2b(Ar)};

%% 1 byte
from_ucs2(Ar, <<2#0:9, D:7, L/binary>>) ->
    Ar1 = [<<2#0:1, D:7>> | Ar],
    from_ucs2(Ar1, L);

%% 2 bytes
from_ucs2(Ar, <<D:16, L/binary>>) ->
    ?MODULE ! {u2g, <<D:16>>, self()},
    receive
        {ok, Gbk} ->
            Ar1 = [Gbk | Ar],
            from_ucs2(Ar1, L);
        _ ->
            {error, rl2b(Ar), L}
    end;

from_ucs2(Ar, L) ->
    {error, rl2b(Ar), L}.





%%% UTF-8 related

%% convert gbk to utf8
to_utf8(G) ->
    to_utf8([], G).


%% return
to_utf8(Ar, <<>>) ->
    {ok, rl2b(Ar)};

%% 1 byte
to_utf8(Ar, <<2#0:1, D:7, L/binary>>) ->
    Ar1 = [ <<2#0:1, D:7>> | Ar],
    to_utf8(Ar1, L);

%% 2 bytes
to_utf8(Ar, <<2#1:1, D:15, L/binary>>) ->
    ?MODULE ! {g2u, <<2#1:1, D:15>>, self()},
    receive
        {ok, Ucs2} ->
            case Ucs2 of
                <<2#0:9, D1:7>> ->
                    to_utf8([<<2#0:1, D1:7>> | Ar], L);
                <<2#0:5, D1:5, D2:6>> ->
                    to_utf8([<<2#110:3, D1:5, 2#10:2, D2:6>> | Ar], L);
                <<D1:4, D2:6, D3:6>> ->
                    Ar1 = [<<2#1110:4, D1:4, 2#10:2, D2:6, 2#10:2, D3:6>>
                           | Ar],
                    to_utf8(Ar1, L);
                _ ->
                    {error, rl2b(Ar), L}
            end;
        _ ->
            {error, rl2b(Ar), L}
    end;

to_utf8(Ar, L) ->
    {error, rl2b(Ar), L}.


