
-module(conv_utf8).
-export([to_ucs2/1, from_ucs2/1]).
-import(lists,[reverse/1]).

to_ucs2(Utf8) ->
    to_ucs2([], Utf8).

%% convert utf-8 to ucs2
to_ucs2(Acc, <<>>) -> % empty -> empty
    {ok, list_to_binary(reverse(Acc))};

%% 1 byte
to_ucs2(Acc, <<2#0:1, D1:7, L/binary>>) ->
    Acc1 = [<<2#0:9, D1:7>> | Acc],
    to_ucs2(Acc1, L);

%% 2 bytes
to_ucs2(Acc, <<2#110:3, D1:5, 2#10:2, D2:6, L/binary>>) ->
    Acc1 = [<<2#0:5, D1:5, D2:6>> | Acc],
    to_ucs2(Acc1, L);

%% 3 bytes
to_ucs2(Acc, <<2#1110:4, D1:4, 2#10:2, D2:6, 2#10:2, D3:6, L/binary>>) ->
    Acc1 = [<<D1:4, D2:6, D3:6>> | Acc],
    to_ucs2(Acc1, L);

%% error
to_ucs2(Acc, Left) ->
    {error, list_to_binary(reverse(Acc)), Left}.





from_ucs2(Ucs2) ->
    from_ucs2([], Ucs2).


%% convert ucs2 to utf-8
from_ucs2(Acc, <<>>) ->
    {ok, list_to_binary(reverse(Acc))};

%% 0x00 -> 0x7f
from_ucs2(Acc, <<2#0:9, D1:7, L/binary>>) ->
    Acc1 = [<<2#0:1, D1:7>> | Acc],
    from_ucs2(Acc1, L);

%% 0x80 -> 0x7ff
from_ucs2(Acc, <<2#0:5, D1:5, D2:6, L/binary>>) ->
    Acc1 = [<<2#110:3, D1:5, 2#10:2, D2:6>> | Acc],
    from_ucs2(Acc1, L);

%% 0x800 -> 0xffff
from_ucs2(Acc, <<D1:4, D2:6, D3:6, L/binary>>) ->
    Acc1 = [<<2#1110:4, D1:4, 2#10:2, D2:6, 2#10:2, D3:6>> | Acc],
    from_ucs2(Acc1, L);

%% error
from_ucs2(Acc, Left) ->
    {error, list_to_binary(reverse(Acc)), Left}.

    
    
