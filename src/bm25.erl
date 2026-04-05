%%%-------------------------------------------------------------------
%%% Copyright 2026 hyperimpose.org
%%% SPDX-License-Identifier: BSD-3-Clause
%%%-------------------------------------------------------------------

-compile({inline, [idf/3]}).
-compile(inline_list_funcs).

-module(bm25).

-include_lib("kernel/include/logger.hrl").

-export([init/1, search/2, search/3, add/3, delete/2]).


-record(document, {tokens = [],  % The document tokenized
                   doclen = 0,   % Length of document in words -> length(tokens)
                   fqi = #{}}).  % The times each token occurs in the document

-record(bm25, {%% Documents
               docs = #{} :: #{Key :: term() => #document{}},
               inverted = #{} :: #{Token :: term() => Keys :: [term()]}, % Index
               %% BM25
               avgdl = 0.0 :: float(),  % Average document length
               sumdl = 0 :: integer(),  % Only used for calculating avgdl
               n = 0,  % Total number of documents in the collection
               %% IDF
               nqi = #{},  % The number of documents that contain each term
               %% Options
               tokenize :: fun((term()) -> [term()])  % Token Pipeline
}).


-spec init(Opt :: #{tokenize := fun((term()) -> [term()])}) -> #bm25{}.

init(#{tokenize := Tokenize}) ->
    #bm25{tokenize = Tokenize}.


-spec search(#bm25{}, term()) -> [{term(), float()}].

search(Index, Query) ->
    search(Index, Query, #{}).


-spec search(#bm25{}, term(), map()) -> [{term(), float()}].

search(Index, Query, Opts) ->
    #bm25{docs = Docs, inverted = Inv,
          avgdl = Avgdl, n = N, nqi = NQi, tokenize = Tn} = Index,

    %% Options
    Defaults = #{k1 => 1.2, b => 0.75},
    #{k1 := K1, b := B} = maps:merge(Defaults, Opts),

    %% Tokenize query
    Q = Tn(Query),

    %% Find relevant documents in the inverted index
    Keys = lists:usort(lists:flatmap(fun (T) -> maps:get(T, Inv, []) end, Q)),

    %% BM25 Scoring
    QiIdf = [{Qi, idf(Qi, NQi, N)} || Qi <- Q],  % Precompute IDF

    F = fun (Key) ->
                #{Key := #document{doclen = DLen, fqi = FQi}} = Docs,

                %% Precompute K1 * Document length Normalization Factor
                Nf = K1 * (1.0 - B + B * (DLen / Avgdl)),

                {Key, bm25(QiIdf, FQi, K1, Nf, 0.0)}
        end,
    Results = lists:map(F, Keys),

    lists:sort(fun({_, S1}, {_, S2}) -> S1 > S2 end, Results).


bm25([{Qi, IDF} | Rest], FQiMap, K1, Nf, Acc) ->
    case FQiMap of
        #{Qi := FQi} ->
            S = IDF * ((FQi * (K1 + 1.0)) / (FQi + Nf)),
            bm25(Rest, FQiMap, K1, Nf, Acc + S);
        _Else        ->
            bm25(Rest, FQiMap, K1, Nf, Acc)  % FQi = 0
    end;
bm25([], _FQiMap, _K1Plus1, _Nf, Acc) ->
    Acc.


idf(Qi, NQiMap, N) ->
    NQi = maps:get(Qi, NQiMap, 0),
    math:log(((N - NQi + 0.5) / (NQi + 0.5)) + 1).


-spec add(Index :: #bm25{}, Key :: term(), Doc :: term()) -> #bm25{}.

add(Index, Key, Doc) ->
    #bm25{docs = Docs, inverted = Inv,
          sumdl = Sumdl, n = N, nqi = NQi, tokenize = Tn} = Index,

    case maps:is_key(Key, Docs) of
        true -> error({duplicate_key, Key});
        false -> ok
    end,

    Tokens = Tn(Doc),
    DocLen = length(Tokens),

    %% Calculate the number of times each term is found in the document.
    FunFQi = fun (T, Acc) ->
                     maps:update_with(T, fun(V) -> V + 1 end, 1, Acc)
             end,
    FQi = lists:foldl(FunFQi, #{}, Tokens),

    %% Calculate the number of documents that contain each term.
    FunNQi = fun (T, Acc) ->
                     maps:update_with(T, fun(V) -> V + 1 end, 1, Acc)
             end,
    NQi1 = lists:foldl(FunNQi, NQi, lists:uniq(Tokens)),

    %% Update the inverted index
    FunInv = fun (T, Acc) ->
                     maps:update_with(T, fun(V) -> [Key | V] end, [Key], Acc)
             end,
    Inv1 = lists:foldl(FunInv, Inv, lists:uniq(Tokens)),

    N1 = N + 1,
    Sumdl1 = Sumdl + DocLen,

    Document = #document{tokens = Tokens, doclen = DocLen, fqi = FQi},
    Index#bm25{docs = Docs#{Key => Document}, inverted = Inv1,
               avgdl = Sumdl1 / N1, sumdl = Sumdl1, n = N1, nqi = NQi1}.


-spec delete(#bm25{}, term()) -> #bm25{}.

delete(Index, Key) ->
    #bm25{docs = Docs, inverted = Inv, sumdl = Sumdl, n = N, nqi = NQi} = Index,

    maybe
        {#document{tokens = Tokens}, Docs1} ?= maps:take(Key, Docs),

        N1 = N - 1,
        Sumdl1 = Sumdl - length(Tokens),
        Avgdl1 = if
                     N1 == 0 -> 0;
                     true    -> Sumdl1 / N1
                 end,

        %% Calculate the number of documents that contain each term.
        FunNQi = fun (Token, Acc) ->
                         case maps:get(Token, Acc, 0) of
                             I when I > 1 -> Acc#{Token => I - 1};
                             _Else        -> maps:remove(Token, Acc)
                         end
                 end,
        NQi1 = lists:foldl(FunNQi, NQi, lists:uniq(Tokens)),

        %% Update the inverted index
        FunInv = fun (Token, Acc) ->
                         maybe
                             I1 = [_|_] ?= maps:get(Token, Acc, []),
                             I2 = [_|_] ?= lists:delete(Key, I1),
                             Acc#{Token => I2}
                         else
                             [] -> maps:remove(Token, Acc)
                         end
                 end,
        Inv1 = lists:foldl(FunInv, Inv, lists:uniq(Tokens)),

        Index#bm25{docs = Docs1, inverted = Inv1,
                   avgdl = Avgdl1, sumdl = Sumdl1, n = N1, nqi = NQi1}
    end.
