%%%-------------------------------------------------------------------
%%% @author ins
%%% @copyright (C) 2019, ins
%%% @doc
%%%
%%% @end
%%% Created : 2019-12-12 13:42:45.336296
%%%-------------------------------------------------------------------
-module(poloniex).

-behaviour(cryptoring_amqp_exchange).

%% API
-export([
         start_link/0
        ]).

%% cryptoring_amqp_exchange callbacks
-export([
         buy/3,
         sell/3,
         balances/0,
         subscribe_pair/1
        ]).

-define(SERVER, ?MODULE).

-include("exchange.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    cryptoring_amqp_exchange:start_link(?MODULE).

%%%===================================================================
%%% cryptoring_amqp_exchange callbacks
%%%===================================================================
buy(Pair, Price, Amount) ->
    Resp = poloniex_http_private:buy(Pair, Price, Amount),
    cryptoring_amqp_log:log(<<"order">>, Resp).

sell(Pair, Price, Amount) ->
    Resp = poloniex_http_private:sell(Pair, Price, Amount),
    cryptoring_amqp_log:log(<<"order">>, Resp).

balances() ->
    case poloniex_http_private:balances() of
        #{<<"error">> := E} ->
            lager:warning("Error getting balancies: ~p", [E]),
            Resp = #{<<"error">> => iolist_to_binary(io_lib:format("~p", [E]))},
            cryptoring_amqp_log:log(<<"error">>, Resp),
            Resp;
        Balancies ->
            maps:map(fun(_Coin, Data) ->
                             maps:map(fun(_K, V) ->
                                              binary_to_float(V)
                                      end,
                                      Data)
                     end,
                     Balancies)
    end.

subscribe_pair(Pair) ->
    poloniex_pair_sup:add_pair(Pair),
    poloniex_ws:subscribe(Pair).

                        

