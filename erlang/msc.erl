%%%-------------------------------------------------------------------
%%% @author Pankaj More <pankajm@pankaj_y9402>
%%% @copyright (C) 2013, Pankaj More
%%% @doc
%%%
%%% @end
%%% Created :  6 Mar 2013 by Pankaj More <pankajm@pankaj_y9402>
%%%-------------------------------------------------------------------
-module(msc).

-behaviour(gen_server).

%% API
-export([start_link/0, hoReq/1, hoConnect/2,flush/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {bsc}).

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
    gen_server:start_link(?MODULE, [], [{debug, [trace]}]).

hoReq(Pid) ->
    gen_server:call(Pid, hoReq). 

hoConnect(Pid, FlushPayload) ->
    gen_server:cast(Pid, {hoConnect,FlushPayload}). 

flush(Pid) ->
    gen_server:cast(Pid, flush).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, BSC1Id} = bsc:start_link(self()),
    {ok, BSC2Id} = bsc:start_link(self()),
    io:format("The pid of bsc1 is ~p~n ", [BSC1Id]),
    io:format("The pid of bsc2 is ~p~n ", [BSC2Id]),
    {ok, #state{bsc=[BSC1Id,BSC2Id]}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(hoReq, _From, State) ->
    case bsc:hoCommand(lists:nth(2, State#state.bsc)) of
        {hoAck, Payload} -> 
            {reply, {hoCommand, Payload}, State};
        dropCall ->
            io:format("Call Dropped!~n"),
            {reply, reject, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({hoConnect, FlushPayload}, State) ->
    %% TODO : send to the corrrect bsc
    bsc:hoConn(hd(State#state.bsc), FlushPayload),
    {noreply, State};
handle_cast(flush, State) ->
    io:format("Handoff Successful!~n"),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
