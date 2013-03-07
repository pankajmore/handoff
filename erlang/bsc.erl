%%%-------------------------------------------------------------------
%%% @author Pankaj More <pankajm@pankaj_y9402>
%%% @copyright (C) 2013, Pankaj More
%%% @doc
%%%
%%% @end
%%% Created :  6 Mar 2013 by Pankaj More <pankajm@pankaj_y9402>
%%%-------------------------------------------------------------------
-module(bsc).

-behaviour(gen_server).

%% API
-export([start_link/1, hoReq/1, hoCommand/1, hoConnect/2, hoConn/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {msc,bs}).

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
start_link(MSCId) ->
    gen_server:start_link(?MODULE, [MSCId], [{debug, [trace]}]).

hoReq(Pid) ->
    gen_server:call(Pid, hoReq). 

hoCommand(Pid) ->
    gen_server:call(Pid, hoCommand). 

hoConnect(Pid,FlushPayload) ->
    gen_server:cast(Pid, {hoConnect, FlushPayload}).

hoConn(Pid, FlushPayload) ->
    gen_server:cast(Pid, {hoConn, FlushPayload}).

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
init([MSCId]) ->
    {ok, BSId} = bs:start_link(self()),
    io:format("The BSId is ~p~n",[BSId]),
    {ok, #state{msc=MSCId,bs=BSId}}.


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
    case msc:hoReq(State#state.msc) of
        {hoCommand, Payload} -> 
            {reply, {hoAck, Payload}, State};
        reject -> 
            {reply, reject, State};
        _ ->
            error(baplyfromMSC) 
    end; 
handle_call(hoCommand, _From, State) ->
    case bs:hoCommand(State#state.bs) of
        {activation, Payload} ->
            {reply, {hoAck, Payload}, State}; 
        noFreeChannel ->
            {reply, dropCall, State}
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
handle_cast({hoConnect,FlushPayload}, State) ->
    msc:hoConnect(State#state.msc, FlushPayload),
    {noreply, State};
handle_cast({hoConn,FlushPayload}, State) ->
    %% TODO : Choose the correct BS
    case bs:flush(State#state.bs,FlushPayload) of 
        flush ->
            msc:flush(State#state.msc);
        _ ->
            error(baderrornearflushatbsc)
    end,
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
