%%%-------------------------------------------------------------------
%%% @author Pankaj More <pankajm@pankaj_y9402>
%%% @copyright (C) 2013, Pankaj More
%%% @doc
%%%
%%% @end
%%% Created :  6 Mar 2013 by Pankaj More <pankajm@pankaj_y9402>
%%%-------------------------------------------------------------------
-module(bs).

-behaviour(gen_server).

%% API
-export([start_link/1, measurement/1, hoCommand/1, linkActivationReq/2, flush/2, alloc/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {bsc,busyChannels=[],freeChannels=lists:seq(1,14)}).

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
start_link(BSCId) ->
    gen_server:start_link(?MODULE, [BSCId], [{debug, [trace]}]).

measurement(Pid) ->
     gen_server:call(Pid,measurement). 

hoCommand(Pid) ->
    gen_server:call(Pid, hoCommand).

linkActivationReq(Pid,NewTCH) ->
    gen_server:call(Pid, {linkActivationReq, NewTCH}).

flush(Pid, FlushPayload) ->
    gen_server:call(Pid, {flush, FlushPayload}).

alloc(Pid) ->
    gen_server:call(Pid, alloc).

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
init([BSCId]) ->
    {ok, #state{bsc=BSCId}}.


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

handle_call(alloc, {Pid,_}, State) ->
    case State#state.freeChannels of 
        [] ->
            {reply, noChannelFree, State};
        [H|T] ->
            NewState = State#state{freeChannels=T,busyChannels=[{H,Pid}|State#state.busyChannels]},
            {reply, {ok,H}, NewState}
    end;
handle_call(measurement, _From, State) ->
    %% TODO : Decide handoff condition
    case bsc:hoReq(State#state.bsc) of 
        {hoAck, Payload} ->
            {reply, {linkActive, Payload}, State};
        reject ->
            {reply, dropCall, State}
    end;
handle_call(hoCommand, _From, State) ->
    case State#state.freeChannels of 
        [H|T] ->
            NewTCH = H,
            NewState = State#state{freeChannels=T},
            Payload = {self(), NewTCH},
            {reply, {activation, Payload}, NewState};
        [] -> 
            {reply, noFreeChannel, State}
    end;
handle_call({linkActivationReq, NewTCH}, {Pid,_}, State) ->
    NewState = State#state{busyChannels = [{NewTCH,Pid}|State#state.busyChannels]},
    %%    gen_server:reply(From, linkEstablished),
    FlushPayload = Pid,
    bsc:hoConnect(State#state.bsc,FlushPayload), %% its a cast so no problem
    {reply, linkEstablished, NewState};
handle_call({flush, MHId}, _From, State) ->
    %% Flush the old TCH and make it free
    [{TCH,MHId}] = [{T,Pid} || {T,Pid} <- State#state.busyChannels, Pid == MHId],
    BC = State#state.busyChannels -- [{TCH,MHId}],
    FC = [TCH|State#state.freeChannels],
    NewState = State#state{busyChannels = BC,freeChannels = FC}, 
    {reply, flush, NewState}.

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
handle_cast(_Msg, State) ->
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
