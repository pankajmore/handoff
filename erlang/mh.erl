%%%-------------------------------------------------------------------
%%% @author Pankaj More <pankajm@pankaj_y9402>
%%% @copyright (C) 2013, Pankaj More
%%% @doc
%%%
%%% @end
%%% Created :  6 Mar 2013 by Pankaj More <pankajm@pankaj_y9402>
%%%-------------------------------------------------------------------
-module(mh).

-behaviour(gen_server).

%% API
-export([start_link/0,attach/2, sendMeasurement/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {bs,tch}).

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

attach(Pid,BSId) ->
    gen_server:call(Pid, {attach, BSId}).

sendMeasurement(Pid) ->
    gen_server:call(Pid, measurement).

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
    {ok, #state{}}.

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
handle_call({attach,BSId}, _From, State) ->
    case bs:alloc(BSId) of 
        {ok, TCH} -> 
            NewState = State#state{bs=BSId,tch=TCH},
            {reply, {ok, TCH} , NewState};
        noChannelFree ->
            {reply, notFree, State}
    end;
handle_call(measurement, _From, State) ->
    case bs:measurement(State#state.bs) of 
        {linkActive,{NewBSId,NewTCH}} ->
            case bs:linkActivationReq(NewBSId,NewTCH) of 
                linkEstablished ->
                    {reply, linkEstablished, State};
                _ -> 
                    error(reason)
            end;
        dropCall ->
            {reply, callDropped, State};
        noHandoffRequired -> 
            {reply, noHandoffRequired, State}
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
