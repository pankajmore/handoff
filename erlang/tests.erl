%%%-------------------------------------------------------------------
%%% @author Pankaj More <pankajm@pankaj_y9402>
%%% @copyright (C) 2013, Pankaj More
%%% @doc
%%%
%%% @end
%%% Created :  7 Mar 2013 by Pankaj More <pankajm@pankaj_y9402>
%%%-------------------------------------------------------------------
-module(tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

%% Normal Handoff
normalHandoff() ->
    {ok, MSC} = msc:start_link(),
    {ok, BSC1} = bsc:start_link(),
    {ok, BSC2} = bsc:start_link(),
    {ok, BS1} = bs:start_link(),
    {ok, BS2} = bs:start_link(),
    bsc:joinMSC(BSC1,MSC),
    bsc:joinMSC(BSC2,MSC),
    bs:joinBSC(BS1,BSC1),
    bs:joinBSC(BS2,BSC2),
    {ok, MH} = mh:start_link(),
    mh:attach(MH,BS1), 
    mh:sendMeasurement(MH),
    ok.

%% Call dropped if no free channel
noFreeChannel(NoOfMHsAtBS2) ->
    {ok, MSC} = msc:start_link(),
    {ok, BSC1} = bsc:start_link(),
    {ok, BSC2} = bsc:start_link(),
    {ok, BS1} = bs:start_link(),
    {ok, BS2} = bs:start_link(),
    bsc:joinMSC(BSC1,MSC),
    bsc:joinMSC(BSC2,MSC),
    bs:joinBSC(BS1,BSC1),
    bs:joinBSC(BS2,BSC2),
    {ok, MH} = mh:start_link(),
    mh:attach(MH,BS1),
    fillBS(BS2,NoOfMHsAtBS2),
    mh:sendMeasurement(MH).
    

%% allocate TCH on BS
fillBS(BSId,Acc) when Acc > 0 ->
    {ok,MH} = mh:start_link(),
    mh:attach(MH,BSId),
    fillBS(BSId, Acc-1);
fillBS(_,0) -> return.
