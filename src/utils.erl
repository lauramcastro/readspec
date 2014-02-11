%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Generic utilities
%%% @end
%%% Created : 10 Feb 2014 by Pablo Lamela

-module(utils).

-export([to_one_list/1, remount_list/2, bt_mapfold_list_of_lists/3,
	 bt_mapfold/3]).

to_one_list(ListOfLists) ->
    {lists:append(ListOfLists), lists:map(fun erlang:length/1, ListOfLists)}.

remount_list([], []) -> [];
remount_list([Num|RestN], List) ->
    {FirstPiece, SecondPiece} = lists:split(Num, List),
    [FirstPiece|remount_list(RestN, SecondPiece)].

bt_mapfold_list_of_lists(Fun, Acc0, ListOfLists) ->
    {OneList, Sizes} = to_one_list(ListOfLists),
    {ResOneList, AccF} = bt_mapfold(Fun, Acc0, OneList),
    {remount_list(Sizes, ResOneList), AccF}.

bt_mapfold(Fun, Acc0, List1) ->
    bt_mapfold(Fun, Acc0, List1, [], []).
bt_mapfold(Fun, Acc0, [H0|Tail], List, BackTrackingList) ->
    case Fun(H0, Acc0) of
	{success, H1, Acc1} ->
	    bt_mapfold(Fun, Acc1, Tail, [H1|List],
		       [{Fun, Acc0, Tail, [H0|List]}
		        |BackTrackingList]);
	{failure, H1, Acc1} ->
	    bt_mapfold(Fun, Acc1, Tail, [H1|List],
		       BackTrackingList);
	{finished, H1, Acc1} ->
	    {lists:reverse(List) ++ [H1|Tail], Acc1}
    end;
bt_mapfold(_, AccF, [], List, []) ->
    {lists:reverse(List), AccF};
bt_mapfold(_, _, [], _, [{Fun, Acc0, ListOri, ListDest}|BackTrackingList]) ->
    bt_mapfold(Fun, Acc0, ListOri, ListDest, BackTrackingList).
