%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Generic utility functions that are independent from the
%%% problem context
%%% @end
%%% Created : 10 Feb 2014 by Pablo Lamela
%%%-------------------------------------------------------------------

-module(utils).

-export([to_one_list/1, remount_list/2, bt_mapfold_list_of_lists/3,
	 bt_mapfold/3]).

%%% @doc
%%% Transforms a list of lists into a flattened list, it also returns
%%% a list with the sizes of the original sublists so that the list
%%% of lists can be restored. This function is designed to let you
%%% use functions that work with lists on lists of lists.
%%% The original list of lists can be restored by using {@link remount_list/2}
%%% on the result.
%%% @param ListOfLists the original list of lists. All the elements of the
%%% outer list must be lists.
%%% @return a flattened list and a list of sizes.
%%% @see remount_list/2
-spec to_one_list(ListOfLists :: [[Elem]]) -> {Sizes :: [integer()], List :: [Elem]} when
      Elem :: term().
to_one_list(ListOfLists) ->
    {lists:append(ListOfLists), lists:map(fun erlang:length/1, ListOfLists)}.

%%% @doc
%%% Transforms a list into a lists of lists by splitting
%%% it as specified by the list of sizes provided. Designed to be
%%% used in conjunction with {@link to_one_list/1}. 
%%% @param Sizes a list of the sizes that the sublists of the resulting
%%% list of lists must have. The sum of the sizes of the sizes must be
%%% equal to the length of the list.
%%% @param List the list with the elements to use.
%%% @return a list of lists with the elements of List.
%%% @see to_one_list/1
-spec remount_list(Sizes :: [integer()], List :: [Elem]) -> ListOfLists :: [[Elem]].
remount_list([], []) -> [];
remount_list([Num|RestN], List) ->
    {FirstPiece, SecondPiece} = lists:split(Num, List),
    [FirstPiece|remount_list(RestN, SecondPiece)].

%%% @doc
%%% Works like {@link bt_mapfold/3}, but expects a list of lists.
%%% @param Fun the function to fold
%%% @param Acc0 the initial value of the accumulator
%%% @param ListOfLists1 the target list of lists
%%% @return a tuple with the updated list of lists and the last accumulator
%%% @see bt_mapfold/3
-spec bt_mapfold_list_of_lists(Fun, Acc0 :: Acc, ListOfLists1 :: [[A]]) ->
				      {ListOfLists2 :: [[B]], Acc1 :: Acc} when
      Fun :: fun((A, AccIn :: Acc) -> {OpRes, B, AccOut :: Acc}),
      A :: term(),
      B :: term(),
      Acc :: term(),
      OpRes :: 'success' | 'failure' | 'finished'.
bt_mapfold_list_of_lists(Fun, Acc0, ListOfLists) ->
    {OneList, Sizes} = to_one_list(ListOfLists),
    {ResOneList, AccF} = bt_mapfold(Fun, Acc0, OneList),
    {remount_list(Sizes, ResOneList), AccF}.

%%% @doc
%%% Based on standard mapfold but implements backtracking
%%% and shortcutting. It is intended to simplify search/matching
%%% functions. The function provided must return
%%% an extra value, (OpRes), in addition to the standard
%%% values in mapfold (accumulator Acc and transformed element B)
%%% OpRes may be one of the following:
%%% <ul>
%%% <li><code>success</code> - There was a match</li>
%%% <li><code>failure</code> - There was not a match</li>
%%% <li><code>finished</code> - There was a match and there will not be more</li>
%%% </ul>
%%% In addition to the original mapfold behaviour, bt_mapfold
%%% will "backtrack" if the end of the list is found and
%%% the atom <code>finished</code> was not returned yet.
%%% The whole state is saved in a stack whenever the atom <code>success</code>
%%% is returned, and these states are retrieved from the stack when
%%% backtracking. After backtracking, one of the originally successful function
%%% application will be skipped under the assumption that maybe that success
%%% was producing some of the later failures.
%%% In addition, if at any moment the atom <code>finished</code> is returned,
%%% the execution will be terminated, and both the list and the accumulator
%%% will be returned as they are.
%%% @param Fun the function to fold
%%% @param Acc0 the initial value of the accumulator
%%% @param List1 the target list
%%% @return a tuple with the updated list and the last accumulator
-spec bt_mapfold(Fun, Acc0 :: Acc, List1 :: [A]) ->
			{List2 :: [B], Acc1 :: Acc} when
      Fun :: fun((A, AccIn :: Acc) -> {OpRes, B, AccOut :: Acc}),
      A :: term(),
      B :: term(),
      Acc :: term(),
      OpRes :: 'success' | 'failure' | 'finished'.
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
