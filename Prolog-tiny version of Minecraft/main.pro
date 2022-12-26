% fatma sena alci
% 2019400045
% compiling: yes
% complete: yes
:- ['cmpecraft.pro'].

:- init_from_map.

% 10 points
manhattan_distance(A, B, Distance) :- A = [H,T], B = [F,S], Distance is abs(S-T)+abs(F-H).
% 10 points
minimum_of_list([Minimum], Minimum).
minimum_of_list(List, Minimum) :- List = [H|T], minimum_of_list(T, M), Minimum is min(H, M).
% 10 points
%Collects all required objects, then calculates distances for each, calls minimum of list to find nearest object. After that,finds object key via value(distance).
bag_rec(State, Bag, St, IndDict, NewDict, _Dist) :-length(Bag,In),Inn is In-1, St == Inn, nth0(St, Bag, Kk),nth0(0, State, A), nth0(1, State, O), get_dict(Kk, O, Obj), manhattan_distance([A.x,A.y], [Obj.x, Obj.y], D), put_dict(Kk, IndDict, D, NewDict).
bag_rec(State, Bag, Index, IndDict, NewDict, Dist) :-  nth0(Index, Bag, K),nth0(0, State, A), nth0(1, State, O), get_dict(K, O, Obj), manhattan_distance([A.x,A.y], [Obj.x, Obj.y], D),Ind is Index+1, bag_rec(State, Bag, Ind, IndDict, NewwDict, Dist), put_dict(K, NewwDict, D, NewDict).
find_nearest_type(State, ObjectType, ObjKey, Object, Distance) :- nth0(1, State, O), findall(X, O.X.type = ObjectType, Bag), IndDict = [], bag_rec(State, Bag, 0, IndDict, NewDict, _Dist), dict_pairs(NewDict, _Tag, NewPairs), pairs_values(NewPairs, Values), minimum_of_list(Values, Min),get_dict(H, NewDict, Min), ObjKey = H, get_dict(ObjKey, O, Object), Distance = Min.
% 10 points
%Reachs required location by increasing or decreasing x,y values one by one. When reachs agent's location, writes all actions to the action list.
navigate_to(State, X, Y, ActionList, DepthLimit) :- nth0(0, State, A), XLim = A.x, YLim = A.y ,navigate_t(State, X, Y, XLim, YLim, ActionList, _ActList, DepthLimit).
navigate_t(_State, XLim,  YLim, XLim, YLim , ActList, ActionList, _DepthLimit) :- ActList = ActionList.
navigate_t(State, X, Y, XLim, YLim, ActList, ActionList, DepthLimit) :- (nth0(0, State, _A)),Xn is X-XLim, Yn is Y-YLim, manhattan_distance([Xn,Yn], [0,0], D), D=<DepthLimit,
((Xn == 0) -> (
		((Yn == 0)-> (
		ActList = ActionList
		);
		((Yn > 0) -> 
			(
			Ynn is Yn-1,
			Yt is Ynn+YLim,
			append(ActionList, [go_down], NewActList),
			navigate_t(State, X, Yt, XLim, YLim, ActList, NewActList, DepthLimit)
			);
			(Ynn is Yn+1,
			Yt is Ynn+YLim,
			append(ActionList, [go_up], NewActList),
			navigate_t(State, X, Yt, XLim, YLim, ActList, NewActList, DepthLimit)
			
			)
		)		
		)
		);
	((Xn > 0) -> 
		( 
		Xnn is Xn-1,
		Xt is Xnn+XLim,
		append(ActionList, [go_right], NewActList),
		navigate_t(State, Xt, Y, XLim, YLim, ActList, NewActList, DepthLimit)
		);
		(Xnn is Xn+1,
		Xt is Xnn+XLim,
		append(ActionList, [go_left], NewActList),
		navigate_t(State, Xt, Y, XLim, YLim, ActList, NewActList, DepthLimit)
		)
		
	)
).
% 10 points
chop_nearest_tree(State, ActionList) :- find_nearest_type(State, tree, _ObjKey, Object, Distance), navigate_to(State, Object.x, Object.y, ActList, Distance),append(ActList, [left_click_c, left_click_c, left_click_c, left_click_c], ActionList).
% 10 points
mine_nearest_stone(State, ActionList) :- find_nearest_type(State, stone, _ObjKey, Object, Distance), navigate_to(State, Object.x, Object.y, ActList, Distance),append(ActList, [left_click_c, left_click_c, left_click_c, left_click_c], ActionList).
mine_nearest_cobblestone(State, ActionList) :- find_nearest_type(State, cobblestone, _ObjKey, Object, Distance), navigate_to(State, Object.x, Object.y, ActList, Distance),append(ActList, [left_click_c, left_click_c, left_click_c, left_click_c], ActionList).

% 10 points
gather_nearest_food(State, ActionList) :- find_nearest_type(State, food, _ObjKey, Object, Distance), navigate_to(State, Object.x, Object.y, ActList, Distance),append(ActList, [left_click_c], ActionList). 
% 10 points
%First of all, checks required items are available on bag. If not, equalizes it to zero, otherwise gets value via get_dict. Then, checks enough item exist or not, if not calls mine or chop function.
makeLog(Agent, LogS, CobS, StickS) :- dict_pairs(Agent.inventory, _Tag, NewInv), pairs_keys(NewInv, Keys),
(member(log,Keys)-> ((get_dict(log, Agent, LogS)));
				(LogS = 0)),
(member(cobblestone,Keys)-> ((get_dict(cobblestone, Agent, CobS)));
				(CobS = 0)),
(member(stick,Keys)-> ((get_dict(stick, Agent, StickS)));
				(StickS = 0)).
collect_requirements(State, ItemType, ActionList) :- nth0(0, State, A), makeLog(A, LogS, CobS, StickS),
((ItemType == stick) -> ((LogS >= 2) -> (ActionList = []);
									(chop_nearest_tree(State, ActionList))
						);
						(((LogS >= 3) ->(LogList = [], NewState = State);
										((chop_nearest_tree(State, LogList)), execute_actions(State, LogList, NewState))),
						((StickS >= 2) ->(StList = [], NewwState = NewState);	
										((chop_nearest_tree(NewState, SttList)), execute_actions(NewState, SttList, NewwState),append(SttList,[craft_stick],StList))),
						((CobS >= 3) ->(CobList = []);	
										((mine_nearest_stone(NewwState, CobbList)) -> (CobList = CobbList);
										(mine_nearest_cobblestone(NewwState,CobList1), execute_actions(NewwState, CobList1, NewSt1),mine_nearest_cobblestone(NewSt1, CobList2), execute_actions(NewSt1, CobList2, NewSt2),mine_nearest_cobblestone(NewSt2, CobList)
										))),
						append([LogList, StList, CobList],ActionList)
						)
).

% 5 points
%Checks all tiles and neighbours if are free or not. My tile_check function works like tile_occupied function.
tile_check(State, X, Y) :-
    State = [_, StateDict, _],
    get_dict(_, StateDict, Object),
    get_dict(x, Object, Ox),
    get_dict(y, Object, Oy),
	X = Ox, Y = Oy.

starting(_State, X, Y) :- X = 2, Y = 2.
widHeit(_State, W, H, X, Y, Xson, Yson) :- Xm is X+2, ((W == Xm) -> 
													((Xson = 2),Ym is Y + 2,
														((H == Ym)->(false);
																		(Yson is Ym-1)));
												(Xson is X+1), (Yson = Y)).
checking(State, X, Y, XMin, YMin, XMax, YMax) :-  Xo is X-1, Xt is X+1, Yo is Y-1, Yt is Y+1,
					((tile_check(State, X, Y); tile_check(State, X, Yo); tile_check(State, X, Yt);
					  tile_check(State, Xo, Y); tile_check(State, Xo, Yo); tile_check(State, Xo, Yt);
					  tile_check(State, Xt, Y); tile_check(State, Xt, Yo); tile_check(State, Xt, Yt)  )-> (width(W), height(H), widHeit(State,W, H, X,Y,Xson,Yson),
																										   checking(State, Xson, Yson, XMin, YMin, XMax, YMax));
					(XMin = Xo, YMin = Yo, XMax = Xt, YMax = Yt)) .
find_castle_location(State, XMin, YMin, XMax, YMax) :- starting(State, X, Y), checking(State, X, Y, XMin, YMin, XMax, YMax).

% 15 points
%Check agent's inventory. Makes mining or not via inventory. Executes actions because agent went to mine stone. Finds castle location. After all, agent comes castle's base via navigate to function.
stoneMining(State,ActionList) :- nth0(0, State, A), makeLog(A, _LogS, CobS, _StickS), 
((CobS>=9) -> (ActionList = []);
				((CobS >= 6) -> (mine_nearest_stone(State,ActionList));
							((CobS >= 3) -> (mine_nearest_stone(State, ActList1), execute_actions(State, ActList1, State2), mine_nearest_stone(State2, ActList2), append(ActList1, ActList2, ActionList));
										(mine_nearest_stone(State, ActList1), execute_actions(State, ActList1, State2), mine_nearest_stone(State2, ActList2), execute_actions(State2, ActList2, State3), mine_nearest_stone(State3, ActList3), append([ActList1,ActList2,ActList3], ActionList))
							)
				)
).
cobMining(_State, 0, _ActionList).
cobMining(State, MissingCobs, ActionList) :- mine_nearest_cobblestone(State, ActionList), execute_actions(State, ActionList, NewState), MissC is MissingCobs-1, cobMining(NewState, MissC, ActionList).
make_castle(State, ActionList) :-nth0(0, State, A), makeLog(A, _LogS, CobS, _StickS),MissingCobs is 9-CobS, nth0(1, State, O), findall(X, O.X.type = stone, Bag), ((Bag == []) -> (cobMining(State, MissingCobs, ActList));
												  (stoneMining(State, ActList))),execute_actions(State, ActList, FinalState), nth0(0, FinalState, Agent),find_castle_location(FinalState, XMin, YMin, _XMax, _YMax), Xm is XMin+1, Ym is YMin+1,manhattan_distance([Agent.x,Agent.y],[Xm,Ym],D), navigate_to(FinalState, Xm, Ym, ActtList,D), append([ActList,ActtList,[place_c,place_e,place_n,place_w,place_s,place_ne,place_nw,place_sw,place_se]],ActionList) .
