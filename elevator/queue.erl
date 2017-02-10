
-module (queue).
-export( [init/0, add_to_queue/3, addToInnerQueue/2, mergeFromQueue/2, 
	getInnerQueue/2, getNextOrder/1, deleteFromQueue/2, isOrder/2]).

% makes a file type to bag, witch corresponds to map.
init() ->
	%Opens the file named FileName.
	dets:open_file(FileName, [{type, bag}]),
	&Returns a list of all objects with key "order" stored in FileName
	Existing_Orders = dets:lookup(FileName, order),

add_to_queue(OrderDir, OrderFloor, Allstates) ->


addToInnerQueue(OrderFloor, ElevatorID) ->

mergeFromQueue(ElevatorID, AllStates) ->

getInnerQueue(ElevatorID) ->

getNextOrder(ElevatorID) -> 

deleteFromQueue(ElevatorID, CurrentFloor) ->

isOrder(Floor, ButtonType) ->