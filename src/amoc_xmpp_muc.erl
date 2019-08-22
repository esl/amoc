-module(amoc_xmpp_muc).

%% API

-export([rooms_to_join/3,
         rooms_to_create/3,
         room_members/2]).

%% Debug API

-export([print_rooms_to_join/3,
         print_rooms_to_create/3]).

%% Room distribution by buckets

%% Returns the IDs of rooms joined by the specified user, intended for MUC
%% The order is important for equal distribution of created rooms
-spec rooms_to_join(amoc_scenario:user_id(), pos_integer(), pos_integer()) -> [pos_integer()].
rooms_to_join(UserId, RoomsPerUser, UsersPerRoom) ->
    BasicRoom = round_id((UserId - 1) * RoomsPerUser / UsersPerRoom, RoomsPerUser > UsersPerRoom),
    RoomBucketPos = BasicRoom rem RoomsPerUser,
    RoomBucketStartId = BasicRoom - RoomBucketPos + 1,
    lists:seq(BasicRoom + 1, RoomBucketStartId + RoomsPerUser - 1)
        ++ lists:seq(RoomBucketStartId, BasicRoom).

%% Returns the IDs of rooms created by the specified user, intended for MUC Light
-spec rooms_to_create(amoc_scenario:user_id(), pos_integer(), pos_integer()) -> [pos_integer()].
rooms_to_create(UserId, RoomsPerUser, UsersPerRoom) ->
    MyRooms = rooms_to_join(UserId, RoomsPerUser, UsersPerRoom),
    lists:filter(fun(R) -> creator(R, RoomsPerUser, UsersPerRoom) =:= UserId end, MyRooms).

%% Returns the list of member IDs for any room created by the specified user, intended for MUC Light
%% The list does not contain the creator ID
-spec room_members(amoc_scenario:user_id(), pos_integer()) -> [amoc_scenario:user_id()].
room_members(CreatorId, UsersPerRoom) ->
    Occupants = bucket_ids(CreatorId, UsersPerRoom),
    lists:delete(CreatorId, Occupants).

creator(RoomId, RoomsPerUser, UsersPerRoom) ->
    round_id((RoomId - 1) * UsersPerRoom / RoomsPerUser + 1, RoomsPerUser =< UsersPerRoom).

round_id(FloatId, RoundUp) when FloatId >= 0 ->
    IntId = trunc(FloatId),
    case FloatId > IntId andalso RoundUp of
        true -> IntId + 1;
        false -> IntId
    end.

bucket_ids(Id, BucketSize) ->
    Position = (Id - 1) rem BucketSize,
    BucketStartId = Id - Position,
    lists:seq(BucketStartId, BucketStartId + BucketSize - 1).

%% Debug - print user distribution in rooms

%% @doc Print a matrix specifying the relation between users and rooms
%%   Each row means one user, each column means one room.
%%   For each user: the first room to join is shown as 'x'
%%                  other rooms to join are shown as '.'
%%
%% Example: amoc_xmpp_muc:print_rooms_to_join(6, 3, 2).
%% x..         <-- User 1 joins rooms 1, 2, 3
%% ..x         <-- User 2 joins rooms 3, 1, 2
%%    x..      <-- User 3 joins rooms 4, 5, 6
%%    ..x          (...)
%%       x..
%%       ..x
-spec print_rooms_to_join(pos_integer(), pos_integer(), pos_integer()) -> ok.
print_rooms_to_join(UserCount, RoomsPerUser, UsersPerRoom) ->
    [print_my_rooms_to_join(UserId, RoomsPerUser, UsersPerRoom)
     || UserId <- lists:seq(1, UserCount)],
    ok.

print_my_rooms_to_join(UserId, RoomsPerUser, UsersPerRoom) ->
    RoomsToJoin = rooms_to_join(UserId, RoomsPerUser, UsersPerRoom),
    Tagged = lists:zip(RoomsToJoin, lists:seq(1, RoomsPerUser)),
    [{Lowest, _} | _] = Sorted = lists:sort(Tagged),
    io:put_chars(lists:duplicate(Lowest-1, $ )),
    io:put_chars([room_char(R) || {_, R} <- Sorted]),
    io:nl().

room_char(1) -> $x;
room_char(R) when R > 1 -> $..

%% @doc Print a matrix specifying the relation between users and rooms
%%   Each row means one user, each column means one room.
%%   For each room: the creator is shown as 'x'
%%                  other members are shown as '.'
%%
%% Example: amoc_xmpp_muc:print_rooms_to_create(6, 3, 2).
%% xx.       <-- User 1 creates rooms 1, 2
%% ..x       <-- User 2 creates room 3
%%    xx.    <-- User 3 creates rooms 4, 5
%%    ..x        (...)
%%       xx.
%%       ..x
-spec print_rooms_to_create(pos_integer(), pos_integer(), pos_integer()) -> ok.
print_rooms_to_create(UserCount, RoomsPerUser, UsersPerRoom) ->
    AllRoomUsers = all_room_users(UserCount, RoomsPerUser, UsersPerRoom),
    [print_my_rooms_to_create(UserId, AllRoomUsers) || UserId <- lists:seq(1, UserCount)],
    ok.

all_room_users(UserCount, RoomsPerUser, UsersPerRoom) ->
    [room_column(RoomId, RoomsPerUser, UsersPerRoom)
     || RoomId <- all_rooms(UserCount, RoomsPerUser, UsersPerRoom)].

all_rooms(UserCount, RoomsPerUser, UsersPerRoom) ->
    lists:flatmap(fun(UserId) -> rooms_to_create(UserId, RoomsPerUser, UsersPerRoom) end,
                  lists:seq(1, UserCount)).

room_column(RoomId, RoomsPerUser, UsersPerRoom) ->
    CreatorId = creator(RoomId, RoomsPerUser, UsersPerRoom),
    Members = room_members(CreatorId, UsersPerRoom),
    {CreatorId, Members}.

print_my_rooms_to_create(UserId, AllRoomUsers) ->
    io:put_chars([room_to_create_char(UserId, CreatorId, Members)
                  || {CreatorId, Members} <- AllRoomUsers]),
    io:nl().

room_to_create_char(UserId, UserId, _) -> $x;
room_to_create_char(UserId, _, Members) ->
    case lists:member(UserId, Members) of
        true -> $.;
        false -> ($ )
    end.
