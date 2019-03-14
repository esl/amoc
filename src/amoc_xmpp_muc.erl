-module(amoc_xmpp_muc).

%% API

-export([rooms_to_join/3,
         rooms_to_create/3,
         room_members/2]).

%% Debug API

-export([print_rooms_to_join/3,
         print_rooms_to_create/3]).

%% Room distribution by buckets

-spec rooms_to_join(amoc_scenario:user_id(), pos_integer(), pos_integer()) -> [pos_integer()].
rooms_to_join(UserId, RoomsPerUser, UsersPerRoom) ->
    BasicRoom = round_id((UserId - 1) * RoomsPerUser / UsersPerRoom, RoomsPerUser > UsersPerRoom),
    RoomBucketPos = BasicRoom rem RoomsPerUser,
    RoomBucketStartId = BasicRoom - RoomBucketPos + 1,
    lists:seq(BasicRoom + 1, RoomBucketStartId + RoomsPerUser - 1)
        ++ lists:seq(RoomBucketStartId, BasicRoom).

-spec rooms_to_create(amoc_scenario:user_id(), pos_integer(), pos_integer()) -> [pos_integer()].
rooms_to_create(UserId, RoomsPerUser, UsersPerRoom) ->
    MyRooms = rooms_to_join(UserId, RoomsPerUser, UsersPerRoom),
    lists:filter(fun(R) -> creator(R, RoomsPerUser, UsersPerRoom) =:= UserId end, MyRooms).

-spec room_members(amoc_scenario:user_id(), pos_integer()) -> [amoc_scenario:user_id()].
room_members(CreatorId, UsersPerRoom) ->
    Occupants = bucket_ids(CreatorId, UsersPerRoom),
    lists:delete(CreatorId, Occupants).

creator(RoomId, RoomsPerUser, UsersPerRoom) ->
    round_id((RoomId - 1) * UsersPerRoom / RoomsPerUser + 1, RoomsPerUser =< UsersPerRoom).

round_id(FloatId, true) -> round(math:ceil(FloatId));
round_id(FloatId, false) -> round(math:floor(FloatId)).

bucket_ids(Id, BucketSize) ->
    Position = (Id - 1) rem BucketSize,
    BucketStartId = Id - Position,
    lists:seq(BucketStartId, BucketStartId + BucketSize - 1).

%% Debug - print rooms

print_rooms_to_join(UserCount, RoomsPerUser, UsersPerRoom) ->
    [print_my_rooms_to_join(UserId, RoomsPerUser, UsersPerRoom)
     || UserId <- lists:seq(1, UserCount)].

print_my_rooms_to_join(UserId, RoomsPerUser, UsersPerRoom) ->
    RoomsToJoin = rooms_to_join(UserId, RoomsPerUser, UsersPerRoom),
    Tagged = lists:zip(RoomsToJoin, lists:seq(1, RoomsPerUser)),
    [{Lowest, _} | _] = Sorted = lists:sort(Tagged),
    io:put_chars(lists:duplicate(Lowest-1, $ )),
    io:put_chars([room_char(R) || {_, R} <- Sorted]),
    io:nl().

room_char(1) -> $x;
room_char(R) when R > 1 -> $..

all_rooms(UserCount, RoomsPerUser, UsersPerRoom) ->
    lists:flatmap(fun(UserId) -> rooms_to_create(UserId, RoomsPerUser, UsersPerRoom) end,
                  lists:seq(1, UserCount)).

all_room_users(UserCount, RoomsPerUser, UsersPerRoom) ->
    [room_column(RoomId, RoomsPerUser, UsersPerRoom)
     || RoomId <- all_rooms(UserCount, RoomsPerUser, UsersPerRoom)].

room_column(RoomId, RoomsPerUser, UsersPerRoom) ->
    Creator = creator(RoomId, RoomsPerUser, UsersPerRoom),
    Occupants = bucket_ids(Creator, UsersPerRoom),
    {Creator, Occupants}.

print_rooms_to_create(UserCount, RoomsPerUser, UsersPerRoom) ->
    AllRoomUsers = all_room_users(UserCount, RoomsPerUser, UsersPerRoom),
    [print_my_rooms_to_create(UserId, AllRoomUsers) || UserId <- lists:seq(1, UserCount)].

print_my_rooms_to_create(UserId, AllRoomUsers) ->
    io:put_chars([room_to_create_char(UserId, Creator, Occupants)
                  || {Creator, Occupants} <- AllRoomUsers]),
    io:nl().

room_to_create_char(UserId, UserId, _) -> $x;
room_to_create_char(UserId, _, Occ) ->
    case lists:member(UserId, Occ) of
        true -> $.;
        false -> ($ )
    end.
