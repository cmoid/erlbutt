-module(rpc_behavior).

-callback handle_data(ReqNo :: integer(), Body :: binary(), Conn :: term()) -> NewNonce :: binary().
