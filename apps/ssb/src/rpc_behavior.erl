-module(rpc_behavior).

-callback exec_rpc(I :: term()) -> O :: term().
