-module(detest_pmd).

-export([start_link/0,
         register_node/2,
         register_node/3,
         port_please/2,
         names/1]).

%% The supervisor module erl_distribution tries to add us as a child
%% process.  We don't need a child process, so return 'ignore'.
start_link() ->
    ignore.

register_node(_Name, _Port) ->
    %% This is where we would connect to epmd and tell it which port
    %% we're listening on, but since we're epmd-less, we don't do that.

    %% Need to return a "creation" number between 1 and 3.
    Creation = rand:uniform(3),
    {ok, Creation}.

%% As of Erlang/OTP 19.1, register_node/3 is used instead of
%% register_node/2, passing along the address family, 'inet_tcp' or
%% 'inet6_tcp'.  This makes no difference for our purposes.
register_node(Name, Port, _Family) ->
    register_node(Name, Port).

port_please(Name, _IP) ->
    % Port = epmdless:dist_port(Name),
    % %% The distribution protocol version number has been 5 ever since
    % %% Erlang/OTP R6.
    Version = 5,
    {port, node_to_port(Name), Version}.

names(_Hostname) ->
    %% Since we don't have epmd, we don't really know what other nodes
    %% there are.
    {error, address}.

node_to_port(Node) ->
    Nodes = butil:ds_val(nodes,etscfg),
    butil:ds_val(butil:toatom(Node),nodes_split(Nodes)).

nodes_split(Nodes) ->
    [begin
        {butil:toatom(butil:ds_val(name,Nd)),butil:ds_val(dist_port,Nd)}
    end || Nd <- Nodes].

