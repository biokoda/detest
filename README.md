Detest is a tool for running distributed erlang tests and it also works very well as a generic library/tool for running erlang VMs. 

It is designed to be simple and flexible. It sets up a distributed environment of multiple nodes, then it calls your code to do whatever you wish with that environment.

Detest is an escript and should be executed from the root folder of your project (like rebar).

For an example look at: test/test.erl and test/app.config

You can run all or some nodes with valgrind/gdb.

As a parameter it takes in path to your test script. This script needs to export at least: cfg/1, run/1, setup/1, cleanup/1.

Detest is configured with your cfg/1 function within the script. It instructs detest how many nodes to run, where their configuration files are, how to execute your app, etc.Detest will then execute the nodes, connect to each (as a hidden node) wait for your app to start, then call run/1 function in your script. You are then free to do whatever you wish. RPC is possible to any of the nodes. You can also terminate the test at any time by entering q to terminal.

Run it like so:
    
    ./detest test/test.erl

You can print all console output from your nodes:

    ./detest -v test/test.erl

You can also send arguments to your script:

    ./detest -v test/test.erl dotest1

Detest also supports executing nodes over ssh. So if you want to test network conditions, use LXC and configure detest to SSH to containers and execute nodes in them.

detest will create a folder named .detest at location it is running from. This is where node logs and state should go. 

Detest supports:

* embedded mode within an app
* executing nodes over SSH
* damocles library to create a seperate network interface for every node (SSH to LXC containers works better)
* executing some or all nodes with GDB/valgrind
* manually executing some nodes (if you want to debug one of them)
* django template files as configs