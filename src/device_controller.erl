%% Author: gosia
%% Created: 2011-07-28
%% Description: TODO: Add description to device_controller
-module(device_controller).
-include("/usr/lib/yaws/include/yaws_api.hrl").

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([out/1]).

%%
%% API Functions
%%

out(Arg) ->
	Method = (Arg#arg.req)#http_request.method,
	Appmoddata= Arg#arg.appmoddata, 
	case {Method, Appmoddata} of
		{'GET', "new"} -> new();
		{'GET', undefined} -> index();
%% 		_ -> {ehtml,[yaws_api:f("~w ~w",[Method,Appmoddata])]};
		{'POST', "create"} -> create(Arg);
		_ -> {redirect_local, "/devices"}
	end.

index() ->
	Title = "Index devices",
	Content={ehtml, [
		{table, [], [
			{tr, [], [
				{td, [], ["Key"]},
				{td, [], ["Value"]}]}
			| index_rows(list_db:all())
		]}
	]},
	[yaws_test:header(Title), Content, yaws_test:footer()].

index_rows([{Key, Value}| List]) ->
	[{tr, [], [
		{td, [], [Key]},
		{td, [], [Value]}]}
	| index_rows(List)];
index_rows([]) ->
	[].

new() ->
	Title = "Add device",
	Form = {ehtml,[
				{form, [{method, post}, {action, "create"}], [
            		{p, [], "Key"},
            		{input, [{name, key}, {type, text}, {value, ""}, {size, "48"}]},
					{p, [], "Password"},
            		{input, [{name, value}, {type, text}, {value, ""}, {size, "48"}]},
					{input, [{type, submit}, {value, "Add"}]}
           		]}
			]},
	[yaws_test:header(Title), Form, yaws_test:footer()].

create(Arg) ->
    {{ok, Key},{ok, Value}}={yaws_api:postvar(Arg, "key"),yaws_api:postvar(Arg, "value")},
	list_db:write(Key, Value),
	{redirect_local, "/devices"}.	

%%
%% Local Functions
%%

