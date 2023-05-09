% INSTRUCTIONS
% =swipl echo-server.pl=
% =:- start_server.=
%
% Then navigate to http://localhost:3000 in your browser

:- encoding(utf8).
:- module(echo_server,
  [ start_server/0,
    stop_server/0
  ]
).


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_error)).
:- use_module(library(http/websocket)).
:- use_module(library(http/http_cors)).

:- set_setting(http:cors, [*]).

:- include('bot.pl').
:- include('gameBot.pl').

% http_handler docs: http://www.swi-prolog.org/pldoc/man?predicate=http_handler/3
% =http_handler(+Path, :Closure, +Options)=
%
% * root(.) indicates we're matching the root URL
% * We create a closure using =http_reply_from_files= to serve up files
%   in the local directory
% * The option =spawn= is used to spawn a thread to handle each new
%   request (not strictly necessary, but otherwise we can only handle one
%   client at a time since echo will block the thread)
:- http_handler(root(.),
                http_reply_from_files('../dist', []),
                [prefix]).
% * root(echo) indicates we're matching the echo path on the URL e.g.
%   localhost:3000/echo of the server
% * We create a closure using =http_upgrade_to_websocket=
% * The option =spawn= is used to spawn a thread to handle each new
%   request (not strictly necessary, but otherwise we can only handle one
%   client at a time since echo will block the thread)
:- http_handler(root(echo),
                http_upgrade_to_websocket(echo, []),
                [spawn([])]).
:- http_handler(root(bot),
                http_upgrade_to_websocket(bot, []),
                [spawn([])]).

:- http_handler(root(play),
                handle_play,
                []).
:- http_handler(root(init),
                handle_init,
                []).


start_server :-
    default_port(Port),
    start_server(Port).
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

stop_server() :-
    default_port(Port),
    stop_server(Port).
stop_server(Port) :-
    http_stop_server(Port, []).

default_port(3000).

%! echo(+WebSocket) is nondet.
% This predicate is used to read in a message via websockets and echo it
% back to the client
echo(WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),
    ( Message.opcode == close
    -> true
    ; get_response(Message.data, Response),
      write("Response: "), writeln(Response),
      ws_send(WebSocket, json(Response)),
      echo(WebSocket)
    ).

%! get_response(+Message, -Response) is det.
% Pull the message content out of the JSON converted to a prolog dict
% then add the current time, then pass it back up to be sent to the
% client
get_response(Message, Response) :-
  get_time(Time),
  Response = _{message:Message.message, time: Time}.

bot(WebSocket) :-
  ws_receive(WebSocket, Message, [format(json)]),
    ( Message.opcode == close
    -> true
    ; bot_response(Message.data.message, Response),
      nl, write("Response: "), writeln(Response),
      ws_send(WebSocket, json(Response)),
      bot(WebSocket)
).

%TODO: Need to fix response?
handle_play(Request) :-
      option(method(options), Request), !,
      cors_enable(Request,
                  [ methods([post])
                  ]),
      format('~n').

handle_play(Request) :-
  http_read_json_dict(Request, State),
  atom_string(Country,State.country),
  play([Country, State.playersPositions, State.countriesCards, State.cards], [NewCountry, NewPlayersPositions, NewCountriesCards, NewCards]),
  atom_string(NewCountry,NewCountry1),
  cors_enable,
  reply_json_dict(_{country:NewCountry1, playersPositions:NewPlayersPositions, countriesCards:NewCountriesCards, cards:NewCards}).

handle_init(Request) :-
  initGame([Country, PlayersPositions, CountriesCards, Cards ]),
  cors_enable,
  reply_json_dict(_{country:Country, playersPositions:PlayersPositions, countriesCards:CountriesCards, cards:Cards}).


:- start_server.