:- module(launch, [
    start/0,
    start/1,
    start/2,
    help/0
]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(web_server).
:- use_module(database).
:- use_module(scheduling).

% Dynamic predicates
:- dynamic initialized/1.
:- dynamic server_handle/1.
:- dynamic optimization_iterations/1.

% Default port
default_port(8000).

% Load sample data for testing
load_sample_data :-
    % Only load if not already initialized
    (initialized(true) -> true ; (
        % Set flag to prevent multiple initializations
        assertz(initialized(true)),
        
        % Add teachers
        database:add_teacher(1, 'Alice', [1, 3, 5, 7, 9]),    % Math, Physics, CS, Biology, Chemistry
        database:add_teacher(2, 'Bob', [2, 4, 6]),            % English, History, Geography
        database:add_teacher(3, 'Clara', [5, 8, 10]),         % CS, Art, Music
        database:add_teacher(4, 'David', [1, 3, 7]),          % Math, Physics, Biology
        database:add_teacher(5, 'Eva', [2, 4, 6, 8]),         % English, History, Geography, Art
        
        % Add rooms
        database:add_room(101, 25, ['Whiteboard']),
        database:add_room(102, 15, ['Whiteboard', 'Laboratory']),
        database:add_room(103, 30, ['Whiteboard', 'Computers']),
        database:add_room(104, 40, ['Whiteboard']),
        database:add_room(105, 20, ['Whiteboard', 'Musical Instruments']),
        
        % Add groups
        database:add_group(201, [1, 2, 3, 5, 7], 20),        % Math, English, Physics, CS, Biology
        database:add_group(202, [1, 2, 4, 6, 8], 15),        % Math, English, History, Geography, Art
        database:add_group(203, [1, 3, 5, 7, 9], 25),        % Math, Physics, CS, Biology, Chemistry
        database:add_group(204, [2, 4, 6, 8, 10], 18),       % English, History, Geography, Art, Music
        
        % Add subjects
        database:add_subject(1, 'Mathematics', 5, ['Whiteboard'], 5),
        database:add_subject(2, 'English', 4, ['Whiteboard'], 5),
        database:add_subject(3, 'Physics', 3, ['Whiteboard', 'Laboratory'], 4),
        database:add_subject(4, 'History', 3, ['Whiteboard'], 3),
        database:add_subject(5, 'Computer Science', 4, ['Computers'], 4),
        database:add_subject(6, 'Geography', 2, ['Whiteboard'], 3),
        database:add_subject(7, 'Biology', 3, ['Whiteboard', 'Laboratory'], 4),
        database:add_subject(8, 'Art', 2, ['Whiteboard'], 2),
        database:add_subject(9, 'Chemistry', 3, ['Whiteboard', 'Laboratory'], 4),
        database:add_subject(10, 'Music', 2, ['Musical Instruments'], 2)
    )).

% Simple entry point - automatically select between user and test modes
% Also adds iterations parameter
start :-
    writeln('Choose mode:'),
    writeln('1. Start with empty database (for real use)'),
    writeln('2. Start with test data (for demonstration)'),
    read_line_to_string(user_input, Choice),
    (Choice = "1" -> 
        ask_iterations(Iterations),
        start(false, Iterations)
    ; Choice = "2" ->
        ask_iterations(Iterations),
        start(true, Iterations)
    ;
        writeln('Invalid choice. Please enter 1 or 2.'),
        start
    ).

% Helper to ask for iterations
ask_iterations(Iterations) :-
    format('Enter optimization iterations [1000]: '),
    read_line_to_string(user_input, Input),
    (Input = "" -> 
        Iterations = 1000
    ; 
        (atom_number(Input, Num), integer(Num), Num > 0) -> 
            Iterations = Num
        ;
            format('Invalid input. Please enter a positive integer.~n'),
            ask_iterations(Iterations)
    ).

% Start with specified mode (true = test data, false = empty database)
start(UseTestData) :-
    start(UseTestData, 10000).

% Start with specified mode and iterations
start(UseTestData, Iterations) :-
    % Stop any existing server
    stop_silently,
    
    % Get default port
    default_port(Port),
    
    % Store iterations for web_server to use
    retractall(optimization_iterations(_)),
    assertz(optimization_iterations(Iterations)),
    
    % Load test data if requested
    (UseTestData == true ->
        format('Starting with TEST DATA on port ~w (iterations: ~w)...~n', [Port, Iterations]),
        load_sample_data
    ;
        format('Starting with EMPTY DATABASE on port ~w (iterations: ~w)...~n', [Port, Iterations])
    ),
    
    % Start the HTTP server
    catch(
        % Use web_server's start_server/1 instead of direct http_server call
        web_server:start_server(Port),
        Error,
        (format('Error starting server: ~w~n', [Error]), fail)
    ),
    
    % Store the server handle for stopping
    assertz(server_handle(Port)),
    
    % Success message with URL & browser opening hint
    format('~n~`*t~80|~n'),
    format('Server started successfully!~n'),
    format('~n'),
    format('Access the application in your web browser at:~n'),
    format('http://localhost:~w/~n', [Port]),
    format('~`*t~80|~n'),
    !.  % Cut to prevent backtracking

% Silent version of stop
stop_silently :-
    (server_handle(Port) -> 
        catch(
            (http_current_server(ServerThread, Port),
             http_stop_server(ServerThread, [])),
            _,
            true
        ),
        retractall(server_handle(_))
    ; true).

% Help command - updated to include iterations info
help :-
    format('~n~`=t~80|~n'),
    format('~t GOGISCHEDULE SYSTEM - QUICK GUIDE ~t~80|~n'),
    format('~`=t~80|~n~n'),
    
    format('STARTING THE SERVER:~n'),
    format('  ?- start.                    % Start with interactive mode selection~n'),
    format('  ?- start(true).              % Start directly with test data~n'),
    format('  ?- start(false).             % Start directly with empty database~n'),
    format('  ?- start(true, 5000).        % Start with test data and 5000 optimization iterations~n'),
    format('  ?- start(false, 5000).       % Start with empty database and 5000 optimization iterations~n~n'),
    
    format('MODE DESCRIPTION:~n'),
    format('  - Empty database: For real use, you\'ll need to add all data manually~n'),
    format('  - Test data: Pre-loaded with 5 teachers, 5 rooms, 4 groups, and 10 subjects~n~n'),
    
    format('OPTIMIZATION ITERATIONS:~n'),
    format('  - Higher values (e.g., 10000) give better schedules but take longer~n'),
    format('  - Lower values (e.g., 1000) are faster but may produce suboptimal schedules~n'),
    format('  - Default is 1000 iterations if not specified~n~n'),
    
    format('~`=t~80|~n~n').

% Command-line entry point
:- initialization(start, main).