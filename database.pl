:- module(database, [
    add_teacher/3,
    add_room/3,
    add_group/3,
    add_subject/5,
    get_teachers/1,
    get_rooms/1,
    get_groups/1,
    get_subjects/1
    % Removed db_load_sample_data/0 from exports
]).

% Define dynamic predicates
:- dynamic teacher/3.      % teacher(ID, Name, Qualifications)
:- dynamic room/3.         % room(ID, Capacity, Equipment)
:- dynamic group/3.        % group(ID, Subjects, NumStudents)
:- dynamic subject/5.      % subject(ID, Name, Hours, Equipment, Priority)

% Add predicates with duplicate checking
add_teacher(ID, Name, Qualifications) :-
    \+ teacher(ID, _, _),
    assertz(teacher(ID, Name, Qualifications)).

add_room(ID, Capacity, Equipment) :-
    \+ room(ID, _, _),
    assertz(room(ID, Capacity, Equipment)).

add_group(ID, Subjects, NumStudents) :-
    \+ group(ID, _, _),
    assertz(group(ID, Subjects, NumStudents)).

add_subject(ID, Name, Hours, Equipment, Priority) :-
    \+ subject(ID, _, _, _, _),
    assertz(subject(ID, Name, Hours, Equipment, Priority)).

% Get predicates to retrieve all entries
get_teachers(Teachers) :-
    findall(teacher(ID, Name, Qualifications), teacher(ID, Name, Qualifications), Teachers).

get_rooms(Rooms) :-
    findall(room(ID, Capacity, Equipment), room(ID, Capacity, Equipment), Rooms).

get_groups(Groups) :-
    findall(group(ID, Subjects, NumStudents), group(ID, Subjects, NumStudents), Groups).

get_subjects(Subjects) :-
    findall(subject(ID, Name, Hours, Equipment, Priority), subject(ID, Name, Hours, Equipment, Priority), Subjects).