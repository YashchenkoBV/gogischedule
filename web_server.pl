:- module(web_server, [
    start_server/1
]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_files)).
:- use_module(library(lists)).  % For max_list
:- use_module(database).    % Import database module
:- use_module(scheduling).  % Import scheduling module

% Dynamic predicates
:- dynamic schedule_entry/7.  % schedule_entry(GroupID, SubjectID, TeacherID, RoomID, Day, StartTime, EndTime)
:- dynamic cell_subject/3.    % cell_subject(Day, TimeSlot, SubjectID)

% Define HTTP handlers
:- http_handler(root(.), home_page, []).
:- http_handler(root(input), input_page, []).
:- http_handler(root(process/teacher), process_teacher, []).
:- http_handler(root(process/room), process_room, []).
:- http_handler(root(process/group), process_group, []).
:- http_handler(root(process/subject), process_subject, []).
:- http_handler(root(display), display_data, []).
:- http_handler(root(schedule), handle_generate_schedule, []).
:- http_handler(root(view_schedule), view_schedule, []).
:- http_handler(root(teacher_schedule), view_teacher_schedule, []).
:- http_handler(root(group_schedule), view_group_schedule, []).
:- http_handler(root(room_schedule), view_room_schedule, []).
:- http_handler(root(export), export_schedule, []).
:- http_handler(root(static), serve_static, [prefix]).

% Serve static files (CSS)
serve_static(Request) :-
    http_reply_from_files('static', [], Request).

% Helper predicate to ensure we have an atom
ensure_atom(Value, Atom) :-
    (   var(Value) -> Atom = ''
    ;   Value == [] -> Atom = ''
    ;   atom(Value) -> Atom = Value
    ;   string(Value) -> atom_string(Atom, Value)
    ;   number(Value) -> atom_number(Atom, Value)
    ;   is_list(Value) -> atomic_list_concat(Value, ',', Atom)
    ;   Atom = ''  % Default safe fallback
    ).

% Auto-ID generation helper predicates
next_teacher_id(NextID) :-
    database:get_teachers(Teachers),
    (   Teachers = []
    ->  NextID = 1  % Start with 1 if no teachers exist
    ;   findall(ID, member(teacher(ID, _, _), Teachers), IDs),
        max_list(IDs, MaxID),
        NextID is MaxID + 1
    ).

next_room_id(NextID) :-
    database:get_rooms(Rooms),
    (   Rooms = []
    ->  NextID = 1  % Start with 1 if no rooms exist
    ;   findall(ID, member(room(ID, _, _), Rooms), IDs),
        max_list(IDs, MaxID),
        NextID is MaxID + 1
    ).

next_group_id(NextID) :-
    database:get_groups(Groups),
    (   Groups = []
    ->  NextID = 1  % Start with 1 if no groups exist
    ;   findall(ID, member(group(ID, _, _), Groups), IDs),
        max_list(IDs, MaxID),
        NextID is MaxID + 1
    ).

next_subject_id(NextID) :-
    database:get_subjects(Subjects),
    (   Subjects = []
    ->  NextID = 1  % Start with 1 if no subjects exist
    ;   findall(ID, member(subject(ID, _, _, _, _), Subjects), IDs),
        max_list(IDs, MaxID),
        NextID is MaxID + 1
    ).

% Start the server with error handling
start_server(Port) :-
    catch(
        http_server(http_dispatch, [port(Port)]),
        Error,
        (   format(atom(ErrorMsg), 'Failed to start server: ~w', [Error]),
            reply_html_page(
                [title('Server Error')],
                [h1('Server Error'),
                 p(ErrorMsg)]
            ))
    ).

% Navigation menu
navigation -->
    html(nav([a(href('/'), 'Home'),
              a(href('/input'), 'Add Data'),
              a(href('/display'), 'View Data'),
              a(href('/view_schedule'), 'View Schedule')])).

% Home page
home_page(_Request) :-
    reply_html_page(
        [title('Scheduling System'),
         link([rel('stylesheet'), href('/static/styles.css')])],
        [nav(\navigation),
         div(class('container'), [
             h1('Scheduling System'),
             p('Welcome to the scheduling system. Use the navigation above to add or view data.')
         ])
        ]).

% Input page
input_page(Request) :-
    (   member(method(post), Request), member(error(Error), Request)
    ->  error_message(Error, ErrorMsg),
        ErrorHTML = p(class('error'), ErrorMsg),
        http_parameters(Request,
            [ name_teacher(NameTeacher, [default('')]),
              quals_teacher(QualsTeacher, [default('')]),
              capacity_room(CapacityRoom, [default('')]),
              equip_room(EquipRoom, [default('')]),
              subjects_group(SubjectsGroup, [default('')]),
              numstudents_group(NumStudentsGroup, [default('')]),
              name_subject(NameSubject, [default('')]),
              hours_subject(HoursSubject, [default('')]),
              equip_subject(EquipSubject, [default('')]),
              priority_subject(PrioritySubject, [default('')])
            ], [form_data(post)])
    ;   ErrorHTML = '',
        NameTeacher = '', QualsTeacher = '',
        CapacityRoom = '', EquipRoom = '',
        SubjectsGroup = '', NumStudentsGroup = '',
        NameSubject = '', HoursSubject = '', EquipSubject = '', PrioritySubject = ''
    ),
    ensure_atom(NameTeacher, NameTeacherAtom),
    ensure_atom(QualsTeacher, QualsTeacherAtom),
    ensure_atom(CapacityRoom, CapacityRoomAtom),
    ensure_atom(EquipRoom, EquipRoomAtom),
    ensure_atom(SubjectsGroup, SubjectsGroupAtom),
    ensure_atom(NumStudentsGroup, NumStudentsGroupAtom),
    ensure_atom(NameSubject, NameSubjectAtom),
    ensure_atom(HoursSubject, HoursSubjectAtom),
    ensure_atom(EquipSubject, EquipSubjectAtom),
    ensure_atom(PrioritySubject, PrioritySubjectAtom),
    reply_html_page(
        [title('Add Data'),
         link([rel('stylesheet'), href('/static/styles.css')])],
        [nav(\navigation),
         div(class('container'), [
             h1('Add Data'),
             ErrorHTML,
             h2('Add Teacher'),
             form([action('/process/teacher'), method('post')],
                  [label(for('name_teacher'), 'Name: (required)'),
                   input([type('text'), id('name_teacher'), name('name_teacher'), value(NameTeacherAtom), required]),
                   label(for('quals_teacher'), 'Qualifications (comma-separated subject IDs): (required)'),
                   input([type('text'), id('quals_teacher'), name('quals_teacher'), value(QualsTeacherAtom), required]),
                   input([type('submit'), class([btn, 'btn-primary']), value('Add Teacher')])
                  ]),
             h2('Add Room'),
             form([action('/process/room'), method('post')],
                  [label(for('capacity_room'), 'Capacity: (required)'),
                   input([type('number'), id('capacity_room'), name('capacity_room'), value(CapacityRoomAtom), min('1'), required]),
                   label(for('equip_room'), 'Equipment (comma-separated):'),
                   input([type('text'), id('equip_room'), name('equip_room'), value(EquipRoomAtom)]),
                   input([type('submit'), class([btn, 'btn-primary']), value('Add Room')])
                  ]),
             h2('Add Group'),
             form([action('/process/group'), method('post')],
                  [label(for('subjects_group'), 'Subjects (comma-separated IDs): (required)'),
                   input([type('text'), id('subjects_group'), name('subjects_group'), value(SubjectsGroupAtom), required]),
                   label(for('numstudents_group'), 'Number of Students: (required)'),
                   input([type('number'), id('numstudents_group'), name('numstudents_group'), value(NumStudentsGroupAtom), min('1'), required]),
                   input([type('submit'), class([btn, 'btn-primary']), value('Add Group')])
                  ]),
             h2('Add Subject'),
             form([action('/process/subject'), method('post')],
                  [label(for('name_subject'), 'Name: (required)'),
                   input([type('text'), id('name_subject'), name('name_subject'), value(NameSubjectAtom), required]),
                   label(for('hours_subject'), 'Hours: (required)'),
                   input([type('number'), id('hours_subject'), name('hours_subject'), value(HoursSubjectAtom), min('1'), required]),
                   label(for('equip_subject'), 'Equipment (comma-separated):'),
                   input([type('text'), id('equip_subject'), name('equip_subject'), value(EquipSubjectAtom)]),
                   label(for('priority_subject'), 'Priority: (required, 1-5)'),
                   input([type('number'), id('priority_subject'), name('priority_subject'), value(PrioritySubjectAtom), min('1'), max('5'), required]),
                   input([type('submit'), class([btn, 'btn-primary']), value('Add Subject')])
                  ]),
             div(class('button-group'), [
                 a([href('/'), class([btn, 'btn-primary'])], 'Back to Home')
             ])
         ])
        ]).

% Process form data with error handling - Auto ID generation
process_teacher(Request) :-
    catch(
        (   http_parameters(Request,
                [ name_teacher(Name, [default('')]),
                  quals_teacher(Quals, [default('')])
                ]),
            % Enhanced validation
            (Name = '' -> throw(empty_field('Teacher name')) ; true),
            
            % Subject IDs validation
            (Quals = '' -> throw(empty_field('Qualifications')) ; true),
            
            % Process qualifications
            split_string(Quals, ",", " ", QualStrList),
            (QualStrList = [] -> throw(empty_field('Qualifications')) ; true),
            
            % Validate that all qualifications are valid integers
            catch(
                maplist(atom_number, QualStrList, QualList),
                _,
                throw(invalid_subject_ids(Quals))
            ),
            
            % Check if subjects exist
            findall(ID, database:subject(ID, _, _, _, _), SubjectIDs),
            forall(
                member(SubjectID, QualList),
                (member(SubjectID, SubjectIDs) -> true ; throw(missing_subject(SubjectID)))
            ),
            
            next_teacher_id(ID),
            database:add_teacher(ID, Name, QualList),
            reply_html_page(
                [title('Teacher Added'),
                 link([rel('stylesheet'), href('/static/styles.css')])],
                [nav(\navigation),
                 div(class('container'), [
                     h1('Teacher Added Successfully'),
                     p(class('success'), ['Teacher ', Name, ' with ID ', ID, ' added.']),
                     div(class('button-group'), [
                         a([href('/display'), class([btn, 'btn-primary'])], 'View Data'),
                         a([href('/input'), class([btn, 'btn-primary'])], 'Add More Data'),
                         a([href('/'), class([btn, 'btn-primary'])], 'Back to Home')
                     ])
                 ])
                ])
        ),
        Error,
        redirect_with_error(Request, '/input', Error)
    ).

process_room(Request) :-
    catch(
        (   http_parameters(Request,
                [ capacity_room(CapacityStr, []),
                  equip_room(EquipStr, [default('')])
                ]),
            % Enhanced validation for capacity
            (CapacityStr = '' -> throw(empty_field('Room capacity')) ; true),
            (atom_number(CapacityStr, Capacity) -> true ; throw(invalid_integer('Room capacity', CapacityStr))),
            (Capacity > 0 -> true ; throw(invalid_capacity(CapacityStr))),
            
            % Process equipment
            split_string(EquipStr, ",", " ", EquipList),
            
            next_room_id(ID),
            database:add_room(ID, Capacity, EquipList),
            reply_html_page(
                [title('Room Added'),
                 link([rel('stylesheet'), href('/static/styles.css')])],
                [nav(\navigation),
                 div(class('container'), [
                     h1('Room Added Successfully'),
                     p(class('success'), ['Room ', ID, ' with capacity ', Capacity, ' added.']),
                     div(class('button-group'), [
                         a([href('/display'), class([btn, 'btn-primary'])], 'View Data'),
                         a([href('/input'), class([btn, 'btn-primary'])], 'Add More Data'),
                         a([href('/'), class([btn, 'btn-primary'])], 'Back to Home')
                     ])
                 ])
                ])
        ),
        Error,
        redirect_with_error(Request, '/input', Error)
    ).

process_group(Request) :-
    catch(
        (   http_parameters(Request,
                [ subjects_group(SubjectsStr, [default('')]),
                  numstudents_group(NumStudentsStr, [default('')])
                ]),
            % Enhanced validation
            (SubjectsStr = '' -> throw(empty_field('Group subjects')) ; true),
            (NumStudentsStr = '' -> throw(empty_field('Number of students')) ; true),
            
            % Process subjects
            split_string(SubjectsStr, ",", " ", SubjStrList),
            (SubjStrList = [] -> throw(empty_field('Group subjects')) ; true),
            
            % Validate subject IDs
            catch(
                maplist(atom_number, SubjStrList, SubjectsList),
                _,
                throw(invalid_subject_ids(SubjectsStr))
            ),
            
            % Check if subjects exist
            findall(ID, database:subject(ID, _, _, _, _), SubjectIDs),
            forall(
                member(SubjectID, SubjectsList),
                (member(SubjectID, SubjectIDs) -> true ; throw(missing_subject(SubjectID)))
            ),
            
            % Validate number of students
            (atom_number(NumStudentsStr, NumStudents) -> true ; throw(invalid_integer('Number of students', NumStudentsStr))),
            (NumStudents > 0 -> true ; throw(invalid_number_students(NumStudentsStr))),
            
            next_group_id(ID),
            database:add_group(ID, SubjectsList, NumStudents),
            reply_html_page(
                [title('Group Added'),
                 link([rel('stylesheet'), href('/static/styles.css')])],
                [nav(\navigation),
                 div(class('container'), [
                     h1('Group Added Successfully'),
                     p(class('success'), ['Group ', ID, ' with ', NumStudents, ' students added.']),
                     div(class('button-group'), [
                         a([href('/display'), class([btn, 'btn-primary'])], 'View Data'),
                         a([href('/input'), class([btn, 'btn-primary'])], 'Add More Data'),
                         a([href('/'), class([btn, 'btn-primary'])], 'Back to Home')
                     ])
                 ])
                ])
        ),
        Error,
        redirect_with_error(Request, '/input', Error)
    ).

process_subject(Request) :-
    catch(
        (   http_parameters(Request,
                [ name_subject(Name, [default('')]),
                  hours_subject(HoursStr, [default('')]),
                  equip_subject(EquipStr, [default('')]),
                  priority_subject(PriorityStr, [default('')])
                ]),
            % Enhanced validation
            (Name = '' -> throw(empty_field('Subject name')) ; true),
            (HoursStr = '' -> throw(empty_field('Subject hours')) ; true),
            (PriorityStr = '' -> throw(empty_field('Subject priority')) ; true),
            
            % Validate hours
            (atom_number(HoursStr, Hours) -> true ; throw(invalid_integer('Subject hours', HoursStr))),
            (Hours > 0 -> true ; throw(invalid_hours(HoursStr))),
            
            % Process equipment
            split_string(EquipStr, ",", " ", EquipList),
            
            % Validate priority
            (atom_number(PriorityStr, Priority) -> true ; throw(invalid_integer('Subject priority', PriorityStr))),
            (Priority > 0, Priority =< 5 -> true ; throw(invalid_priority(PriorityStr))),
            
            next_subject_id(ID),
            database:add_subject(ID, Name, Hours, EquipList, Priority),
            reply_html_page(
                [title('Subject Added'),
                 link([rel('stylesheet'), href('/static/styles.css')])],
                [nav(\navigation),
                 div(class('container'), [
                     h1('Subject Added Successfully'),
                     p(class('success'), ['Subject ', Name, ' with ID ', ID, ' added.']),
                     div(class('button-group'), [
                         a([href('/display'), class([btn, 'btn-primary'])], 'View Data'),
                         a([href('/input'), class([btn, 'btn-primary'])], 'Add More Data'),
                         a([href('/'), class([btn, 'btn-primary'])], 'Back to Home')
                     ])
                 ])
                ])
        ),
        Error,
        redirect_with_error(Request, '/input', Error)
    ).

display_data(_Request) :-
    database:get_teachers(Teachers),
    (   Teachers = []
    ->  TeacherHTML = p('No teachers added yet.')
    ;   TeacherHTML = table([tr([th('ID'), th('Name'), th('Qualifications')]),
                                \teacher_table(Teachers)])
    ),
    database:get_rooms(Rooms),
    (   Rooms = []
    ->  RoomHTML = p('No rooms added yet.')
    ;   RoomHTML = table([tr([th('ID'), th('Capacity'), th('Equipment')]),
                            \room_table(Rooms)])
    ),
    database:get_groups(Groups),
    (   Groups = []
    ->  GroupHTML = p('No groups added yet.')
    ;   GroupHTML = table([tr([th('ID'), th('Subjects'), th('Number of Students')]),
                            \group_table(Groups)])
    ),
    database:get_subjects(Subjects),
    (   Subjects = []
    ->  SubjectHTML = p('No subjects added yet.')
    ;   SubjectHTML = table([tr([th('ID'), th('Name'), th('Hours'), th('Equipment'), th('Priority')]),
                                \subject_table(Subjects)])
    ),
    reply_html_page(
        [title('Schedule Data'),
            link([rel('stylesheet'), href('/static/styles.css')])],
        [nav(\navigation),
            div(class('container'), [
                h1('Schedule Data')
            ]),
            
            % Only show Generate Schedule button
            div(style('margin-left: 40px; margin-bottom: 20px;'), [
                a([href('/schedule'), class([btn, 'btn-primary']), style('margin-right: 10px;')], 'SCHEDULIZE!')
            ]),
            
            div(class('container'), [
                div(class('schedule-section'), [
                    h2('Teachers'),
                    TeacherHTML
                ]),
                
                div(class('schedule-section'), [
                    h2('Rooms'),
                    RoomHTML
                ]),
                
                div(class('schedule-section'), [
                    h2('Groups'),
                    GroupHTML
                ]),
                
                div(class('schedule-section'), [
                    h2('Subjects'),
                    SubjectHTML
                ]),
                
                div(class('button-group'), [
                    a([href('/'), class([btn, 'btn-primary'])], 'Back to Home')
                ])
            ])
        ]).

% Schedule generation handler with iterations from launch.pl
handle_generate_schedule(_Request) :-
    scheduling:set_debug(off),  % Disable debug output for web server
    
    % Get iterations set from launch.pl
    (current_predicate(launch:optimization_iterations/1),
     launch:optimization_iterations(Iterations) -> true ; Iterations = 10000),
    
    % Verify we have sufficient data
    verify_scheduling_prerequisites(Result, ErrorMsg),
    
    (Result = missing_data ->
        % Return the specific error about missing data
        reply_html_page(
            [title('Insufficient Data'),
             link([rel('stylesheet'), href('/static/styles.css')])],
            [nav(\navigation),
             div(class('container'), [
                 h1('Insufficient Data for Scheduling'),
                 p(class('error'), ErrorMsg),
                 p('Please add the required data and try again.'),
                 a([href('/display'), class([btn, 'btn-primary'])], 'Back to Data')
             ])
            ])
    ;
        % Try to generate schedule with iterations from launch.pl
        (   catch(
                scheduling:optimize_schedule(Schedule, Iterations),
                Error,
                (   format(atom(ErrorMsg), 'Scheduling failed: ~w', [Error]),
                    reply_html_page(
                        [title('Scheduling Error'),
                         link([rel('stylesheet'), href('/static/styles.css')])],
                        [nav(\navigation),
                         div(class('container'), [h1('Scheduling Error'),
                                                  p(class('error'), ErrorMsg),
                                                  a(href('/display'), 'Back to Data')])
                        ])
                )
            )
        ->  (   Schedule = []
            ->  reply_html_page(
                    [title('No Schedule'),
                     link([rel('stylesheet'), href('/static/styles.css')])],
                    [nav(\navigation),
                     div(class('container'), [h1('No Schedule Generated'),
                                              p('The scheduling algorithm did not produce any schedule. Check your data and constraints.'),
                                              a(href('/display'), 'Back to Data')])
                    ])
            ;   % Store schedule for matrix view
                retractall(schedule_entry(_, _, _, _, _, _, _)),
                process_schedule(Schedule),
                
                % Display schedule with original format
                reply_html_page(
                    [title('Generated Schedule'),
                     link([rel('stylesheet'), href('/static/styles.css')])],
                    [nav(\navigation),
                     div(class('container'), [h1('Generated Schedule'),
                                              div(class('schedule-container'), [
                                                  div(class('main-schedule'), [\main_schedule_table(Schedule)]),
                                                  div(class('side-table'), [h2('By Teachers'), \teacher_schedule_table(Schedule)]),
                                                  div(class('side-table'), [h2('By Groups'), \group_schedule_table(Schedule)]),
                                                  div(class('side-table'), [h2('By Rooms'), \room_schedule_table(Schedule)])
                                              ]),
                                              div(class('button-group'), [
                                                  a([href('/view_schedule'), class([btn, 'btn-primary'])], 'View as Matrix'),
                                                  a([href('/display'), class([btn, 'btn-primary'])], 'Back to Data')
                                              ])
                                             ])
                    ])
            )
        ;   reply_html_page(
                [title('Scheduling Failed'),
                 link([rel('stylesheet'), href('/static/styles.css')])],
                [nav(\navigation),
                 div(class('container'), [h1('Scheduling Failed'),
                                          p('An error occurred while generating the schedule.'),
                                          a(href('/display'), 'Back to Data')])
                ])
        )
    ).

% Check if we have sufficient data for scheduling
verify_scheduling_prerequisites(Result, ErrorMsg) :-
    % Check if we have teachers
    database:get_teachers(Teachers),
    (Teachers = [] -> 
        Result = missing_data, 
        ErrorMsg = 'No teachers have been added. At least one teacher is required.', 
        !
    ; true),
    
    % Check if we have rooms
    database:get_rooms(Rooms),
    (Rooms = [] -> 
        Result = missing_data, 
        ErrorMsg = 'No rooms have been added. At least one room is required.', 
        !
    ; true),
    
    % Check if we have groups
    database:get_groups(Groups),
    (Groups = [] -> 
        Result = missing_data, 
        ErrorMsg = 'No groups have been added. At least one group is required.', 
        !
    ; true),
    
    % Check if we have subjects
    database:get_subjects(Subjects),
    (Subjects = [] -> 
        Result = missing_data, 
        ErrorMsg = 'No subjects have been added. At least one subject is required.', 
        !
    ; true),
    
    % Check subject-teacher compatibility
    findall(SubjID, database:subject(SubjID, _, _, _, _), AllSubjIDs),
    findall(Teacher-Qualifs, (database:teacher(Teacher, _, Qualifs), Qualifs \= []), TeacherQualifs),
    
    % Check if any subjects are not covered by teacher qualifications
    findall(SubjID, (
        member(SubjID, AllSubjIDs),
        \+ (member(_-Qualifs, TeacherQualifs), member(SubjID, Qualifs))
    ), UncoveredSubjects),
    
    (UncoveredSubjects \= [] ->
        findall(Name, (
            member(SubjID, UncoveredSubjects),
            database:subject(SubjID, Name, _, _, _)
        ), UncoveredNames),
        atomic_list_concat(UncoveredNames, ', ', UncoveredStr),
        Result = missing_data,
        format(atom(ErrorMsg), 'Some subjects have no qualified teachers: ~w', [UncoveredStr]),
        !
    ; true),
    
    % Check that all groups have subjects assigned that exist
    findall(GroupID-SubjID, (
        database:group(GroupID, Subjects, _),
        member(SubjID, Subjects),
        \+ member(SubjID, AllSubjIDs)
    ), GroupsWithUnknownSubjects),
    
    (GroupsWithUnknownSubjects \= [] ->
        Result = missing_data,
        format(atom(ErrorMsg), 'Some groups reference non-existent subject IDs: ~w', [GroupsWithUnknownSubjects]),
        !
    ; true),
    
    % Check that groups have assigned subjects
    findall(GroupID, (
        database:group(GroupID, Subjects, _),
        Subjects = []
    ), EmptyGroups),
    
    (EmptyGroups \= [] ->
        Result = missing_data,
        format(atom(ErrorMsg), 'Some groups have no subjects assigned: ~w', [EmptyGroups]),
        !
    ; true),
    
    % Check room capacity for groups
    findall(group(GroupID, NumStudents), (
        database:group(GroupID, _, NumStudents),
        \+ (database:room(_, Capacity, _), NumStudents =< Capacity)
    ), GroupsWithoutRooms),
    
    (GroupsWithoutRooms \= [] ->
        Result = missing_data,
        format(atom(ErrorMsg), 'Some groups are too large for any available room: ~w', [GroupsWithoutRooms]),
        !
    ; true),
    
    % All tests passed
    Result = ok,
    ErrorMsg = 'Data is sufficient for scheduling'.

process_schedule(Schedule) :-
    % Get days and times information
    scheduling:days(DaysAtoms),
    scheduling:times(_),  % Changed TimesList to underscore since it's not used
    
    % Process each schedule entry
    forall(
        member([_, DayAtom, TimeAtom, TeacherID, SubjectID, RoomID, GroupID], Schedule),
        (   % Find day number from atom
            nth0(DayNum0, DaysAtoms, DayAtom),
            DayNum is DayNum0 + 1,  % Convert 0-based to 1-based for visualizer
            
            % Find start time in minutes
            (   atom_concat(HourStr, ':00', TimeAtom)
            ->  atom_number(HourStr, Hour),
                StartTime is Hour * 60
            ;   StartTime = 480  % Default to 8:00 if format is unexpected
            ),
            
            % End time is 1 hour later
            EndTime is StartTime + 60,
            
            % Assert the entry
            assertz(schedule_entry(GroupID, SubjectID, TeacherID, RoomID, DayNum, StartTime, EndTime))
        )
    ).

% View schedule as matrix
view_schedule(_Request) :-
    collect_schedule_entries(Schedule),
    reply_html_page(
        [title('Schedule Matrix View'),
         link([rel('stylesheet'), href('/static/styles.css')])],
        [nav(\navigation),
         div(class('container'), [
             h1('Schedule Matrix View'),
             
             h2('View By:'),
             div(class('filter-section'), [
                 \teacher_buttons,
                 \group_buttons,
                 \room_buttons
             ]),
             
             div(class('schedule-section'), [
                 \matrix_schedule_table(Schedule, 'Full Schedule')
             ]),
             
             div(class('button-group'), [
                a([href('/schedule'), class([btn, 'btn-primary'])], 'View as Tables'),
                a([href('/export'), class([btn, 'btn-primary']), download('full_schedule.html')], 'Save Schedule'),
                a([href('/display'), class([btn, 'btn-primary'])], 'Back to Data')
            ])
            
         ])
        ]).

% View teacher schedule
view_teacher_schedule(Request) :-
    http_parameters(Request, [id(TeacherID, [integer])]),
    database:teacher(TeacherID, TeacherName, _),
    collect_teacher_schedule(TeacherID, Schedule),
    reply_html_page(
        [title(['Schedule for ', TeacherName]),
         link([rel('stylesheet'), href('/static/styles.css')])],
        [nav(\navigation),
         div(class('container'), [
             h1(['Schedule for Teacher: ', TeacherName]),
             
             h2('View By:'),
             div(class('filter-section'), [
                 \teacher_buttons,
                 \group_buttons,
                 \room_buttons
             ]),
             
             div(class('schedule-section'), [
                 \matrix_schedule_table(Schedule, ['Teacher: ', TeacherName])
             ]),
             
             div(class('button-group'), [
                a([href('/view_schedule'), class([btn, 'btn-primary'])], 'Back to Full Schedule'),
                a([href('/export?teacher_id=' + TeacherID), class([btn, 'btn-primary']), 
                   download('teacher_schedule.html')], 'Save Schedule'),
                a([href('/display'), class([btn, 'btn-primary'])], 'Back to Data')
            ])
         ])
        ]).

% View group schedule
view_group_schedule(Request) :-
    http_parameters(Request, [id(GroupID, [integer])]),
    collect_group_schedule(GroupID, Schedule),
    reply_html_page(
        [title(['Schedule for Group ', GroupID]),
         link([rel('stylesheet'), href('/static/styles.css')])],
        [nav(\navigation),
         div(class('container'), [
             h1(['Schedule for Group: ', GroupID]),
             
             h2('View By:'),
             div(class('filter-section'), [
                 \teacher_buttons,
                 \group_buttons,
                 \room_buttons
             ]),
             
             div(class('schedule-section'), [
                 \matrix_schedule_table(Schedule, ['Group: ', GroupID])
             ]),
             
             div(class('button-group'), [
                a([href('/view_schedule'), class([btn, 'btn-primary'])], 'Back to Full Schedule'),
                a([href('/export?group_id=' + GroupID), class([btn, 'btn-primary']), 
                   download('group_schedule.html')], 'Save Schedule'),
                a([href('/display'), class([btn, 'btn-primary'])], 'Back to Data')
            ])
         ])
        ]).

% View room schedule
view_room_schedule(Request) :-
    http_parameters(Request, [id(RoomID, [integer])]),
    collect_room_schedule(RoomID, Schedule),
    reply_html_page(
        [title(['Schedule for Room ', RoomID]),
         link([rel('stylesheet'), href('/static/styles.css')])],
        [nav(\navigation),
         div(class('container'), [
             h1(['Schedule for Room: ', RoomID]),
             
             h2('View By:'),
             div(class('filter-section'), [
                 \teacher_buttons,
                 \group_buttons,
                 \room_buttons
             ]),
             
             div(class('schedule-section'), [
                 \matrix_schedule_table(Schedule, ['Room: ', RoomID])
             ]),
             
             div(class('button-group'), [
                a([href('/view_schedule'), class([btn, 'btn-primary'])], 'Back to Full Schedule'),
                a([href('/export?room_id=' + RoomID), class([btn, 'btn-primary']), 
                   download('room_schedule.html')], 'Save Schedule'),
                a([href('/display'), class([btn, 'btn-primary'])], 'Back to Data')
            ])
         ])
        ]).

% Export schedule to HTML file
export_schedule(Request) :-
    http_parameters(Request, 
        [ teacher_id(TeacherID, [integer, default(-1)]),
          group_id(GroupID, [integer, default(-1)]),
          room_id(RoomID, [integer, default(-1)])
        ]),
    (   TeacherID > 0
    ->  collect_teacher_schedule(TeacherID, Schedule),
        database:teacher(TeacherID, TeacherName, _),
        format(atom(FileName), 'teacher_~w_schedule.html', [TeacherID]),
        format(atom(Title), 'Schedule for Teacher: ~w', [TeacherName])
    ;   GroupID > 0
    ->  collect_group_schedule(GroupID, Schedule),
        format(atom(FileName), 'group_~w_schedule.html', [GroupID]),
        format(atom(Title), 'Schedule for Group: ~w', [GroupID])
    ;   RoomID > 0
    ->  collect_room_schedule(RoomID, Schedule),
        format(atom(FileName), 'room_~w_schedule.html', [RoomID]),
        format(atom(Title), 'Schedule for Room: ~w', [RoomID])
    ;   collect_schedule_entries(Schedule),
        FileName = 'full_schedule.html',
        Title = 'Full Schedule'
    ),
    export_schedule_as_html(Schedule, Title, HTMLContent),
    format('Content-type: text/html~n'),
    format('Content-disposition: attachment; filename="~w"~n~n', [FileName]),
    format('~w', [HTMLContent]).

% Collect all schedule entries
collect_schedule_entries(Schedule) :-
    findall(schedule(GroupID, SubjectID, TeacherID, RoomID, Day, Start, End),
            schedule_entry(GroupID, SubjectID, TeacherID, RoomID, Day, Start, End),
            Schedule).

% Collect schedule entries for a specific teacher
collect_teacher_schedule(TeacherID, Schedule) :-
    findall(schedule(GroupID, SubjectID, TeacherID, RoomID, Day, Start, End),
            schedule_entry(GroupID, SubjectID, TeacherID, RoomID, Day, Start, End),
            Schedule).

% Collect schedule entries for a specific group
collect_group_schedule(GroupID, Schedule) :-
    findall(schedule(GroupID, SubjectID, TeacherID, RoomID, Day, Start, End),
            schedule_entry(GroupID, SubjectID, TeacherID, RoomID, Day, Start, End),
            Schedule).

% Collect schedule entries for a specific room
collect_room_schedule(RoomID, Schedule) :-
    findall(schedule(GroupID, SubjectID, TeacherID, RoomID, Day, Start, End),
            schedule_entry(GroupID, SubjectID, TeacherID, RoomID, Day, Start, End),
            Schedule).

% Convert minutes to time string (e.g., 480 -> "08:00")
minutes_to_time(Minutes, TimeStr) :-
    Hours is Minutes // 60,
    Mins is Minutes mod 60,
    format(atom(TimeStr), '~|~`0t~d~2+:~|~`0t~d~2+', [Hours, Mins]).

% Convert day number to name
day_name(1, 'Monday').
day_name(2, 'Tuesday').
day_name(3, 'Wednesday').
day_name(4, 'Thursday').
day_name(5, 'Friday').

% Original schedule tables
main_schedule_table([]) --> 
    html(p('No schedule generated yet. Add more data and try again.')).
main_schedule_table(Schedule) -->
    html(table([tr([th('Day'), th('Time'), th('Teacher'), th('Subject'), th('Room'), th('Group')]),
                \main_schedule_rows(Schedule)])).

main_schedule_rows([]) --> [].
main_schedule_rows([[_, Day, Time, TID, SID, RID, GID]|Rest]) -->
    { database:teacher(TID, TName, _),
      database:subject(SID, SName, _, _, _),
      database:room(RID, _, _),
      database:group(GID, _, _) },
    html(tr([td(Day), td(Time), td(TName), td(SName), td(RID), td(GID)])),
    main_schedule_rows(Rest).

teacher_schedule_table(Schedule) -->
    html(table([tr([th('Teacher'), th('Day'), th('Time'), th('Subject'), th('Room'), th('Group')]),
                \teacher_schedule_rows(Schedule)])).

teacher_schedule_rows([]) --> [].
teacher_schedule_rows([[_, Day, Time, TID, SID, RID, GID]|Rest]) -->
    { database:teacher(TID, TName, _),
      database:subject(SID, SName, _, _, _),
      database:room(RID, _, _),
      database:group(GID, _, _) },
    html(tr([td(TName), td(Day), td(Time), td(SName), td(RID), td(GID)])),
    teacher_schedule_rows(Rest).

group_schedule_table(Schedule) -->
    html(table([tr([th('Group'), th('Day'), th('Time'), th('Teacher'), th('Subject'), th('Room')]),
                \group_schedule_rows(Schedule)])).

group_schedule_rows([]) --> [].
group_schedule_rows([[_, Day, Time, TID, SID, RID, GID]|Rest]) -->
    { database:teacher(TID, TName, _),
      database:subject(SID, SName, _, _, _),
      database:room(RID, _, _),
      database:group(GID, _, _) },
    html(tr([td(GID), td(Day), td(Time), td(TName), td(SName), td(RID)])),
    group_schedule_rows(Rest).

room_schedule_table(Schedule) -->
    html(table([tr([th('Room'), th('Day'), th('Time'), th('Teacher'), th('Subject'), th('Group')]),
                \room_schedule_rows(Schedule)])).

room_schedule_rows([]) --> [].
room_schedule_rows([[_, Day, Time, TID, SID, RID, GID]|Rest]) -->
    { database:teacher(TID, TName, _),
      database:subject(SID, SName, _, _, _),
      database:room(RID, _, _),
      database:group(GID, _, _) },
    html(tr([td(RID), td(Day), td(Time), td(TName), td(SName), td(GID)])),
    room_schedule_rows(Rest).

% Matrix schedule table (days as columns, time slots as rows)
matrix_schedule_table(Schedule, Title) -->
    {
      Days = [1, 2, 3, 4, 5],
      % Get time slots from scheduling module
      scheduling:times(TimeList),
      
      % Convert time atoms to minutes for sorting and processing
      findall(Minutes, 
              (member(TimeAtom, TimeList),
               atom_concat(HourStr, ':00', TimeAtom),
               atom_number(HourStr, Hour),
               Minutes is Hour * 60),
              TimeSlots),
      
      % Clear any previous cell subject assignments for coloring
      retractall(cell_subject(_, _, _))
    },
    html(div(class('schedule-container'), [
        h2(Title),
        table(class('schedule-table'), [
            thead(
                tr([th('Time') | \day_headers(Days)])
            ),
            tbody(
                \time_slot_rows(TimeSlots, Days, Schedule)
            )
        ])
    ])).

% Generate day headers
day_headers([]) --> [].
day_headers([Day|Rest]) -->
    { day_name(Day, DayName) },
    html(th(DayName)),
    day_headers(Rest).

% Generate time slot rows
time_slot_rows([], _, _) --> [].
time_slot_rows([TimeSlot|Rest], Days, Schedule) -->
    { EndTime is TimeSlot + 60,
      minutes_to_time(TimeSlot, StartTimeStr),
      minutes_to_time(EndTime, EndTimeStr),
      format(atom(TimeRange), '~w-~w', [StartTimeStr, EndTimeStr])
    },
    html(tr(
        [th(TimeRange) | \day_cells(Days, TimeSlot, Schedule)]
    )),
    time_slot_rows(Rest, Days, Schedule).

% Generated cells for each day in a time slot
day_cells([], _, _) --> [].
day_cells([Day|Rest], TimeSlot, Schedule) -->
    { findall(Entry,
             schedule_entry_for_cell(Schedule, Day, TimeSlot, Entry),
             Entries),
      % Get subject ID for this cell for coloring
      (cell_subject(Day, TimeSlot, SubjectID) -> 
          % Get a consistent color for this subject
          subject_color(SubjectID, Color),
          CellStyle = style(['background-color: ', Color])
      ; 
          CellStyle = ''
      )
    },
    html(td(CellStyle, \cell_entries(Entries))),
    day_cells(Rest, TimeSlot, Schedule).

% Dynamic predicate to track cell subjects
:- dynamic cell_subject/3.  % cell_subject(Day, TimeSlot, SubjectID)

% Generate a consistent color based on subject ID
subject_color(SubjectID, Color) :-
    Colors = ['#ffcccc', '#ccffcc', '#ccccff', '#ffffcc', '#ffccff', '#ccffff', '#ffddaa', '#ddffaa', '#aaddff', '#ffaadd'],
    length(Colors, Length),
    Index is (SubjectID mod Length) + 1,
    nth1(Index, Colors, Color).

% Generate content for entries in a cell
cell_entries([]) --> 
    html(span(class('empty-cell'), '-')).
cell_entries([Entry|Rest]) -->
    html(div(class('schedule-entry'), Entry)),
    cell_entries(Rest).

schedule_entry_for_cell(Schedule, Day, TimeSlot, FormattedEntry) :-
    member(schedule(GroupID, SubjectID, TeacherID, RoomID, Day, Start, End), Schedule),
    Start =< TimeSlot,
    End > TimeSlot,
    % Get subject and teacher names
    database:subject(SubjectID, SubjectName, _, _, _),
    database:teacher(TeacherID, TeacherName, _),
    % Use HTML structure WITHOUT the html wrapper
    FormattedEntry = [
        strong(SubjectName), br([]),
        'Teacher ', TeacherName, br([]),
        'Group ', GroupID, br([]),
        'Room ', RoomID
    ],
    % Store the subject ID for coloring
    asserta(cell_subject(Day, TimeSlot, SubjectID)).

% Generate teacher buttons
teacher_buttons -->
    { findall(TeacherID-Name, 
             database:teacher(TeacherID, Name, _), 
             Teachers),
      sort(Teachers, SortedTeachers) },
    html(div(class('filter-buttons'), [
        h3('By Teacher:'), 
        div(class('button-group'), \teacher_button_items(SortedTeachers))
    ])).

teacher_button_items([]) --> [].
teacher_button_items([ID-Name|Rest]) --> 
    { format(atom(URL), '/teacher_schedule?id=~w', [ID]) },
    html(a([href(URL), class([btn, 'btn-primary'])], Name)),
    teacher_button_items(Rest).

% Generate group buttons
group_buttons -->
    { findall(GroupID, 
             database:group(GroupID, _, _), 
             Groups),
      sort(Groups, SortedGroups) },
    html(div(class('filter-buttons'), [
        h3('By Group:'), 
        div(class('button-group'), \group_button_items(SortedGroups))
    ])).

group_button_items([]) --> [].
group_button_items([ID|Rest]) --> 
    { format(atom(URL), '/group_schedule?id=~w', [ID]) },
    html(a([href(URL), class([btn, 'btn-primary'])], ['Group ', ID])),
    group_button_items(Rest).

% Generate room buttons
room_buttons -->
    { findall(RoomID, 
             database:room(RoomID, _, _), 
             Rooms),
      sort(Rooms, SortedRooms) },
    html(div(class('filter-buttons'), [
        h3('By Room:'), 
        div(class('button-group'), \room_button_items(SortedRooms))
    ])).

room_button_items([]) --> [].
room_button_items([ID|Rest]) --> 
    { format(atom(URL), '/room_schedule?id=~w', [ID]) },
    html(a([href(URL), class([btn, 'btn-primary'])], ['Room ', ID])),
    room_button_items(Rest).

% Export schedule as standalone HTML file with specified title
export_schedule_as_html(Schedule, Title, HTMLContent) :-
    phrase(html(
        html([
            head([
                title(Title),
                style(\export_css)
            ]),
            body([
                h1(Title),
                \matrix_schedule_table(Schedule, Title)
            ])
        ])), Tokens),
    with_output_to(atom(HTMLContent), print_html(Tokens)).

% CSS for exported schedules - use DCG to generate the CSS content
export_css -->
    html('
        body { font-family: Arial, sans-serif; margin: 20px; }
        h1, h2 { color: #333; }
        .schedule-container { margin: 20px; overflow-x: auto; background: white; padding: 15px; border-radius: 5px; box-shadow: 0 0 10px rgba(0, 0, 0, 0.1); }
        .schedule-table { border-collapse: collapse; width: 100%; margin-bottom: 30px; }
        .schedule-table th, .schedule-table td { border: 1px solid #ddd; padding: 8px; text-align: center; }
        .schedule-table th { background-color: #333; color: white; position: sticky; top: 0; }
        .schedule-table tr:nth-child(even) { background-color: #f9f9f9; }
        .schedule-table tr:hover { background-color: #f1f1f1; }
        .schedule-entry { padding: 8px; margin: 2px; border-radius: 4px; background-color: #e1f5fe; font-size: 12px; border-left: 4px solid #03a9f4; text-align: left; }
        .empty-cell { color: #ccc; font-style: italic; }
        strong { font-weight: bold; }
        em { font-style: italic; }
        b { font-weight: bold; }
        i { font-style: italic; }
    ').

% DCG for tables without delete buttons
teacher_table([]) --> [].
teacher_table([teacher(ID, Name, Quals)|Rest]) -->
    { atomic_list_concat(Quals, ', ', QualsStr) },
    html(tr([
        td(ID), 
        td(Name), 
        td(QualsStr)
    ])),
    teacher_table(Rest).

room_table([]) --> [].
room_table([room(ID, Capacity, Equip)|Rest]) -->
    { atomic_list_concat(Equip, ', ', EquipStr) },
    html(tr([
        td(ID), 
        td(Capacity), 
        td(EquipStr)
    ])),
    room_table(Rest).

group_table([]) --> [].
group_table([group(ID, Subjects, NumStudents)|Rest]) -->
    { atomic_list_concat(Subjects, ', ', SubjectsStr) },
    html(tr([
        td(ID), 
        td(SubjectsStr), 
        td(NumStudents)
    ])),
    group_table(Rest).

subject_table([]) --> [].
subject_table([subject(ID, Name, Hours, Equip, Priority)|Rest]) -->
    { atomic_list_concat(Equip, ', ', EquipStr) },
    html(tr([
        td(ID), 
        td(Name), 
        td(Hours), 
        td(EquipStr), 
        td(Priority)
    ])),
    subject_table(Rest).

% Redirect with error and preserve form data
redirect_with_error(Request, Location, Error) :-
    http_parameters(Request, Params, [form_data(post)]),
    http_redirect(moved, Location, [method(post), error(Error)|Params], Request).

% Error message handler
error_message(empty_name, 'Error: Name cannot be empty.') :- !.
error_message(empty_field(Field), Msg) :- format(atom(Msg), 'Error: ~w cannot be empty.', [Field]), !.
error_message(invalid_capacity(Value), Msg) :- format(atom(Msg), 'Error: Capacity "~w" must be a positive integer.', [Value]), !.
error_message(invalid_number_students(Value), Msg) :- format(atom(Msg), 'Error: Number of students "~w" must be a positive integer.', [Value]), !.
error_message(invalid_hours(Value), Msg) :- format(atom(Msg), 'Error: Hours "~w" must be a positive integer.', [Value]), !.
error_message(invalid_priority(Value), Msg) :- format(atom(Msg), 'Error: Priority "~w" must be an integer between 1 and 5.', [Value]), !.
error_message(duplicate_id(ID), Msg) :- format(atom(Msg), 'Error: ID "~w" is already in use.', [ID]), !.
error_message(invalid_integer(Field, Value), Msg) :- format(atom(Msg), 'Error: ~w "~w" must be a valid integer.', [Field, Value]), !.
error_message(invalid_subject_ids(Value), Msg) :- format(atom(Msg), 'Error: Subject IDs "~w" must be comma-separated integers.', [Value]), !.
error_message(missing_subject(ID), Msg) :- format(atom(Msg), 'Error: Subject with ID ~w does not exist. Please add it first.', [ID]), !.
error_message(Error, Msg) :- format(atom(Msg), 'Unexpected error: ~w', [Error]).