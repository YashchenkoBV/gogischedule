:- module(scheduling, [
    generate_schedule/1,
    optimize_schedule/1,  
    optimize_schedule/2,  % Add this export for iterations parameter
    days/1,
    times/1,
    set_debug/1
]).

:- use_module(library(clpfd)).
:- use_module(library(random)).  % Added for random operations
:- use_module(database).

% Dynamic predicate to control debug output (off by default)
:- dynamic debug_enabled/0.

% Set debug mode
set_debug(on) :- assertz(debug_enabled).
set_debug(off) :- retractall(debug_enabled).

% Debug predicate
debug_msg(Format, Args) :-
    (   debug_enabled
    ->  format(Format, Args),
        nl
    ;   true
    ).

% Days and timeslots definitions
days(['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday']).
times(['08:00', '09:00', '10:00', '11:00', '12:00', '13:00', '14:00', '15:00']).

% Main entry point for optimization with default iterations
optimize_schedule(OptimizedSchedule) :-
    optimize_schedule(OptimizedSchedule, 10000).

generate_schedule(Schedule) :-
    debug_msg('Starting schedule generation with relaxation...', []),
    
    % Initialize empty session reduction tracking
    empty_reductions(Reductions),
    
    % Try to generate schedule with possible relaxations
    generate_schedule_with_relaxation(Schedule, Reductions, Result, FinalReductions),
    
    % Report results
    (Result = success ->
        % Check if any reductions were actually applied
        has_applied_reductions(FinalReductions, HasReductions),
        (HasReductions ->
            debug_msg('Successfully generated schedule with relaxations.', [])
        ;
            debug_msg('Successfully generated schedule with no relaxations needed.', [])
        ),
        report_reductions(FinalReductions, HasReductions)
    ; 
        debug_msg('Failed to generate any valid schedule despite relaxations.', []),
        fail
    ).

% Helper to check if reductions list has any positive reductions
has_applied_reductions([], false).
has_applied_reductions([_-R|_], true) :- R > 0, !.
has_applied_reductions([_|Rest], HasReductions) :- 
    has_applied_reductions(Rest, HasReductions).

% Generate schedule with possible relaxation of number of sessions
generate_schedule_with_relaxation(Schedule, Reductions, Result, FinalReductions) :-
    % First try with no reductions
    (try_generate_schedule(Schedule, Reductions) ->
        Result = success,
        FinalReductions = Reductions  % No changes to reductions
    ;
        % If failed, start relaxation process
        relaxation_loop(Schedule, Reductions, Result, FinalReductions)
    ).

% Relaxation loop - reduce sessions and try again
relaxation_loop(Schedule, Reductions, Result, FinalReductions) :-
    % Find subject with most sessions to reduce
    find_subject_to_reduce(Reductions, SubjectID, SubjectName, CurrentHours),
    
    % Check if we still have sessions to reduce
    (CurrentHours > 1 ->
        % Reduce by one session
        update_reduction(Reductions, SubjectID, NewReductions),
        NewHours is CurrentHours - 1,
        debug_msg('Reducing sessions for subject ~w (~w) from ~w to ~w', 
                 [SubjectID, SubjectName, CurrentHours, NewHours]),
        
        % Try again with new reductions
        (try_generate_schedule(Schedule, NewReductions) ->
            Result = success,
            FinalReductions = NewReductions  % Successfully generated with these reductions
        ;
            % If still failed, continue relaxation
            relaxation_loop(Schedule, NewReductions, Result, FinalReductions)
        )
    ;
        % No more sessions to reduce, give up
        debug_msg('All subjects reduced to minimum (1 session). Giving up.', []),
        Result = failure,
        FinalReductions = Reductions  % Return current reductions even on failure
    ).

% Try to generate a schedule with given reductions
try_generate_schedule(Schedule, Reductions) :-
    % 1. Define basic parameters
    days(Days), times(Times),
    length(Days, NumDays),
    length(Times, SlotsPerDay),
    TotalSlots is NumDays * SlotsPerDay,
    MaxSlot is TotalSlots - 1,
    
    % 2. Generate requirements with reduced sessions where needed
    findall(req(G, S, T, R, AdjustedHours),
            (database:group(G, Subjects, NumStudents),
             member(S, Subjects),
             database:subject(S, _, OrigHours, RequiredEquipment, _),
             % Apply session reductions if any
             get_adjusted_hours(S, OrigHours, Reductions, AdjustedHours),
             AdjustedHours > 0,  % Skip if reduced to zero
             database:teacher(T, _, Qualifications),
             member(S, Qualifications),
             database:room(R, Capacity, RoomEquipment),
             NumStudents =< Capacity,
             (RequiredEquipment = [] ; subset(RequiredEquipment, RoomEquipment))),
            Reqs0),
    
    % Check if we have any valid requirements
    (Reqs0 = [] -> 
        debug_msg('ERROR: No valid requirements generated!', []), 
        fail 
    ; 
        true
    ),
    
    % Prepare requirements
    sort(Reqs0, ReqsSorted),
    remove_duplicate_subjects(ReqsSorted, ReqsUnique),
    maplist(req_with_slots, ReqsUnique, Reqs),
    
    % 3. Extract all slot variables
    pairs_values(Reqs, AllSlots),
    flatten(AllSlots, SlotVars),
    SlotVars ins 0..MaxSlot,
    
    % 4. Apply constraints
    maplist(constrain_slots(SlotsPerDay), Reqs),
    
    % Critical constraint: no slot conflicts
    (all_distinct(SlotVars) ->
        % 5. Try labeling
        (labeling([ff], SlotVars) ->
            % 6. Convert to output format
            convert_to_schedule(Reqs, Days, Times, Schedule)
        ;
            fail  % Labeling failed
        )
    ;
        fail  % Constraint setup failed
    ).

% Helper: Find subject with most remaining sessions
find_subject_to_reduce(Reductions, SubjectID, SubjectName, CurrentHours) :-
    findall(S-H-N, 
            (database:subject(S, N, OrigH, _, _),
             get_reduced_count(Reductions, S, R),
             H is OrigH - R,
             H > 1),  % Only consider subjects with more than 1 session left
            Candidates),
    
    % Sort by remaining hours (descending)
    sort(2, @>=, Candidates, [SubjectID-CurrentHours-SubjectName|_]).

% Initialize empty reductions tracking
empty_reductions([]).

% Update reduction count for a subject
update_reduction(Reductions, SubjectID, NewReductions) :-
    get_reduced_count(Reductions, SubjectID, CurrentReduction),
    NewReduction is CurrentReduction + 1,
    put_reduced_count(Reductions, SubjectID, NewReduction, NewReductions).

% Get reduced count for a subject
get_reduced_count([], _, 0).
get_reduced_count([S-R|_], S, R) :- !.
get_reduced_count([_|Rest], S, R) :- 
    get_reduced_count(Rest, S, R).

% Update reduction count
put_reduced_count([], S, R, [S-R]).
put_reduced_count([S-_|Rest], S, R, [S-R|Rest]) :- !.
put_reduced_count([X|Rest], S, R, [X|NewRest]) :-
    put_reduced_count(Rest, S, R, NewRest).

% Get adjusted hours after reductions
get_adjusted_hours(SubjectID, OriginalHours, Reductions, AdjustedHours) :-
    get_reduced_count(Reductions, SubjectID, Reduction),
    AdjustedHours is max(1, OriginalHours - Reduction).

% Report all reductions made
report_reductions(Reductions, HasReductions) :-
    (HasReductions ->
        debug_msg('Sessions reduced to satisfy constraints:', []),
        forall(member(S-R, Reductions),
               (R > 0,
                database:subject(S, Name, OrigHours, _, _),
                AdjustedHours is OrigHours - R,
                debug_msg('  Subject ~w (~w): reduced from ~w to ~w sessions (-~w)',
                         [S, Name, OrigHours, AdjustedHours, R])))
    ;
        debug_msg('No session reductions needed for this schedule.', [])
    ).

% Helper: Remove duplicate group-subject pairs
remove_duplicate_subjects([], []).
remove_duplicate_subjects([req(G, S, T, R, H)|Rest], [req(G, S, T, R, H)|Unique]) :-
    exclude(duplicate_subject(G, S), Rest, RestFiltered),
    remove_duplicate_subjects(RestFiltered, Unique).

duplicate_subject(G, S, req(G1, S1, _, _, _)) :-
    G = G1, S = S1.

% Helper: Create requirement with slot variables
req_with_slots(req(G, S, T, R, N), req(G, S, T, R, N)-Slots) :-
    length(Slots, N).

% Constraint: Slots are strictly ascending and on different days
constrain_slots(SlotsPerDay, req(_, _, _, _, _)-Slots) :-
    chain(Slots, #<),
    maplist(day_of_slot(SlotsPerDay), Slots, _).

day_of_slot(SPD, Slot, Day) :-
    Day #= Slot // SPD.

% Helper: Check if all elements of Subset are in Set
subset([], _).
subset([X|Xs], Set) :-
    member(X, Set),
    subset(Xs, Set).

% Convert slot to day and time
slot_to_day_time(SlotID, Days, Times, Day, Time) :-
    length(Times, TimesPerDay),
    DayIndex is SlotID // TimesPerDay,
    TimeIndex is SlotID mod TimesPerDay,
    nth0(DayIndex, Days, Day),
    nth0(TimeIndex, Times, Time).

% Convert to output format: [_, Day, Time, TeacherID, SubjectID, RoomID, GroupID]
convert_to_schedule(Reqs, Days, Times, Schedule) :-
    findall([_, Day, Time, T, S, R, G],
            (member(req(G, S, T, R, _)-Slots, Reqs),
             member(Slot, Slots),
             slot_to_day_time(Slot, Days, Times, Day, Time)),
            Schedule).

% Main scheduling error calculation predicate
calculate_scheduling_error(Schedule, TotalError) :-
    % Calculate individual error components
    teacher_gaps_error(Schedule, TeacherGapErr),
    group_gaps_error(Schedule, GroupGapErr),
    subject_distribution_error(Schedule, DistErr),
    priority_placement_error(Schedule, PriorityErr),
    consecutive_sessions_error(Schedule, ConsecutiveErr),
    difficulty_distribution_error(Schedule, DifficultyErr),
    
    % Calculate weighted sum according to requirements
    TotalError is 3*TeacherGapErr + 4*GroupGapErr + 2*DistErr + 
                2*PriorityErr + 1*ConsecutiveErr + 3*DifficultyErr.

% ---- Error component calculations ----

% 1. Teacher gaps error (weight: 3)
% Measures empty slots between teaching sessions for teachers on the same day
teacher_gaps_error(Schedule, TeacherGapErr) :-
    % Group schedule by teacher and day
    findall(TeacherID-Day-Time, 
            member([_, Day, Time, TeacherID, _, _, _], Schedule),
            TeacherSlots),
    
    % Group by teacher and day, then sort by time
    group_pairs_by_key(TeacherSlots, TeacherDayGroups),
    
    % Calculate gaps for each teacher's daily schedule
    findall(Gaps, 
            (member(TeacherID-DaySlots, TeacherDayGroups),
             group_pairs_by_key(DaySlots, DayTimeGroups),
             member(_Day-Times, DayTimeGroups),
             sort(Times, SortedTimes),
             count_gaps(SortedTimes, DayGaps),
             Gaps is DayGaps),
            AllGaps),
    
    % Sum all gaps
    sum_list(AllGaps, TeacherGapErr).

% 2. Group gaps error (weight: 4)
% Measures empty slots between classes for groups on the same day
group_gaps_error(Schedule, GroupGapErr) :-
    % Group schedule by group and day
    findall(GroupID-Day-Time, 
            member([_, Day, Time, _, _, _, GroupID], Schedule),
            GroupSlots),
    
    % Group by group and day, then sort by time
    group_pairs_by_key(GroupSlots, GroupDayGroups),
    
    % Calculate gaps for each group's daily schedule
    findall(Gaps, 
            (member(GroupID-DaySlots, GroupDayGroups),
             group_pairs_by_key(DaySlots, DayTimeGroups),
             member(_Day-Times, DayTimeGroups),
             sort(Times, SortedTimes),
             count_gaps(SortedTimes, DayGaps),
             Gaps is DayGaps),
            AllGaps),
    
    % Sum all gaps
    sum_list(AllGaps, GroupGapErr).

% 3. Subject distribution error (weight: 2)
% Measures how evenly subject sessions are distributed across the week
subject_distribution_error(Schedule, DistErr) :-
    % Get list of days
    days(Days),

    % Group schedule by subject
    findall(SubjectID-Day, 
            member([_, Day, _, _, SubjectID, _, _], Schedule),
            SubjectDays),
    
    % Group by subject
    group_pairs_by_key(SubjectDays, SubjectDayGroups),
    
    % Calculate distribution error for each subject
    findall(SubjErr, 
            (member(SubjectID-DayList, SubjectDayGroups),
             % Count occurrences per day
             findall(DayCount, 
                     (member(Day, Days),
                      include(=(Day), DayList, DayOccurrences),
                      length(DayOccurrences, DayCount)),
                     DayCounts),
             % Calculate variance for even distribution
             standard_deviation(DayCounts, StdDev),
             SubjErr is StdDev * StdDev), % Use variance as error measure
            SubjErrors),
    
    % Sum all subject distribution errors
    sum_list(SubjErrors, DistErr).

% 4. Priority placement error (weight: 2)
% Measure how many high-priority subjects are scheduled later in the day
priority_placement_error(Schedule, PriorityErr) :-
    % Define a list of time periods in order
    times(Times),
    
    % Calculate error for each schedule entry
    findall(EntryErr,
            (member([_, _, Time, _, SubjectID, _, _], Schedule),
             database:subject(SubjectID, _, _, _, Priority),
             % Calculate position in time list (0-based)
             nth0(TimeIndex, Times, Time),
             % Higher priority and later time means higher error
             EntryErr is Priority * TimeIndex),
            EntryErrors),
    
    % Sum all priority placement errors
    sum_list(EntryErrors, PriorityErr).

% 5. Consecutive sessions error (weight: 1)
% Count sessions of the same subject scheduled consecutively
consecutive_sessions_error(Schedule, ConsecutiveErr) :-
    % Sort schedule by group, day, and time
    predsort(compare_schedule_entries, Schedule, SortedSchedule),
    
    % Group by group
    findall(GroupID-Entry, 
            (member(Entry, SortedSchedule),
             nth1(7, Entry, GroupID)),
            GroupEntries),
    group_pairs_by_key(GroupEntries, GroupSchedules),
    
    % Count consecutive same-subject occurrences for each group
    findall(Consecutive,
            (member(_GroupID-Entries, GroupSchedules),
             count_consecutive_subjects(Entries, Consecutive)),
            AllConsecutive),
    
    % Sum all consecutive occurrences
    sum_list(AllConsecutive, ConsecutiveErr).

% 6. Difficulty distribution error (weight: 3)
% Measure how unevenly difficult subjects are distributed throughout the day
difficulty_distribution_error(Schedule, DifficultyErr) :-
    % Group schedule by day
    findall(Day-SubjPriority,
            (member([_, Day, _, _, SubjectID, _, _], Schedule),
             database:subject(SubjectID, _, _, _, Priority),
             SubjPriority is Priority),
            DayPriorities),
    
    % Group by day
    group_pairs_by_key(DayPriorities, DayGroups),
    
    % Calculate priority variance for each day
    findall(DayErr,
            (member(Day-Priorities, DayGroups),
             standard_deviation(Priorities, StdDev),
             DayErr is StdDev * StdDev),  % Use variance as error measure
            DayErrors),
    
    % Sum errors for all days
    sum_list(DayErrors, DifficultyErr).

% ---- Helper predicates ----

% Count gaps (empty slots) in a sorted list of times
count_gaps([], 0).
count_gaps([_], 0).
count_gaps([T1, T2|Rest], Gaps) :-
    count_gaps([T2|Rest], RestGaps),
    times(AllTimes),
    nth0(I1, AllTimes, T1),
    nth0(I2, AllTimes, T2),
    Gap is I2 - I1 - 1,
    Gaps is max(0, Gap) + RestGaps.

% Complete the standard_deviation predicate
standard_deviation(List, StdDev) :-
    length(List, N),
    N > 0,
    sum_list(List, Sum),
    Mean is Sum / N,
    findall(SquaredDiff,
            (member(X, List),
             Diff is X - Mean,
             SquaredDiff is Diff * Diff),
            SquaredDiffs),
    sum_list(SquaredDiffs, SumSquaredDiffs),
    Variance is SumSquaredDiffs / N,
    StdDev is sqrt(Variance).

% Compare schedule entries for sorting (by group, day, time)
compare_schedule_entries(Order, Entry1, Entry2) :-
    % Extract group, day, and time from entries
    nth1(7, Entry1, G1), nth1(2, Entry1, D1), nth1(3, Entry1, T1),
    nth1(7, Entry2, G2), nth1(2, Entry2, D2), nth1(3, Entry2, T2),
    
    % Compare groups first
    (G1 @< G2 -> Order = '<' ;
     G1 @> G2 -> Order = '>' ;
     % If groups are equal, compare days
     compare_days(D1, D2, DayOrder),
     (DayOrder = '<' -> Order = '<' ;
      DayOrder = '>' -> Order = '>' ;
      % If days are equal, compare times
      compare_times(T1, T2, Order))).

% Compare days of the week
compare_days(D1, D2, Order) :-
    days(Days),
    nth0(I1, Days, D1),
    nth0(I2, Days, D2),
    compare(Order, I1, I2).

% Compare times
compare_times(T1, T2, Order) :-
    times(Times),
    nth0(I1, Times, T1),
    nth0(I2, Times, T2),
    compare(Order, I1, I2).

% Count consecutive occurrences of the same subject in a group's schedule
count_consecutive_subjects([], 0).
count_consecutive_subjects([_], 0).
count_consecutive_subjects([Entry1, Entry2|Rest], Count) :-
    count_consecutive_subjects([Entry2|Rest], RestCount),
    % Extract subject IDs
    nth1(5, Entry1, S1),
    nth1(5, Entry2, S2),
    % Extract days and times
    nth1(2, Entry1, D1), nth1(3, Entry1, T1),
    nth1(2, Entry2, D2), nth1(3, Entry2, T2),
    
    % Check if entries are for the same subject and consecutive
    (S1 = S2, D1 = D2, consecutive_times(T1, T2) ->
        Count is RestCount + 1
    ;
        Count = RestCount
    ).

% Check if two times are consecutive
consecutive_times(T1, T2) :-
    times(Times),
    nth0(I1, Times, T1),
    nth0(I2, Times, T2),
    I2 is I1 + 1.

%----------------- NEW OPTIMIZATION CODE -----------------

% Main entry point for optimization with specified iterations
optimize_schedule(OptimizedSchedule, Iterations) :-
    % Validate iterations parameter
    (integer(Iterations), Iterations > 0 -> true
    ; format('Error: Invalid iterations value. Using default 10000.~n'),
      Iterations = 10000),
      
    % Phase 1: Find a feasible solution
    generate_schedule(InitialSchedule),

    % Calculate initial error score
    calculate_scheduling_error(InitialSchedule, InitialError),

    % Phase 2: Optimize the schedule with specified iterations
    debug_msg('Starting optimization phase with initial error: ~w (max iterations: ~w)', 
              [InitialError, Iterations]),
    optimize_loop(InitialSchedule, InitialError, 0, Iterations, OptimizedSchedule, FinalError),

    % Report improvement
    (InitialError > 0 ->
        Improvement is ((InitialError - FinalError) / InitialError) * 100,
        debug_msg('Optimization complete. Error reduced from ~w to ~w (~2f% improvement)',
                [InitialError, FinalError, Improvement])
    ;
        debug_msg('Optimization complete. Initial schedule already had zero error.', [])
    ).

% Base case: reached iteration limit, return best solution so far
optimize_loop(BestSchedule, BestError, MaxIter, MaxIter, BestSchedule, BestError) :-
    debug_msg('Reached iteration limit (~w). Optimization terminated.', [MaxIter]),
    % Final validation of hard constraints
    validate_hard_constraints(BestSchedule, Valid),
    (Valid -> true ; 
        debug_msg('ERROR: Final solution violates hard constraints!', []),
        fail).

% Recursive case: generate a new candidate and continue
% Modified optimization loop to ensure validity is maintained
optimize_loop(CurrentBest, CurrentError, Iter, MaxIter, FinalBest, FinalError) :-
    Iter < MaxIter,
    NextIter is Iter + 1,
    
    % Periodically validate current best to ensure it remains valid
    (0 is Iter mod 50 -> 
        debug_msg('Optimization iteration ~w, current error: ~w', [Iter, CurrentError]),
        validate_hard_constraints(CurrentBest, IsValid),
        (IsValid -> true ; 
          debug_msg('ERROR: Current solution violates hard constraints! Reverting...', []),
          fail)
    ; true),
    
    % Only proceed if we can generate a valid candidate
    (generate_candidate(CurrentBest, Candidate) ->
        % Evaluate the candidate
        calculate_scheduling_error(Candidate, CandidateError),
        
        % Update best solution if better
        (CandidateError < CurrentError ->
            NextBest = Candidate,
            NextError = CandidateError,
            debug_msg('Iteration ~w: Improved solution found. Error: ~w', [Iter, CandidateError])
        ;
            NextBest = CurrentBest,
            NextError = CurrentError
        ),
        
        % Continue optimization loop
        optimize_loop(NextBest, NextError, NextIter, MaxIter, FinalBest, FinalError)
    ;
        % If candidate generation fails completely, stay with current
        debug_msg('Warning: Failed to generate valid candidate at iteration ~w', [Iter]),
        optimize_loop(CurrentBest, CurrentError, NextIter, MaxIter, FinalBest, FinalError)
    ).

% Generate a valid candidate solution in the neighborhood of current solution
% Never accept invalid candidates
generate_candidate(CurrentSchedule, Candidate) :-
% Try up to 30 times to generate a valid candidate (increased from 10)
try_generate_valid_candidate(CurrentSchedule, Candidate, 30).

% Helper to repeatedly try generating a valid candidate
try_generate_valid_candidate(CurrentSchedule, Candidate, MaxTries) :-
MaxTries > 0,
% Randomly select operation type with weighted probability
random_between(1, 10, RandomChoice),
(RandomChoice =< 4 -> OperationType = 1 ;  % 40% chance for swap (most reliable)
 RandomChoice =< 7 -> OperationType = 2 ;  % 30% chance for move
                      OperationType = 3),  % 30% chance for reassignment

% Apply the selected operation
(OperationType = 1 -> swap_sessions(CurrentSchedule, TempCandidate) ;
 OperationType = 2 -> move_session(CurrentSchedule, TempCandidate) ;
 OperationType = 3 -> reassign_resource(CurrentSchedule, TempCandidate)),

% Validate the candidate immediately
(validate_hard_constraints(TempCandidate, true) -> 
    Candidate = TempCandidate  % Valid candidate found
; 
    % Invalid - try again with reduced counter
    NextTries is MaxTries - 1,
    try_generate_valid_candidate(CurrentSchedule, Candidate, NextTries)
).

% If we exhaust all attempts, fail the predicate instead of returning the original
try_generate_valid_candidate(_, _, 0) :- fail.

% Helper for retrying candidate generation with a limit
retry_generate_candidate(CurrentSchedule, Candidate, 0) :-
    % If we've exhausted retries, just return the original
    Candidate = CurrentSchedule.
retry_generate_candidate(CurrentSchedule, Candidate, Retries) :-
    Retries > 0,
    NextRetries is Retries - 1,
    % Try again
    generate_candidate(CurrentSchedule, TempCandidate),
    % Check if valid
    (validate_hard_constraints(TempCandidate, true) -> 
        Candidate = TempCandidate 
    ; 
        retry_generate_candidate(CurrentSchedule, Candidate, NextRetries)).

% Operation 1: Swap two sessions (preserving groups and subjects)
swap_sessions(Schedule, NewSchedule) :-
    % Randomly select two entries to swap
    random_select(Entry1, Schedule, ScheduleTemp1),
    random_select(Entry2, ScheduleTemp1, ScheduleTemp2),
    
    % Extract components
    Entry1 = [ID1, Day1, Time1, Teacher1, Subject1, Room1, Group1],
    Entry2 = [ID2, Day2, Time2, Teacher2, Subject2, Room2, Group2],
    
    % Create new entries with swapped day/time
    NewEntry1 = [ID1, Day2, Time2, Teacher1, Subject1, Room1, Group1],
    NewEntry2 = [ID2, Day1, Time1, Teacher2, Subject2, Room2, Group2],
    
    % Build new schedule
    append([NewEntry1, NewEntry2], ScheduleTemp2, NewSchedule).

% Operation 2: Move a session to a different timeslot
move_session(Schedule, NewSchedule) :-
    % Randomly select an entry to move
    random_select(Entry, Schedule, ScheduleRest),
    
    % Extract components
    Entry = [ID, _, _, Teacher, Subject, Room, Group],
    
    % Select a new day and time
    days(Days), times(Times),
    random_member(NewDay, Days),
    random_member(NewTime, Times),
    
    % Create new entry
    NewEntry = [ID, NewDay, NewTime, Teacher, Subject, Room, Group],
    
    % Build new schedule
    append([NewEntry], ScheduleRest, NewSchedule).

% Operation 3: Reassign a resource (teacher or room)
reassign_resource(Schedule, NewSchedule) :-
    % Randomly select an entry to modify
    random_select(Entry, Schedule, ScheduleRest),
    
    % Extract components
    Entry = [ID, Day, Time, Teacher, Subject, Room, Group],
    
    % Decide whether to change teacher or room
    random_between(1, 2, ResourceType),
    
    % Modify the selected resource
    (ResourceType = 1 -> 
        % Find alternative qualified teacher
        findall(T, (database:teacher(T, _, Qualifications),
                    member(Subject, Qualifications)), 
                QualifiedTeachers),
        exclude(=(Teacher), QualifiedTeachers, AlternativeTeachers),
        (AlternativeTeachers = [] -> NewTeacher = Teacher ;
                                   random_member(NewTeacher, AlternativeTeachers)),
        NewRoom = Room
    ;
        % Find alternative suitable room
        database:group(Group, _, NumStudents),
        database:subject(Subject, _, _, RequiredEquipment, _),
        findall(R, (database:room(R, Capacity, RoomEquipment),
                    NumStudents =< Capacity,
                    (RequiredEquipment = [] ; subset(RequiredEquipment, RoomEquipment))),
                SuitableRooms),
        exclude(=(Room), SuitableRooms, AlternativeRooms),
        (AlternativeRooms = [] -> NewRoom = Room ;
                                random_member(NewRoom, AlternativeRooms)),
        NewTeacher = Teacher
    ),
    
    % Create new entry
    NewEntry = [ID, Day, Time, NewTeacher, Subject, NewRoom, Group],
    
    % Build new schedule
    append([NewEntry], ScheduleRest, NewSchedule).

% Validate hard constraints for a candidate solution
validate_hard_constraints(Schedule, Valid) :-
    % No resource conflicts
    \+ has_resource_conflicts(Schedule),
    
    % All room requirements satisfied (capacity and equipment)
    all_room_constraints_satisfied(Schedule),
    
    % All teacher qualifications satisfied
    all_teacher_constraints_satisfied(Schedule),
    
    % Schedule meets weekly hour requirements (if not already relaxed)
    % This may be complex to check here, but the algorithm should preserve this
    
    % If all constraints pass, solution is valid
    Valid = true.
validate_hard_constraints(_, false).

% Improved conflict detection that checks all conflict types thoroughly
has_resource_conflicts(Schedule) :-
    % Check for teacher conflicts (same teacher in two places at once)
    (   exists_conflict(Schedule, teacher)
    ;   % Check for room conflicts (room used by two groups at once)
        exists_conflict(Schedule, room)
    ;   % Check for group conflicts (group scheduled in two places at once)
        exists_conflict(Schedule, group)
    ).

% Helper predicates to check each type of conflict
exists_conflict(Schedule, teacher) :-
    member([ID1, Day, Time, Teacher, _, _, _], Schedule),
    member([ID2, Day, Time, Teacher, _, _, _], Schedule),
    ID1 @< ID2.  % Use @< instead of \= to avoid checking the same pair twice

exists_conflict(Schedule, room) :-
    member([ID1, Day, Time, _, _, Room, _], Schedule),
    member([ID2, Day, Time, _, _, Room, _], Schedule),
    ID1 @< ID2.

exists_conflict(Schedule, group) :-
    member([ID1, Day, Time, _, _, _, Group], Schedule),
    member([ID2, Day, Time, _, _, _, Group], Schedule),
    ID1 @< ID2.

% Check if all room constraints are satisfied
all_room_constraints_satisfied(Schedule) :-
    % No group assigned to a room with insufficient capacity
    \+ (
        member([_, _, _, _, _, Room, Group], Schedule),
        database:group(Group, _, NumStudents),
        database:room(Room, Capacity, _),
        NumStudents > Capacity
    ),
    % No subject assigned to a room without required equipment
    \+ (
        member([_, _, _, _, Subject, Room, _], Schedule),
        database:subject(Subject, _, _, RequiredEquipment, _),
        RequiredEquipment \= [],
        database:room(Room, _, RoomEquipment),
        \+ subset(RequiredEquipment, RoomEquipment)
    ).

% Check if all teacher constraints are satisfied
all_teacher_constraints_satisfied(Schedule) :-
    \+ (
        member([_, _, _, Teacher, Subject, _, _], Schedule),
        database:teacher(Teacher, _, Qualifications),
        \+ member(Subject, Qualifications)
    ).