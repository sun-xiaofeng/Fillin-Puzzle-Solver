/**
 *  Fillin Puzzles
 */

:- ensure_loaded(library(clpfd)).

/**
 * main(PuzzleFile, WordlistFile, SolutionFile)
 * The predicate main takes in a puzzle file, a word list files, 
 * outputs the solution of the puzzle to a solution file. 
 * e.g. main(puzzle1, words1, sol1).
 */
main(PuzzleFile, WordlistFile, SolutionFile) :-
    read_file(PuzzleFile, Puzzle),
    read_file(WordlistFile, Wordlist),
    valid_puzzle(Puzzle),
    solve_puzzle(Puzzle, Wordlist, Solved),
    print_puzzle(SolutionFile, Solved).

read_file(Filename, Content) :-
    open(Filename, read, Stream),
    read_lines(Stream, Content),
    close(Stream).

read_lines(Stream, Content) :-
    read_line(Stream, Line, Last),
    (   Last = true
    ->  (   Line = []
        ->  Content = []
        ;   Content = [Line]
        )
    ;  Content = [Line|Content1],
        read_lines(Stream, Content1)
    ).

read_line(Stream, Line, Last) :-
    get_char(Stream, Char),
    (   Char = end_of_file
    ->  Line = [],
        Last = true
    ; Char = '\n'
    ->  Line = [],
        Last = false
    ;   Line = [Char|Line1],
        read_line(Stream, Line1, Last)
    ).

print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).

print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).

put_puzzle_char(Stream, Char) :-
    (   var(Char)
    ->  put_char(Stream, '_')
    ;   put_char(Stream, Char)
    ).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(samelength(Row), Rows).


samelength([], []).
samelength([_|L1], [_|L2]) :-
    same_length(L1, L2).

/**
 *  solve_puzzle(Puzzle, Words, Solution)
 *  should hold when Puzzle is a solved version of Puzzle0, with the
 *  empty slots filled in with words from WordList.  Puzzle0 and Puzzle
 *  should be lists of lists of characters (single-character atoms), one
 *  list per puzzle row.  WordList is also a list of lists of
 *  characters, one list per word.
 */
solve_puzzle(Puzzle0, Words, Puzzle1) :-
    fill_in_variables(Puzzle0, Puzzle1),        % Fill in logic variables into the puzzle
    find_all_slots(Puzzle1, Slots),             % Find out all slots 
    fill_in_words(Slots, Words).               % Fill all words into slots

/**
 *  filter(Predicate, List0, List)
 *  The predicate filter retains all elements in the List that satisfy the Pred
 */
filter(_, [], []).
filter(Pred, [X|Xs], Ys) :-               
    ( call(Pred, X) ->
        Ys = [X|Ys1]
    ;   Ys = Ys1
    ), filter(Pred, Xs, Ys1).

/**
 * fill_in_variables(Puzzle0, Puzzle)
 * The fill_in_variables will replace all the character '_' with a distinct logic variable for all rows
 */
fill_in_variables([], []).
fill_in_variables([X|Xs], [Y|Ys]) :-
    fill_in_variables_row(X, Y),
    fill_in_variables(Xs, Ys).

/**
 * fill_in_variables_row(Row0, Row)
 * The fill_in_variables_row will replace all the character '_' with a distinct logic variable 
 */
fill_in_variables_row([], []).
fill_in_variables_row([X|Xs], [Y|Ys]) :-        % If the character is '_', replace it with a logic variable
    ( X == '_' ->
        Y = _
    ;   Y = X
    ), fill_in_variables_row(Xs, Ys).

/**
 * find_slots_row(Row, Slots)
 * The find_slots_row takes in a row, returns a list of slots contained in this row
 */
find_slots_row(Row, Slots) :-
    find_slots_row(Row, [], Slots).
find_slots_row([], Slot, List) :-               % If the slot is not empty, add the slot to the list
    ( Slot \= [] ->
        List = [Slot]                           
    ;   List = []
    ).
find_slots_row([X|Xs], Slot, List) :-
    ( X == '#' ->                               % Find a '#', add the slot to the slot list. Otherwise keeps adding the current slot
        List = [Slot|List1],
        find_slots_row(Xs, [], List1)
    ;                                           
        append(Slot, [X], Slot1),               
        find_slots_row(Xs, Slot1, List)
    ).

/**
 * find_slots_row(Row, Slots)
 * The find_slots_row takes in a puzzle, returns a all horizontal slots contained in the puzzle
 */
find_slots_in_one_dimension([], []).
find_slots_in_one_dimension([X|Xs], List) :-
    find_slots_row(X, Slots0),                         % Find all slots in a row
    append(Slots0, Slots1, List),                      % Add the result to the list
    find_slots_in_one_dimension(Xs, Slots1).           % Find all slots in other rows

/**
 * valid_length(List)
 * The valid_length is true if the the length of the list is greater than 1
 */
valid_length(List) :-                                       % The predicate is true if the length of the list
    length(List, Length0),                                  % is greater than 1
    Length0 > 1.

/**
 * find_all_slots(Puzzle, Slots)
 * The find_all_slots takes in a puzzles, returns all slots (horizontal and vertical) contained in the puzzle
 */
find_all_slots(Puzzle0, Slots) :-
    find_slots_in_one_dimension(Puzzle0, Slots0),
    filter(valid_length, Slots0, FilteredRawSlots),         % Remove the slot if the length is less than 2
    transpose(Puzzle0, Puzzle1),                            % Transpose the puzzle
    find_slots_in_one_dimension(Puzzle1, Slots1),
    filter(valid_length, Slots1, FilteredColSlots),
    append(FilteredRawSlots, FilteredColSlots, Slots).      % Add all slots to the list

/**
 * fill_in_words(Slots, Words)
 * The fill_in_words tries to fill-in words into the puzzle
 */
fill_in_words([], []).
fill_in_words(Slots, Words) :-
    next_slot(Slots, Words, SelectedSlot),
    filter(test_unify(SelectedSlot), Words, MatchedWords),  % Select all words that can match the selected Slot
    member(Word, MatchedWords),                             % Select a word
    SelectedSlot = Word,                                    % Unify the slot with the word
    filter(\==(Word), Words, WordsLeft),                    % Remove the word from the list
    filter(\==(SelectedSlot), Slots, SlotsLeft),            % Remove the filled slots
    fill_in_words(SlotsLeft, WordsLeft).                    % Try remaining slots

/**
 * next_slot(Slots, Words, SelectedSlot) 
 * The next_slot finds the next slot that can be filled. 
 * It picks the slot that matches with fewest words to 
 * minimise the searching space.
 */
next_slot([Slot|Slots], Words, SelectedSlot) :-
    match_word(Slot, Words, NumberOfMatches),
    next_slot(Slots, Words, NumberOfMatches, Slot, SelectedSlot).

next_slot([], _, _, SelectedSlot, SelectedSlot).
next_slot([Slot|Slots], Words, MinimumMatches0, CurrentSelectedSlot0, SelectedSlot) :-  % Find the slot which has lowest number of matches by iterates through them
    match_word(Slot, Words, NumberOfMatches),
    ( NumberOfMatches < MinimumMatches0 ->                     % If the current slot has lower number of matches, update the selected slot
        MinimumMatches = NumberOfMatches,
        CurrentSelectedSlot = Slot
    ;   MinimumMatches = MinimumMatches0,
        CurrentSelectedSlot = CurrentSelectedSlot0
    ), next_slot(Slots, Words, MinimumMatches, CurrentSelectedSlot, SelectedSlot).  % Do the same for remaning slots
    
/**
 * match_word(Slot, Words, NumberOfMatches)
 * The match word takes in a slot and a list of words, it finds the number of words that can fit into the slot
 */
match_word(Slot, Words, NumberOfMatches) :-
    match_word(Slot, Words, 0, NumberOfMatches).

match_word(_, [], Acc, Acc).
match_word(Slot, [W|Ws], Acc0, NumberOfMatches) :-          % Find out how many words can fit into the slot by iterates through all words
    ( test_unify(Slot, W) ->                                % Test if the word fits the slots. If so, increment the count. 
        Acc is Acc0 + 1
    ;   Acc is Acc0
    ), match_word(Slot, Ws, Acc, NumberOfMatches).

/**
 * test_unify(Slot, Word).
 * The test_unify tests whether the slots and word 
 * can be unified without unifying them
 */
test_unify(Slot, Word) :-                                 
    not(not(Slot = Word)).
