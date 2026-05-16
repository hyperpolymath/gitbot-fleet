% SPDX-License-Identifier: PMPL-1.0-or-later
% Auto-generated rule: Detect unsafe blocks without documentation
% Generated: 2026-02-06T22:00:00+00:00
% Observations: 19
% CWE: CWE-1188 (Insecure Default Initialization)
% Pattern: Rust unsafe blocks without preceding safety documentation

:- object(unsafe_without_doc_detector,
    extends(code_pattern_detector)).

    :- info([
        version is 1:0:0,
        author is 'Hypatia Learning Loop',
        date is 2026-02-06,
        comment is 'Detects Rust unsafe blocks that lack safety documentation comments',
        remarks is [
            'Pattern learned from 19 observations across 2 repositories',
            'Most prevalent in affinescript (14 instances), vordr (5 instances)',
            'Ensures all unsafe code has explicit safety invariants documented',
            'Critical for memory safety verification and audit trails'
        ]
    ]).

    % Detection logic: Find unsafe blocks without preceding documentation
    has_issue(Path, unsafe_without_doc(Location, UnsafeBlock)) :-
        source_file(Path),
        rust_file(Path),
        file_contains_unsafe(Path, Location, UnsafeBlock),
        \+ has_safety_doc(Path, Location).

    % Identify Rust source files
    rust_file(Path) :-
        atom_concat(_, '.rs', Path).

    % Detect unsafe block pattern
    file_contains_unsafe(Path, line(LineNum), Block) :-
        file_line(Path, LineNum, Line),
        contains_unsafe_block(Line, Block).

    % unsafe block patterns
    contains_unsafe_block(Line, Block) :-
        sub_atom(Line, Start, _, _, 'unsafe'),
        sub_atom(Line, BlockStart, _, _, '{'),
        BlockStart > Start,
        sub_atom(Line, Start, _, 0, Block).

    % Check for safety documentation in preceding lines
    has_safety_doc(Path, line(LineNum)) :-
        PrevLine is LineNum - 1,
        PrevLine > 0,
        file_line(Path, PrevLine, Line),
        is_safety_comment(Line).

    has_safety_doc(Path, line(LineNum)) :-
        PrevLine is LineNum - 2,
        PrevLine > 0,
        file_line(Path, PrevLine, Line),
        is_safety_comment(Line).

    % Safety comment patterns
    is_safety_comment(Line) :-
        sub_atom(Line, _, _, _, '//'),
        (contains_safety_keyword(Line) ;
         contains_invariant_keyword(Line) ;
         contains_rationale_keyword(Line)).

    % Keywords indicating safety documentation
    contains_safety_keyword(Line) :-
        (sub_atom(Line, _, _, _, 'SAFETY:') ;
         sub_atom(Line, _, _, _, 'Safety:') ;
         sub_atom(Line, _, _, _, 'safe because') ;
         sub_atom(Line, _, _, _, 'Safe because')).

    contains_invariant_keyword(Line) :-
        (sub_atom(Line, _, _, _, 'invariant') ;
         sub_atom(Line, _, _, _, 'Invariant') ;
         sub_atom(Line, _, _, _, 'guarantee') ;
         sub_atom(Line, _, _, _, 'Guarantee')).

    contains_rationale_keyword(Line) :-
        (sub_atom(Line, _, _, _, 'rationale') ;
         sub_atom(Line, _, _, _, 'Rationale') ;
         sub_atom(Line, _, _, _, 'reason') ;
         sub_atom(Line, _, _, _, 'Reason')).

    % Severity classification: HIGH (memory safety critical)
    classify_severity(unsafe_without_doc(_, _), high).

    % Context-dependent severity: escalate in production code
    classify_severity_contextual(unsafe_without_doc(Location, _), critical) :-
        in_production_code(Location).

    in_production_code(line(LineNum)) :-
        \+ in_test_code(LineNum).

    in_test_code(LineNum) :-
        file_line(Path, LineNum, _),
        (sub_atom(Path, _, _, _, '/test/') ;
         sub_atom(Path, _, _, _, '/tests/') ;
         atom_concat(_, '_test.rs', Path)).

    % Fix suggestion
    suggest_fix(unsafe_without_doc(_, Block), Fix) :-
        atom_concat('Add safety documentation comment before unsafe block: ', Block, Part1),
        atom_concat(Part1, '\n// SAFETY: [Explain why this is safe]', Part2),
        atom_concat(Part2, '\n// - Document safety invariants', Part3),
        atom_concat(Part3, '\n// - Explain caller obligations', Part4),
        atom_concat(Part4, '\n// - Justify why unsafe is necessary', Fix).

    % Not auto-fixable (requires human safety analysis)
    auto_fixable(unsafe_without_doc(_, _), false).

    % Quality metrics
    unsafe_documentation_ratio(Path, Ratio) :-
        rust_file(Path),
        findall(1, file_contains_unsafe(Path, _, _), AllUnsafe),
        findall(1, (file_contains_unsafe(Path, Loc, _), has_safety_doc(Path, Loc)), Documented),
        length(AllUnsafe, Total),
        length(Documented, DocCount),
        (Total > 0 -> Ratio is DocCount / Total ; Ratio is 1.0).

    % Alert if documentation ratio is poor
    poor_unsafe_documentation(Path) :-
        unsafe_documentation_ratio(Path, Ratio),
        Ratio < 0.5.  % Less than 50% documented

    % Recommend: Use safe abstractions where possible
    suggest_safe_alternative(Path, Location) :-
        has_issue(Path, unsafe_without_doc(Location, Block)),
        potentially_avoidable_unsafe(Block).

    potentially_avoidable_unsafe(Block) :-
        (sub_atom(Block, _, _, _, 'transmute') ;
         sub_atom(Block, _, _, _, 'raw pointer') ;
         sub_atom(Block, _, _, _, 'as *const') ;
         sub_atom(Block, _, _, _, 'as *mut')).

:- end_object.
