% SPDX-License-Identifier: PMPL-1.0-or-later
% Auto-generated rule: Detect technical_debt markers
% Generated: 2026-02-06T22:00:00+00:00
% Observations: 196
% CWE: CWE-1057 (Data Access from Outside Expected Data Manager)
% Pattern: TODO/FIXME/HACK/XXX/BUG comments indicating deferred work

:- object(technical_debt_detector,
    extends(code_pattern_detector)).

    :- info([
        version is 1:0:0,
        author is 'Hypatia Learning Loop',
        date is 2026-02-06,
        comment is 'Detects technical debt markers (TODO, FIXME, HACK, XXX, BUG) across codebases',
        remarks is [
            'Pattern learned from 196 observations across 7 repositories',
            'Most prevalent in affinescript (143 markers), vordr (34 markers)',
            'Enables systematic tracking of deferred work and maintenance items'
        ]
    ]).

    % Detection logic: Find TODO/FIXME/HACK/XXX/BUG comments
    has_issue(Path, technical_debt(Location, Marker, Message)) :-
        source_file(Path),
        file_contains_pattern(Path, Location, DebtPattern),
        debt_marker_pattern(DebtPattern, Marker, Message),
        valid_debt_marker(Marker).

    % Debt marker patterns
    debt_marker_pattern(Line, 'TODO', Message) :-
        sub_atom(Line, _, _, _, 'TODO:'),
        extract_message_after(Line, 'TODO:', Message).

    debt_marker_pattern(Line, 'FIXME', Message) :-
        sub_atom(Line, _, _, _, 'FIXME:'),
        extract_message_after(Line, 'FIXME:', Message).

    debt_marker_pattern(Line, 'HACK', Message) :-
        sub_atom(Line, _, _, _, 'HACK:'),
        extract_message_after(Line, 'HACK:', Message).

    debt_marker_pattern(Line, 'XXX', Message) :-
        sub_atom(Line, _, _, _, 'XXX:'),
        extract_message_after(Line, 'XXX:', Message).

    debt_marker_pattern(Line, 'BUG', Message) :-
        sub_atom(Line, _, _, _, 'BUG:'),
        extract_message_after(Line, 'BUG:', Message).

    % Valid markers (case-insensitive)
    valid_debt_marker('TODO').
    valid_debt_marker('FIXME').
    valid_debt_marker('HACK').
    valid_debt_marker('XXX').
    valid_debt_marker('BUG').

    % Helper: Extract message after marker
    extract_message_after(Line, Marker, Message) :-
        sub_atom(Line, Before, _, After, Marker),
        MarkerEnd is Before + atom_length(Marker),
        sub_atom(Line, MarkerEnd, After, 0, MessageRaw),
        atom_trim(MessageRaw, Message).

    % Severity classification: INFO (tracking, not critical)
    classify_severity(technical_debt(_, _, _), info).

    % Priority by marker type (for triage)
    marker_priority('BUG', critical).
    marker_priority('FIXME', high).
    marker_priority('HACK', medium).
    marker_priority('TODO', low).
    marker_priority('XXX', low).

    % Fix suggestion
    suggest_fix(technical_debt(_, Marker, Message), FixSuggestion) :-
        marker_priority(Marker, Priority),
        atom_concat('Create GitHub issue for ', Marker, Part1),
        atom_concat(Part1, ' item with priority: ', Part2),
        atom_concat(Part2, Priority, Part3),
        atom_concat(Part3, '. Message: ', Part4),
        atom_concat(Part4, Message, FixSuggestion).

    % Not auto-fixable (requires human triage and issue creation)
    auto_fixable(technical_debt(_, _, _), false).

    % Potential future auto-fix: Create GitHub issues automatically
    % Requires: GitHub API integration, issue deduplication, priority assignment

    % Metrics: Count debt by repository
    repo_debt_count(Repo, Count) :-
        findall(1,
            (source_file(Path),
             repo_path(Repo, Path),
             has_issue(Path, technical_debt(_, _, _))),
            Issues),
        length(Issues, Count).

    % Alert if debt exceeds threshold
    excessive_debt(Repo, Count) :-
        repo_debt_count(Repo, Count),
        Count > 50.  % Threshold: 50+ markers indicates high debt

:- end_object.
