% SPDX-License-Identifier: PMPL-1.0-or-later
% Auto-generated rule: Detect unsafe_crash issues
% Generated: 2026-02-06T21:38:05+00:00
% Observations: 8

:- object(unsafe_crash_detector,
    extends(code_pattern_detector)).

    % TODO: Implement detection logic for unsafe_crash
    has_issue(Path, unsafe_crash(Location)) :-
        source_file(Path),
        % Add specific detection logic here
        fail.  % Replace with actual implementation

    classify_severity(unsafe_crash(_), medium).

    suggest_fix(unsafe_crash(_), 'Manual review required for unsafe_crash').

    auto_fixable(unsafe_crash(_), false).

:- end_object.
