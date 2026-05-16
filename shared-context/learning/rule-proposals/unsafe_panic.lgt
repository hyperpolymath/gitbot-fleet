% SPDX-License-Identifier: PMPL-1.0-or-later
% Auto-generated rule: Detect unsafe_panic issues
% Generated: 2026-02-06T21:31:24+00:00
% Observations: 15

:- object(unsafe_panic_detector,
    extends(code_pattern_detector)).

    % TODO: Implement detection logic for unsafe_panic
    has_issue(Path, unsafe_panic(Location)) :-
        source_file(Path),
        % Add specific detection logic here
        fail.  % Replace with actual implementation

    classify_severity(unsafe_panic(_), medium).

    suggest_fix(unsafe_panic(_), 'Manual review required for unsafe_panic').

    auto_fixable(unsafe_panic(_), false).

:- end_object.
