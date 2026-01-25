% SPDX-License-Identifier: PMPL-1.0-or-later
% Auto-generated rule proposal
% Pattern: getexn_on_external_data
% Type: unsafe_crash
% Observations: 6
% Generated: 2026-01-25T08:18:41+00:00

% TODO: Review and integrate into code-safety-lessons.lgt

has_unsafe_crash_issue(Path, getexn_on_external_data_pattern(Line)) :-
    read_code_line(Path, LineNum, Line),
    atom_concat(_, 'getexn_on_external_data', Line),
    LineNum.

classify_severity(getexn_on_external_data_pattern(_), high).

suggest_fix(getexn_on_external_data_pattern(_),
    'Auto-generated fix suggestion - NEEDS REVIEW').
