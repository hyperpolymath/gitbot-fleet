% SPDX-License-Identifier: PMPL-1.0-or-later
% Auto-generated rule: Detect eval() and dynamic code execution
% Generated: 2026-02-06T22:00:00+00:00
% Observations: 7
% CWE: CWE-95 (Improper Neutralization of Directives in Dynamically Evaluated Code)
% Pattern: eval(), Function(), setTimeout/setInterval with string arguments

:- object(eval_usage_detector,
    extends(code_pattern_detector)).

    :- info([
        version is 1:0:0,
        author is 'Hypatia Learning Loop',
        date is 2026-02-06,
        comment is 'Detects dangerous dynamic code execution patterns in JavaScript/ReScript',
        remarks is [
            'Pattern learned from 7 observations in academic-workflow-suite',
            'Extremely dangerous - enables code injection attacks',
            'Common in legacy code, test utilities, and development tools',
            'Should be eliminated from production code paths'
        ]
    ]).

    % Detection logic: Find eval() and related dynamic execution
    has_issue(Path, eval_usage(Location, Pattern, Context)) :-
        source_file(Path),
        javascript_file(Path),
        file_contains_dynamic_eval(Path, Location, Pattern, Context).

    % Identify JavaScript/ReScript source files
    javascript_file(Path) :-
        (atom_concat(_, '.js', Path) ;
         atom_concat(_, '.jsx', Path) ;
         atom_concat(_, '.res', Path) ;
         atom_concat(_, '.mjs', Path) ;
         atom_concat(_, '.cjs', Path)).

    % Detect dangerous eval patterns
    file_contains_dynamic_eval(Path, line(LineNum), direct_eval, Context) :-
        file_line(Path, LineNum, Line),
        contains_eval_call(Line),
        extract_context(Line, Context).

    file_contains_dynamic_eval(Path, line(LineNum), function_constructor, Context) :-
        file_line(Path, LineNum, Line),
        contains_function_constructor(Line),
        extract_context(Line, Context).

    file_contains_dynamic_eval(Path, line(LineNum), string_timeout, Context) :-
        file_line(Path, LineNum, Line),
        contains_string_timeout(Line),
        extract_context(Line, Context).

    file_contains_dynamic_eval(Path, line(LineNum), string_interval, Context) :-
        file_line(Path, LineNum, Line),
        contains_string_interval(Line),
        extract_context(Line, Context).

    % Pattern matchers
    contains_eval_call(Line) :-
        sub_atom(Line, Before, _, _, 'eval('),
        \+ in_comment(Line, Before),
        \+ in_string_literal(Line, Before).

    contains_function_constructor(Line) :-
        (sub_atom(Line, Before, _, _, 'Function(') ;
         sub_atom(Line, Before, _, _, 'new Function(')),
        \+ in_comment(Line, Before),
        \+ in_string_literal(Line, Before).

    contains_string_timeout(Line) :-
        sub_atom(Line, Before, _, _, 'setTimeout('),
        sub_atom(Line, Before, _, _, '"', QuotePos),
        QuotePos > Before,
        \+ in_comment(Line, Before).

    contains_string_interval(Line) :-
        sub_atom(Line, Before, _, _, 'setInterval('),
        sub_atom(Line, Before, _, _, '"', QuotePos),
        QuotePos > Before,
        \+ in_comment(Line, Before).

    % Context checks
    in_comment(Line, Position) :-
        sub_atom(Line, CommentStart, _, _, '//'),
        CommentStart < Position.

    in_string_literal(Line, Position) :-
        findall(QuotePos,
            (sub_atom(Line, QuotePos, _, _, '"'),
             QuotePos < Position),
            Quotes),
        length(Quotes, Count),
        1 is Count mod 2.  % Odd number of quotes = inside string

    extract_context(Line, Context) :-
        atom_length(Line, Len),
        (Len > 60 ->
            sub_atom(Line, 0, 60, _, Context)
        ;
            Context = Line
        ).

    % Severity classification: CRITICAL (code injection)
    classify_severity(eval_usage(_, _, _), critical).

    % Context-dependent severity
    classify_severity_contextual(eval_usage(Location, _, _), critical) :-
        in_production_code(Location).

    classify_severity_contextual(eval_usage(Location, _, _), high) :-
        in_test_code(Location).

    in_production_code(line(LineNum)) :-
        file_line(Path, LineNum, _),
        \+ in_test_code(line(LineNum)),
        \+ in_dev_tools(Path).

    in_test_code(line(LineNum)) :-
        file_line(Path, LineNum, _),
        (sub_atom(Path, _, _, _, '/test/') ;
         sub_atom(Path, _, _, _, '/tests/') ;
         sub_atom(Path, _, _, _, '/__tests__/') ;
         atom_concat(_, '.test.js', Path) ;
         atom_concat(_, '.spec.js', Path)).

    in_dev_tools(Path) :-
        (sub_atom(Path, _, _, _, '/scripts/') ;
         sub_atom(Path, _, _, _, '/tools/') ;
         sub_atom(Path, _, _, _, '/build/') ;
         sub_atom(Path, _, _, _, 'webpack.config') ;
         sub_atom(Path, _, _, _, 'rollup.config')).

    % Fix suggestions by pattern type
    suggest_fix(eval_usage(_, direct_eval, _),
        'Replace eval() with safe alternatives:\n' +
        '- For JSON: use JSON.parse()\n' +
        '- For dynamic properties: use bracket notation obj[prop]\n' +
        '- For conditionals: use switch/match statements\n' +
        '- For dynamic imports: use import() or require()\n' +
        'eval() allows arbitrary code execution and cannot be made safe.').

    suggest_fix(eval_usage(_, function_constructor, _),
        'Replace Function() constructor with:\n' +
        '- Named functions or arrow functions\n' +
        '- Function expressions\n' +
        '- Strategy pattern for dynamic behavior\n' +
        'Function constructor enables code injection like eval().').

    suggest_fix(eval_usage(_, string_timeout, _),
        'Replace setTimeout(string) with function reference:\n' +
        '- BAD: setTimeout("doSomething()", 1000)\n' +
        '- GOOD: setTimeout(() => doSomething(), 1000)\n' +
        'String arguments are evaluated like eval().').

    suggest_fix(eval_usage(_, string_interval, _),
        'Replace setInterval(string) with function reference:\n' +
        '- BAD: setInterval("doSomething()", 1000)\n' +
        '- GOOD: setInterval(() => doSomething(), 1000)\n' +
        'String arguments are evaluated like eval().').

    % Not auto-fixable (requires code refactoring)
    auto_fixable(eval_usage(_, _, _), false).

    % Security metrics
    eval_count_by_repo(Repo, Count) :-
        findall(1,
            (source_file(Path),
             repo_path(Repo, Path),
             has_issue(Path, eval_usage(_, _, _))),
            Issues),
        length(Issues, Count).

    % Alert if eval detected in production code
    critical_eval_in_production(Path, Location) :-
        has_issue(Path, eval_usage(Location, Pattern, _)),
        in_production_code(Location),
        critical_pattern(Pattern).

    critical_pattern(direct_eval).
    critical_pattern(function_constructor).

    % CSP recommendation
    recommend_csp_header(Path) :-
        has_issue(Path, eval_usage(_, _, _)),
        \+ has_csp_protection(Path).

    has_csp_protection(Path) :-
        repo_has_csp_header(Path).

    % Suggest Content-Security-Policy header to block eval
    csp_recommendation :-
        'Add Content-Security-Policy header:\n' +
        'Content-Security-Policy: script-src \'self\'; object-src \'none\'\n' +
        'This blocks eval() and inline scripts at the browser level.'.

:- end_object.
