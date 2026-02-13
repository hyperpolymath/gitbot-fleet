%% SPDX-License-Identifier: AGPL-3.0-or-later
%% SPDX-FileCopyrightText: 2024-2025 hyperpolymath
%%
%% Oikos Bot Policy Engine - DeepProbLog Rules
%% ===========================================
%% Probabilistic logic programming rules that learn from practice.
%% These rules complement the deterministic Datalog rules by handling
%% uncertainty and learning patterns from observed outcomes.
%%
%% Reference: https://github.com/ML-KULeuven/deepproblog

%% =============================================================================
%% NEURAL PREDICATES
%% =============================================================================

%% Neural network for estimating carbon intensity from code features
%% Input: Code feature vector (complexity, loop depth, allocations, etc.)
%% Output: Probability of high carbon intensity
nn(carbon_estimator, [CodeFeatures], CarbonProb) :: high_carbon_prob(Code, CarbonProb) :-
    code_features(Code, CodeFeatures).

%% Neural network for energy pattern classification
nn(energy_classifier, [CodeFeatures], EnergyClass) :: energy_pattern(Code, EnergyClass) :-
    code_features(Code, CodeFeatures).

%% Neural network for predicting refactoring success
nn(refactor_predictor, [CodeFeatures, RefactorType], SuccessProb) ::
    refactor_success_prob(Code, RefactorType, SuccessProb) :-
    code_features(Code, CodeFeatures).

%% Neural network for technical debt estimation
nn(debt_estimator, [CodeFeatures, HistoricalChanges], DebtScore) ::
    predicted_debt(Code, DebtScore) :-
    code_features(Code, CodeFeatures),
    change_history(Code, HistoricalChanges).

%% =============================================================================
%% PROBABILISTIC ECO RULES
%% =============================================================================

%% Probabilistic eco-friendliness based on learned patterns
%% P(eco_friendly | carbon_prob, energy_pattern)
P :: eco_friendly_prob(Code) :-
    high_carbon_prob(Code, CarbonP),
    energy_pattern(Code, EnergyClass),
    P is (1 - CarbonP) * energy_efficiency_factor(EnergyClass).

%% Energy efficiency factors (can be learned)
energy_efficiency_factor(efficient) := 0.9.
energy_efficiency_factor(moderate) := 0.6.
energy_efficiency_factor(inefficient) := 0.3.
energy_efficiency_factor(unknown) := 0.5.

%% =============================================================================
%% PROBABILISTIC PARETO RULES
%% =============================================================================

%% Probability that a change will improve Pareto position
%% Learned from historical refactoring outcomes
P :: pareto_improvement_likely(Code, ChangeType) :-
    dominated(Code),
    refactor_success_prob(Code, ChangeType, P),
    P > 0.6.

%% Probability of maintaining Pareto optimality after change
P :: maintains_pareto(Code, ChangeType) :-
    pareto_optimal(Code),
    refactor_success_prob(Code, ChangeType, BaseP),
    % Penalty for changes to already optimal code
    P is BaseP * 0.8.

%% =============================================================================
%% LEARNING FROM PRACTICE (Praxis Loop)
%% =============================================================================

%% Evidence from past refactoring outcomes
%% These facts are updated as we observe real outcomes
observed_improvement(code_id_1, carbon_reduction, 0.15).
observed_improvement(code_id_1, energy_reduction, 0.20).
observed_no_improvement(code_id_2, complexity_reduction).

%% Learn policy effectiveness
%% P(policy_effective | policy, outcomes)
P :: policy_effective(Policy) :-
    policy_application(Policy, Code, Outcome),
    outcome_positive(Outcome),
    policy_success_rate(Policy, P).

%% Update success rates based on observations (simplified)
policy_success_rate(Policy, Rate) :-
    findall(1, (policy_application(Policy, _, positive)), Successes),
    findall(1, (policy_application(Policy, _, _)), Total),
    length(Successes, S),
    length(Total, T),
    T > 0,
    Rate is S / T.

%% =============================================================================
%% RECOMMENDATION CONFIDENCE
%% =============================================================================

%% Confidence in refactoring recommendations
%% Combines rule-based reasoning with learned patterns
confidence(recommendation(Code, Action, Reason), Confidence) :-
    base_confidence(Reason, BaseConf),
    refactor_success_prob(Code, Action, SuccessProb),
    Confidence is BaseConf * SuccessProb.

base_confidence(eco_improvement, 0.8).
base_confidence(debt_reduction, 0.7).
base_confidence(quality_improvement, 0.75).
base_confidence(pareto_optimization, 0.65).

%% =============================================================================
%% ADAPTIVE THRESHOLDS
%% =============================================================================

%% Thresholds that adapt based on project context and history
%% Start with defaults, adjust based on outcomes

adaptive_threshold(eco_minimum, carbon, Threshold) :-
    project_baseline(carbon, Baseline),
    learned_improvement_rate(carbon, Rate),
    % Gradually increase expectations
    Threshold is max(50, Baseline * (1 + Rate)).

adaptive_threshold(eco_minimum, energy, Threshold) :-
    project_baseline(energy, Baseline),
    learned_improvement_rate(energy, Rate),
    Threshold is max(50, Baseline * (1 + Rate)).

%% Default thresholds when no history
adaptive_threshold(eco_minimum, carbon, 50) :- \+ project_baseline(carbon, _).
adaptive_threshold(eco_minimum, energy, 50) :- \+ project_baseline(energy, _).

%% =============================================================================
%% KNOWLEDGE GRAPH INTEGRATION
%% =============================================================================

%% Query patterns for Virtuoso (RDF) integration
sparql_query(eco_best_practices, "
    PREFIX eco: <http://oikos-bot.dev/ontology#>
    PREFIX sw: <http://schema.org/SoftwareSourceCode>

    SELECT ?practice ?description ?impact
    WHERE {
        ?practice a eco:BestPractice ;
                  eco:description ?description ;
                  eco:carbonImpact ?impact .
        FILTER (?impact > 0.1)
    }
    ORDER BY DESC(?impact)
").

%% Query patterns for ArangoDB (graph) integration
aql_query(dependency_impact, "
    FOR v, e, p IN 1..3 OUTBOUND @startNode GRAPH 'code_dependencies'
        LET impact = SUM(p.vertices[*].carbon_score)
        RETURN { path: p, total_impact: impact }
").

%% =============================================================================
%% COACHING AND SUGGESTIONS
%% =============================================================================

%% Generate coaching suggestions with confidence levels
P :: coaching_suggestion(Code, Suggestion, Priority) :-
    needs_attention(Code, Reason),
    suggestion_for(Reason, Suggestion),
    priority_for(Reason, Priority),
    confidence(recommendation(Code, Suggestion, Reason), P),
    P > 0.5.

suggestion_for(high_carbon, "Consider memoization for repeated computations").
suggestion_for(high_carbon, "Evaluate algorithm complexity - can you reduce from O(n^2)?").
suggestion_for(energy_inefficient, "Replace busy-waiting with event-driven patterns").
suggestion_for(energy_inefficient, "Use connection pooling instead of creating new connections").
suggestion_for(high_debt, "Extract duplicated logic into shared functions").
suggestion_for(low_coverage, "Add tests for edge cases in critical paths").

priority_for(high_carbon, high) :- !.
priority_for(energy_inefficient, high) :- !.
priority_for(high_debt, medium) :- !.
priority_for(low_coverage, medium) :- !.
priority_for(_, low).

%% =============================================================================
%% TRAINING DATA GENERATION
%% =============================================================================

%% Generate training examples for neural networks
%% Format: input features -> expected output
training_example(carbon_estimator, Features, Label) :-
    historical_analysis(Code, Features, carbon_score, ActualScore),
    (ActualScore < 40 -> Label = high ; Label = normal).

training_example(refactor_predictor, [Features, Action], Label) :-
    historical_refactor(Code, Action, Outcome),
    code_features(Code, Features),
    (Outcome = success -> Label = 1.0 ; Label = 0.0).
