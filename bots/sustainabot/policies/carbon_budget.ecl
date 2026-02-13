// SPDX-License-Identifier: PMPL-1.0-or-later
// SustainaBot Policy: Carbon Budget
//
// Per-function and per-project carbon budgets.
// Enforces sustainable computation practices.

def exceeds_function_carbon(carbon_grams: Float) -> Bool
    @requires: energy < 0.1J
{
    carbon_grams > 0.5
}

def exceeds_project_carbon(total_carbon_grams: Float) -> Bool
    @requires: energy < 0.1J
{
    total_carbon_grams > 5.0
}

def evaluate_carbon_budget(function_carbon: Float, total_carbon: Float) -> Bool
    @requires: energy < 1J, carbon < 0.001gCO2e
{
    exceeds_function_carbon(function_carbon) || exceeds_project_carbon(total_carbon)
}
