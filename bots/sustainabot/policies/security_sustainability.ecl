// SPDX-License-Identifier: PMPL-1.0-or-later
// SustainaBot Policy: Security-Sustainability Correlation
//
// Correlates panic-attack findings with eco scores.
// Functions that are BOTH high-energy AND have security weak points
// get elevated severity.

def is_high_energy(energy_joules: Float) -> Bool
    @requires: energy < 0.05J
{
    energy_joules > 50.0
}

def has_security_issues(weak_point_count: Float) -> Bool
    @requires: energy < 0.05J
{
    weak_point_count > 0.0
}

def correlate(energy: Float, weak_points: Float) -> Bool
    @requires: energy < 0.5J
{
    is_high_energy(energy) && has_security_issues(weak_points)
}
