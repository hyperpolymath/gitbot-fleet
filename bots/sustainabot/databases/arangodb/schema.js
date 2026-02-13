// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2024-2025 hyperpolymath
/**
 * Oikos Bot ArangoDB Schema Setup
 *
 * This script initializes the ArangoDB collections and graphs
 * for storing code analysis results, dependency graphs, and
 * historical metrics.
 *
 * Run with: arangosh --javascript.execute schema.js
 */

const db = require('@arangodb').db;
const graph_module = require('@arangodb/general-graph');

// =============================================================================
// COLLECTIONS
// =============================================================================

// Document Collections
const documentCollections = [
    {
        name: 'projects',
        schema: {
            rule: {
                type: 'object',
                properties: {
                    name: { type: 'string' },
                    url: { type: 'string' },
                    platform: { type: 'string', enum: ['github', 'gitlab', 'bitbucket'] },
                    default_branch: { type: 'string' },
                    eco_config: { type: 'object' },
                    created_at: { type: 'string', format: 'date-time' },
                    updated_at: { type: 'string', format: 'date-time' }
                },
                required: ['name', 'url', 'platform']
            },
            level: 'moderate',
            message: 'Project document validation failed'
        }
    },
    {
        name: 'files',
        schema: {
            rule: {
                type: 'object',
                properties: {
                    project_id: { type: 'string' },
                    path: { type: 'string' },
                    language: { type: 'string' },
                    size_bytes: { type: 'number' },
                    last_modified: { type: 'string', format: 'date-time' },
                    hash: { type: 'string' }
                },
                required: ['project_id', 'path']
            },
            level: 'moderate'
        }
    },
    {
        name: 'functions',
        schema: {
            rule: {
                type: 'object',
                properties: {
                    file_id: { type: 'string' },
                    name: { type: 'string' },
                    start_line: { type: 'number' },
                    end_line: { type: 'number' },
                    parameters: { type: 'array' },
                    return_type: { type: 'string' },
                    visibility: { type: 'string' }
                },
                required: ['file_id', 'name']
            },
            level: 'moderate'
        }
    },
    {
        name: 'analyses',
        schema: {
            rule: {
                type: 'object',
                properties: {
                    entity_id: { type: 'string' },
                    entity_type: { type: 'string', enum: ['project', 'file', 'function', 'module'] },
                    timestamp: { type: 'string', format: 'date-time' },
                    commit_sha: { type: 'string' },
                    eco_metrics: {
                        type: 'object',
                        properties: {
                            carbon_score: { type: 'number', minimum: 0, maximum: 100 },
                            energy_score: { type: 'number', minimum: 0, maximum: 100 },
                            resource_score: { type: 'number', minimum: 0, maximum: 100 },
                            eco_score: { type: 'number', minimum: 0, maximum: 100 }
                        }
                    },
                    econ_metrics: {
                        type: 'object',
                        properties: {
                            pareto_distance: { type: 'number' },
                            allocation_score: { type: 'number' },
                            debt_score: { type: 'number' },
                            econ_score: { type: 'number' }
                        }
                    },
                    quality_metrics: {
                        type: 'object',
                        properties: {
                            complexity_score: { type: 'number' },
                            coupling_score: { type: 'number' },
                            coverage_score: { type: 'number' },
                            quality_score: { type: 'number' }
                        }
                    },
                    health_index: { type: 'number' },
                    violations: { type: 'array' },
                    recommendations: { type: 'array' }
                },
                required: ['entity_id', 'entity_type', 'timestamp']
            },
            level: 'moderate'
        }
    },
    {
        name: 'policies',
        schema: {
            rule: {
                type: 'object',
                properties: {
                    name: { type: 'string' },
                    type: { type: 'string', enum: ['eco', 'econ', 'quality', 'composite'] },
                    version: { type: 'string' },
                    rules: { type: 'array' },
                    thresholds: { type: 'object' },
                    active: { type: 'boolean' },
                    created_at: { type: 'string', format: 'date-time' }
                },
                required: ['name', 'type', 'version']
            },
            level: 'moderate'
        }
    },
    {
        name: 'praxis_observations',
        schema: {
            rule: {
                type: 'object',
                properties: {
                    entity_id: { type: 'string' },
                    action_taken: { type: 'string' },
                    metrics_before: { type: 'object' },
                    metrics_after: { type: 'object' },
                    outcome: { type: 'string', enum: ['positive', 'negative', 'neutral'] },
                    timestamp: { type: 'string', format: 'date-time' },
                    notes: { type: 'string' }
                },
                required: ['entity_id', 'action_taken', 'outcome', 'timestamp']
            },
            level: 'moderate'
        }
    }
];

// Edge Collections
const edgeCollections = [
    {
        name: 'depends_on',
        description: 'Dependency relationships between code entities'
    },
    {
        name: 'contains',
        description: 'Containment relationships (project->file, file->function)'
    },
    {
        name: 'calls',
        description: 'Function call relationships'
    },
    {
        name: 'imports',
        description: 'Import/require relationships'
    },
    {
        name: 'evolved_from',
        description: 'Historical evolution (for tracking changes over time)'
    }
];

// =============================================================================
// CREATE COLLECTIONS
// =============================================================================

console.log('Creating document collections...');
documentCollections.forEach(col => {
    if (!db._collection(col.name)) {
        db._createDocumentCollection(col.name);
        console.log(`  Created: ${col.name}`);

        // Apply schema validation
        db._collection(col.name).properties({ schema: col.schema });
    } else {
        console.log(`  Exists: ${col.name}`);
    }
});

console.log('\nCreating edge collections...');
edgeCollections.forEach(col => {
    if (!db._collection(col.name)) {
        db._createEdgeCollection(col.name);
        console.log(`  Created: ${col.name}`);
    } else {
        console.log(`  Exists: ${col.name}`);
    }
});

// =============================================================================
// CREATE GRAPHS
// =============================================================================

console.log('\nCreating graphs...');

// Code dependency graph
const codeGraphName = 'code_dependencies';
if (!graph_module._exists(codeGraphName)) {
    graph_module._create(codeGraphName, [
        {
            collection: 'depends_on',
            from: ['files', 'functions'],
            to: ['files', 'functions']
        },
        {
            collection: 'calls',
            from: ['functions'],
            to: ['functions']
        },
        {
            collection: 'imports',
            from: ['files'],
            to: ['files']
        }
    ], [
        'projects'
    ]);
    console.log(`  Created: ${codeGraphName}`);
} else {
    console.log(`  Exists: ${codeGraphName}`);
}

// Project structure graph
const structureGraphName = 'project_structure';
if (!graph_module._exists(structureGraphName)) {
    graph_module._create(structureGraphName, [
        {
            collection: 'contains',
            from: ['projects', 'files'],
            to: ['files', 'functions']
        }
    ]);
    console.log(`  Created: ${structureGraphName}`);
} else {
    console.log(`  Exists: ${structureGraphName}`);
}

// Evolution graph (for praxis tracking)
const evolutionGraphName = 'code_evolution';
if (!graph_module._exists(evolutionGraphName)) {
    graph_module._create(evolutionGraphName, [
        {
            collection: 'evolved_from',
            from: ['analyses', 'files', 'functions'],
            to: ['analyses', 'files', 'functions']
        }
    ]);
    console.log(`  Created: ${evolutionGraphName}`);
} else {
    console.log(`  Exists: ${evolutionGraphName}`);
}

// =============================================================================
// CREATE INDEXES
// =============================================================================

console.log('\nCreating indexes...');

// Projects indexes
db.projects.ensureIndex({ type: 'hash', fields: ['url'], unique: true });
db.projects.ensureIndex({ type: 'hash', fields: ['platform'] });

// Files indexes
db.files.ensureIndex({ type: 'hash', fields: ['project_id', 'path'], unique: true });
db.files.ensureIndex({ type: 'hash', fields: ['language'] });
db.files.ensureIndex({ type: 'hash', fields: ['hash'] });

// Functions indexes
db.functions.ensureIndex({ type: 'hash', fields: ['file_id', 'name'] });

// Analyses indexes
db.analyses.ensureIndex({ type: 'hash', fields: ['entity_id'] });
db.analyses.ensureIndex({ type: 'skiplist', fields: ['timestamp'] });
db.analyses.ensureIndex({ type: 'hash', fields: ['commit_sha'] });
db.analyses.ensureIndex({ type: 'skiplist', fields: ['health_index'] });

// Praxis observations indexes
db.praxis_observations.ensureIndex({ type: 'hash', fields: ['entity_id'] });
db.praxis_observations.ensureIndex({ type: 'skiplist', fields: ['timestamp'] });
db.praxis_observations.ensureIndex({ type: 'hash', fields: ['outcome'] });

console.log('  Indexes created successfully');

// =============================================================================
// EXAMPLE QUERIES
// =============================================================================

console.log('\n=== Example AQL Queries ===\n');

console.log('// Find all eco hotspots in a project');
console.log(`
FOR a IN analyses
    FILTER a.eco_metrics.eco_score < 50
    SORT a.eco_metrics.eco_score ASC
    RETURN {
        entity: a.entity_id,
        eco_score: a.eco_metrics.eco_score,
        carbon: a.eco_metrics.carbon_score,
        recommendations: a.recommendations
    }
`);

console.log('\n// Trace dependency impact (propagate eco scores)');
console.log(`
FOR v, e, p IN 1..5 OUTBOUND @startEntity GRAPH 'code_dependencies'
    LET analysis = FIRST(
        FOR a IN analyses
            FILTER a.entity_id == v._key
            SORT a.timestamp DESC
            LIMIT 1
            RETURN a
    )
    RETURN {
        path: p.vertices[*]._key,
        depth: LENGTH(p.edges),
        eco_impact: analysis.eco_metrics.eco_score
    }
`);

console.log('\n// Track praxis learning outcomes');
console.log(`
FOR obs IN praxis_observations
    COLLECT action = obs.action_taken,
            outcome = obs.outcome
    WITH COUNT INTO count
    RETURN {
        action: action,
        outcome: outcome,
        count: count,
        success_rate: outcome == 'positive' ? count : 0
    }
`);

console.log('\nSchema setup complete!');
