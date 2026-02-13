// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2024-2025 hyperpolymath
//
// ArangoDB Schema for Oikos Bot
// This script creates the database schema for storing code analysis results,
// dependency graphs, and praxis observations.
//
// Run with: arangosh --server.endpoint tcp://localhost:8529 < schema.js

// Database name
const dbName = "oikos";

// Create database if not exists
try {
  db._createDatabase(dbName);
  console.log(`Created database: ${dbName}`);
} catch (e) {
  if (e.errorNum !== 1207) { // 1207 = database already exists
    throw e;
  }
  console.log(`Database ${dbName} already exists`);
}

db._useDatabase(dbName);

// =============================================================================
// DOCUMENT COLLECTIONS
// =============================================================================

// Repositories being analyzed
if (!db._collection("repositories")) {
  db._create("repositories");
  db.repositories.ensureIndex({ type: "hash", fields: ["owner", "name"], unique: true });
  db.repositories.ensureIndex({ type: "hash", fields: ["platform"] });
  console.log("Created collection: repositories");
}

// Analysis results
if (!db._collection("analyses")) {
  db._create("analyses");
  db.analyses.ensureIndex({ type: "hash", fields: ["repository_id"] });
  db.analyses.ensureIndex({ type: "hash", fields: ["commit_sha"] });
  db.analyses.ensureIndex({ type: "persistent", fields: ["timestamp"] });
  db.analyses.ensureIndex({ type: "hash", fields: ["grade"] });
  console.log("Created collection: analyses");
}

// Code entities (files, functions, modules)
if (!db._collection("entities")) {
  db._create("entities");
  db.entities.ensureIndex({ type: "hash", fields: ["repository_id"] });
  db.entities.ensureIndex({ type: "hash", fields: ["type"] });
  db.entities.ensureIndex({ type: "fulltext", fields: ["path"], minLength: 3 });
  console.log("Created collection: entities");
}

// Metrics snapshots
if (!db._collection("metrics")) {
  db._create("metrics");
  db.metrics.ensureIndex({ type: "hash", fields: ["entity_id"] });
  db.metrics.ensureIndex({ type: "hash", fields: ["analysis_id"] });
  db.metrics.ensureIndex({ type: "persistent", fields: ["carbon_score"] });
  db.metrics.ensureIndex({ type: "persistent", fields: ["energy_score"] });
  db.metrics.ensureIndex({ type: "persistent", fields: ["quality_score"] });
  console.log("Created collection: metrics");
}

// Policy violations
if (!db._collection("violations")) {
  db._create("violations");
  db.violations.ensureIndex({ type: "hash", fields: ["entity_id"] });
  db.violations.ensureIndex({ type: "hash", fields: ["analysis_id"] });
  db.violations.ensureIndex({ type: "hash", fields: ["policy"] });
  db.violations.ensureIndex({ type: "hash", fields: ["severity"] });
  console.log("Created collection: violations");
}

// Recommendations
if (!db._collection("recommendations")) {
  db._create("recommendations");
  db.recommendations.ensureIndex({ type: "hash", fields: ["entity_id"] });
  db.recommendations.ensureIndex({ type: "hash", fields: ["action"] });
  db.recommendations.ensureIndex({ type: "persistent", fields: ["confidence"] });
  console.log("Created collection: recommendations");
}

// Praxis observations (learning from practice)
if (!db._collection("observations")) {
  db._create("observations");
  db.observations.ensureIndex({ type: "hash", fields: ["entity_id"] });
  db.observations.ensureIndex({ type: "hash", fields: ["action_taken"] });
  db.observations.ensureIndex({ type: "hash", fields: ["outcome"] });
  db.observations.ensureIndex({ type: "persistent", fields: ["timestamp"] });
  console.log("Created collection: observations");
}

// Best practices knowledge base
if (!db._collection("best_practices")) {
  db._create("best_practices");
  db.best_practices.ensureIndex({ type: "hash", fields: ["category"] });
  db.best_practices.ensureIndex({ type: "hash", fields: ["domain"] });
  db.best_practices.ensureIndex({ type: "persistent", fields: ["impact"] });
  db.best_practices.ensureIndex({ type: "fulltext", fields: ["description"], minLength: 3 });
  console.log("Created collection: best_practices");
}

// =============================================================================
// EDGE COLLECTIONS (for graph relationships)
// =============================================================================

// Code dependencies
if (!db._collection("depends_on")) {
  db._createEdgeCollection("depends_on");
  console.log("Created edge collection: depends_on");
}

// Code containment (file contains function, module contains class)
if (!db._collection("contains")) {
  db._createEdgeCollection("contains");
  console.log("Created edge collection: contains");
}

// Pareto dominance relationships
if (!db._collection("dominates")) {
  db._createEdgeCollection("dominates");
  console.log("Created edge collection: dominates");
}

// Similarity relationships
if (!db._collection("similar_to")) {
  db._createEdgeCollection("similar_to");
  console.log("Created edge collection: similar_to");
}

// =============================================================================
// GRAPH DEFINITIONS
// =============================================================================

// Code dependency graph
const graphName = "code_dependencies";
try {
  if (!graphs._exists(graphName)) {
    graphs._create(graphName, [
      { collection: "depends_on", from: ["entities"], to: ["entities"] },
      { collection: "contains", from: ["entities"], to: ["entities"] }
    ]);
    console.log(`Created graph: ${graphName}`);
  }
} catch (e) {
  console.log(`Graph ${graphName} may already exist`);
}

// Pareto frontier graph
const paretoGraphName = "pareto_frontier";
try {
  if (!graphs._exists(paretoGraphName)) {
    graphs._create(paretoGraphName, [
      { collection: "dominates", from: ["entities"], to: ["entities"] }
    ]);
    console.log(`Created graph: ${paretoGraphName}`);
  }
} catch (e) {
  console.log(`Graph ${paretoGraphName} may already exist`);
}

// =============================================================================
// VIEWS (for search)
// =============================================================================

// Full-text search view for code entities
try {
  db._createView("entities_search", "arangosearch", {
    links: {
      entities: {
        includeAllFields: true,
        analyzers: ["text_en"],
        storeValues: "id"
      }
    }
  });
  console.log("Created view: entities_search");
} catch (e) {
  console.log("View entities_search may already exist");
}

// =============================================================================
// SAMPLE DATA
// =============================================================================

// Insert sample best practices
const samplePractices = [
  {
    _key: "connection_pooling",
    name: "Use connection pooling",
    category: "resource_efficiency",
    domain: "database",
    description: "Reuse database connections instead of creating new ones for each request",
    impact: 0.15,
    carbon_reduction: 0.10,
    energy_reduction: 0.15,
    tags: ["database", "performance", "resources"]
  },
  {
    _key: "lazy_evaluation",
    name: "Implement lazy evaluation",
    category: "computation_efficiency",
    domain: "general",
    description: "Defer computation until results are actually needed",
    impact: 0.12,
    carbon_reduction: 0.08,
    energy_reduction: 0.12,
    tags: ["performance", "memory", "computation"]
  },
  {
    _key: "event_driven",
    name: "Use event-driven patterns",
    category: "energy_efficiency",
    domain: "general",
    description: "Replace polling with event-driven or reactive patterns",
    impact: 0.20,
    carbon_reduction: 0.15,
    energy_reduction: 0.25,
    tags: ["async", "performance", "energy"]
  },
  {
    _key: "memoization",
    name: "Implement memoization",
    category: "computation_efficiency",
    domain: "general",
    description: "Cache results of expensive function calls with same inputs",
    impact: 0.18,
    carbon_reduction: 0.12,
    energy_reduction: 0.18,
    tags: ["caching", "performance", "computation"]
  },
  {
    _key: "batch_operations",
    name: "Batch I/O operations",
    category: "io_efficiency",
    domain: "general",
    description: "Combine multiple small I/O operations into larger batches",
    impact: 0.16,
    carbon_reduction: 0.10,
    energy_reduction: 0.20,
    tags: ["io", "performance", "batching"]
  }
];

for (const practice of samplePractices) {
  try {
    db.best_practices.insert(practice);
    console.log(`Inserted best practice: ${practice.name}`);
  } catch (e) {
    if (e.errorNum !== 1210) { // 1210 = unique constraint violated
      console.log(`Best practice ${practice._key} may already exist`);
    }
  }
}

console.log("\nSchema creation complete!");
console.log("Collections:", db._collections().map(c => c.name()).join(", "));
