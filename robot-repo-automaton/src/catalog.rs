// SPDX-License-Identifier: PMPL-1.0-or-later
//! Error catalog parsing from ERROR-CATALOG.scm

use lexpr::Value;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

use crate::error::{Error, Result};

/// Parsed error catalog
#[derive(Debug, Clone)]
pub struct ErrorCatalog {
    pub metadata: CatalogMetadata,
    pub error_types: Vec<ErrorType>,
}

/// Catalog metadata
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CatalogMetadata {
    pub format_version: String,
    pub schema_version: String,
    pub purpose: String,
    pub generator: Option<String>,
}

/// Single error type definition
#[derive(Debug, Clone)]
pub struct ErrorType {
    pub id: String,
    pub name: String,
    pub severity: Severity,
    pub category: String,
    pub description: String,
    pub detection: Detection,
    pub affected_repos: Vec<String>,
    pub fix: Fix,
    pub commit_message: String,
}

/// Severity levels (ordered from lowest to highest for correct Ord derivation)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    Info,
    Low,
    Medium,
    High,
    Critical,
}

impl Severity {
    fn from_str(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "critical" => Severity::Critical,
            "high" => Severity::High,
            "medium" => Severity::Medium,
            "low" => Severity::Low,
            _ => Severity::Info,
        }
    }
}

/// Detection method for an error type
#[derive(Debug, Clone)]
pub struct Detection {
    pub method: DetectionMethod,
    pub files: Vec<String>,
    pub condition: Option<String>,
    pub extension_map: HashMap<String, String>,
}

/// Detection method types
#[derive(Debug, Clone)]
pub enum DetectionMethod {
    FileExistence,
    LanguageDetection,
    ContentMatch,
    Custom(String),
}

/// Fix action for an error type
#[derive(Debug, Clone)]
pub struct Fix {
    pub action: FixAction,
    pub target: String,
    pub reason: Option<String>,
    pub modification: Option<String>,
    pub fallback: Option<String>,
}

/// Fix action types
#[derive(Debug, Clone)]
pub enum FixAction {
    Delete,
    Modify,
    Create,
    Disable,
}

impl ErrorCatalog {
    /// Parse an error catalog from a file
    pub fn from_file(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)?;
        Self::parse(&content)
    }

    /// Parse an error catalog from a string
    pub fn parse(content: &str) -> Result<Self> {
        let value = lexpr::from_str(content)?;
        Self::from_sexpr(&value)
    }

    /// Convert from S-expression value
    fn from_sexpr(value: &Value) -> Result<Self> {
        // Expect (define error-catalog '(...))
        let _list = value.as_cons()
            .ok_or_else(|| Error::CatalogParse("Expected list at top level".into()))?;

        // Navigate to the catalog content
        let catalog_list = Self::find_catalog_content(value)?;

        let mut metadata = CatalogMetadata {
            format_version: "1.0".to_string(),
            schema_version: "unknown".to_string(),
            purpose: "Error catalog".to_string(),
            generator: None,
        };
        let mut error_types = Vec::new();

        // Parse each item in the catalog
        for item in catalog_list.iter() {
            if let Some(cons) = item.as_cons() {
                let car = cons.car();
                if let Some(sym) = car.as_symbol() {
                    match sym {
                        "metadata" => {
                            metadata = Self::parse_metadata(item)?;
                        }
                        "error-type" => {
                            if let Ok(et) = Self::parse_error_type(item) {
                                error_types.push(et);
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        Ok(ErrorCatalog {
            metadata,
            error_types,
        })
    }

    fn find_catalog_content(value: &Value) -> Result<Vec<&Value>> {
        // Handle (define error-catalog '(...)) structure
        if let Some(cons) = value.as_cons() {
            let items: Vec<&Value> = cons.iter().map(|c| c.car()).collect();
            if items.len() >= 3 {
                if let Some(sym) = items[0].as_symbol() {
                    if sym == "define" {
                        // Third element should be the quoted list
                        if let Some(quoted) = items[2].as_cons() {
                            let quoted_items: Vec<&Value> = quoted.iter().map(|c| c.car()).collect();
                            if !quoted_items.is_empty() {
                                if let Some(quote_sym) = quoted_items[0].as_symbol() {
                                    if quote_sym == "quote" && quoted_items.len() > 1 {
                                        return Self::find_catalog_content(quoted_items[1]);
                                    }
                                }
                                return Ok(quoted_items);
                            }
                        }
                    }
                }
            }
            // Return as list of items
            return Ok(cons.iter().map(|c| c.car()).collect());
        }
        Err(Error::CatalogParse("Could not find catalog content".into()))
    }

    fn parse_metadata(value: &Value) -> Result<CatalogMetadata> {
        let mut meta = CatalogMetadata {
            format_version: "1.0".to_string(),
            schema_version: "unknown".to_string(),
            purpose: "Error catalog".to_string(),
            generator: None,
        };

        if let Some(cons) = value.as_cons() {
            for item in cons.iter().skip(1) {
                if let Some(pair) = item.car().as_cons() {
                    let key = pair.car().as_symbol().unwrap_or("");
                    let val = pair.cdr();
                    match key {
                        "format-version" => {
                            meta.format_version = Self::extract_string(val);
                        }
                        "schema-version" => {
                            meta.schema_version = Self::extract_string(val);
                        }
                        "purpose" => {
                            meta.purpose = Self::extract_string(val);
                        }
                        "generator" => {
                            meta.generator = Some(Self::extract_string(val));
                        }
                        _ => {}
                    }
                }
            }
        }

        Ok(meta)
    }

    fn parse_error_type(value: &Value) -> Result<ErrorType> {
        let mut id = String::new();
        let mut name = String::new();
        let mut severity = Severity::Medium;
        let mut category = String::new();
        let mut description = String::new();
        let mut detection = Detection {
            method: DetectionMethod::FileExistence,
            files: Vec::new(),
            condition: None,
            extension_map: HashMap::new(),
        };
        let mut affected_repos = Vec::new();
        let mut fix = Fix {
            action: FixAction::Delete,
            target: String::new(),
            reason: None,
            modification: None,
            fallback: None,
        };
        let mut commit_message = String::new();

        if let Some(cons) = value.as_cons() {
            for item in cons.iter().skip(1) {
                if let Some(pair) = item.car().as_cons() {
                    let key = pair.car().as_symbol().unwrap_or("");
                    let val = pair.cdr();
                    match key {
                        "id" => id = Self::extract_string(val),
                        "name" => name = Self::extract_string(val),
                        "severity" => severity = Severity::from_str(&Self::extract_string(val)),
                        "category" => category = Self::extract_string(val),
                        "description" => description = Self::extract_string(val),
                        "detection" => detection = Self::parse_detection(val)?,
                        "affected-repos" => affected_repos = Self::extract_string_list(val),
                        "fix" => fix = Self::parse_fix(val)?,
                        "commit-message" => commit_message = Self::extract_string(val),
                        _ => {}
                    }
                }
            }
        }

        Ok(ErrorType {
            id,
            name,
            severity,
            category,
            description,
            detection,
            affected_repos,
            fix,
            commit_message,
        })
    }

    fn parse_detection(value: &Value) -> Result<Detection> {
        let mut detection = Detection {
            method: DetectionMethod::FileExistence,
            files: Vec::new(),
            condition: None,
            extension_map: HashMap::new(),
        };

        if let Some(cons) = value.as_cons() {
            for item in cons.iter() {
                if let Some(pair) = item.car().as_cons() {
                    let key = pair.car().as_symbol().unwrap_or("");
                    let val = pair.cdr();
                    match key {
                        "method" => {
                            let method_str = Self::extract_string(val);
                            detection.method = match method_str.as_str() {
                                "file-existence" => DetectionMethod::FileExistence,
                                "language-detection" => DetectionMethod::LanguageDetection,
                                "content-match" => DetectionMethod::ContentMatch,
                                other => DetectionMethod::Custom(other.to_string()),
                            };
                        }
                        "files" => detection.files = Self::extract_string_list(val),
                        "condition" => detection.condition = Some(Self::extract_string(val)),
                        _ => {}
                    }
                }
            }
        }

        Ok(detection)
    }

    fn parse_fix(value: &Value) -> Result<Fix> {
        let mut fix = Fix {
            action: FixAction::Delete,
            target: String::new(),
            reason: None,
            modification: None,
            fallback: None,
        };

        if let Some(cons) = value.as_cons() {
            for item in cons.iter() {
                if let Some(pair) = item.car().as_cons() {
                    let key = pair.car().as_symbol().unwrap_or("");
                    let val = pair.cdr();
                    match key {
                        "action" => {
                            let action_str = Self::extract_string(val);
                            fix.action = match action_str.as_str() {
                                "delete" => FixAction::Delete,
                                "modify" => FixAction::Modify,
                                "create" => FixAction::Create,
                                "disable" => FixAction::Disable,
                                _ => FixAction::Delete,
                            };
                        }
                        "target" => fix.target = Self::extract_string(val),
                        "reason" => fix.reason = Some(Self::extract_string(val)),
                        "modification" => fix.modification = Some(Self::extract_string(val)),
                        "fallback" => fix.fallback = Some(Self::extract_string(val)),
                        _ => {}
                    }
                }
            }
        }

        Ok(fix)
    }

    fn extract_string(value: &Value) -> String {
        if let Some(s) = value.as_str() {
            s.to_string()
        } else if let Some(sym) = value.as_symbol() {
            sym.to_string()
        } else {
            value.to_string().trim_matches('"').to_string()
        }
    }

    fn extract_string_list(value: &Value) -> Vec<String> {
        let mut result = Vec::new();
        if let Some(cons) = value.as_cons() {
            for item in cons.iter() {
                result.push(Self::extract_string(item.car()));
            }
        }
        result
    }

    /// Get error types by severity
    pub fn by_severity(&self, severity: Severity) -> Vec<&ErrorType> {
        self.error_types
            .iter()
            .filter(|e| e.severity == severity)
            .collect()
    }

    /// Get error type by ID
    pub fn get(&self, id: &str) -> Option<&ErrorType> {
        self.error_types.iter().find(|e| e.id == id)
    }

    /// Check if a repo is affected by any error type
    pub fn errors_for_repo(&self, repo_name: &str) -> Vec<&ErrorType> {
        self.error_types
            .iter()
            .filter(|e| e.affected_repos.iter().any(|r| r == repo_name))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_severity_ordering() {
        assert!(Severity::Critical > Severity::High);
        assert!(Severity::High > Severity::Medium);
        assert!(Severity::Medium > Severity::Low);
        assert!(Severity::Low > Severity::Info);
    }
}
