/**
 * Type definitions for mcp-prolog package
 */

/**
 * Session state for Prolog interface
 */
export type SessionState =
  | "idle"
  | "query"
  | "query_completed"
  | "engine"
  | "engine_completed";

/**
 * Source entry for preserving original clause text
 */
export interface SourceEntry {
  /** Unique identifier (session-scoped) */
  id: string;
  /** Original input text with variable names preserved */
  sourceText: string;
  /** Origin: direct assertion or file import */
  type: 'inline' | 'file';
  /** File path (if type='file') */
  file?: string;
  /** When added (Date.now()) */
  timestamp: number;
  /** Successfully asserted to Prolog? */
  compiled: boolean;
}

/**
 * Server capabilities summary structure
 */
export interface CapabilitiesSummary {
  [key: string]: unknown; // Allow index signature for structuredContent compatibility
  server: {
    name: string;
    version: string;
  };
  branding: {
    logo: {
      uri: string;
      format: string;
      description: string;
    };
  };
  modes: readonly ["standard", "engine"];
  predicates: {
    standard_prolog: string;
    clpfd_available: boolean;
    clpfd_note: string;
  };
  tools: {
    core: readonly string[];
    knowledge_base: readonly string[];
    query: readonly string[];
    analysis: readonly string[];
  };
  prompts: {
    domain_examples: readonly string[];
  };
  security: {
    module: string;
    file_restrictions: {
      allowed_directory: string;
      blocked_directories: readonly string[];
      validation: string;
    };
    consult: string;
    model: string;
    dangerous_predicate_blocking: {
      detection: string;
      blocked_predicates: readonly string[];
      error_format: string;
    };
    sandbox_validation: string;
    user_predicates: string;
    safe_categories: readonly string[];
    blocked_categories: readonly string[];
  };
  available_libraries: {
    note: string;
    safe_libraries: readonly string[];
    description: string;
  };
}
