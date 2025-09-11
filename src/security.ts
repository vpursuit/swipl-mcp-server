import path from "path";
import os from "os";

/**
 * Security module for SWI-Prolog MCP Server
 * Provides file path validation and security error handling
 */

// The only allowed directory for file operations
const ALLOWED_DIR = path.join(os.homedir(), '.swipl-mcp-server');

// System directories that are always blocked
const SYSTEM_DIRECTORIES = [
  '/etc',
  '/usr',
  '/bin',
  '/sbin',
  '/var',
  '/sys',
  '/proc',
  '/boot',
  '/dev',
  '/root'
];

export interface SecurityError {
  type: 'file_path_violation' | 'dangerous_operation';
  message: string;
  blockedPath?: string;
  dangerousPredicate?: string;
}

/**
 * Validates if a file path is allowed for loading
 * Only files within ~/.swipl-mcp-server/ are permitted
 */
export function validateFilePath(filename: string): { allowed: boolean; error?: SecurityError } {
  try {
    const absolutePath = path.resolve(filename);
    const normalizedPath = path.normalize(absolutePath);
    
    // Check if path is within allowed directory
    const relativePath = path.relative(ALLOWED_DIR, normalizedPath);
    const isWithinAllowed = !relativePath.startsWith('..') && !path.isAbsolute(relativePath);
    
    if (isWithinAllowed) {
      return { allowed: true };
    }
    
    // Check if trying to access system directories
    const isSystemPath = SYSTEM_DIRECTORIES.some(sysDir => 
      normalizedPath.startsWith(sysDir + '/') || normalizedPath === sysDir
    );
    
    if (isSystemPath) {
      return {
        allowed: false,
        error: {
          type: 'file_path_violation',
          message: `Security Error: Access to system directories is blocked. Files can only be loaded from ${ALLOWED_DIR}`,
          blockedPath: normalizedPath
        }
      };
    }
    
    // General path violation
    return {
      allowed: false,
      error: {
        type: 'file_path_violation',
        message: `Security Error: Files can only be loaded from ${ALLOWED_DIR}`,
        blockedPath: normalizedPath
      }
    };
    
  } catch (error) {
    return {
      allowed: false,
      error: {
        type: 'file_path_violation',
        message: `Security Error: Invalid file path`,
        blockedPath: filename
      }
    };
  }
}

/**
 * Detects if an error message indicates a dangerous operation was blocked
 */
export function detectDangerousOperation(errorMessage: string): SecurityError | null {
  const message = errorMessage.toLowerCase();
  
  // Detect dangerous predicates in error messages
  const dangerousPredicates = [
    'call', 'assert', 'assertz', 'asserta', 'retract', 'retractall', 
    'abolish', 'system', 'shell', 'halt'
  ];
  
  for (const predicate of dangerousPredicates) {
    if (message.includes(predicate) && message.includes('unsafe')) {
      return {
        type: 'dangerous_operation',
        message: `Security Error: Operation blocked - contains dangerous predicate '${predicate}'`,
        dangerousPredicate: predicate
      };
    }
  }
  
  // Check for timeout that might be a security violation - this is the main issue
  if (message.includes('timeout')) {
    for (const predicate of dangerousPredicates) {
      if (errorMessage.includes(predicate)) {
        return {
          type: 'dangerous_operation',
          message: `Security Error: Operation blocked - contains dangerous predicate '${predicate}'`,
          dangerousPredicate: predicate
        };
      }
    }
  }
  
  return null;
}

/**
 * Checks if a fact/clause contains dangerous predicates before assertion
 */
export function validateFactSafety(fact: string): { safe: boolean; error?: SecurityError } {
  const lowerFact = fact.toLowerCase();
  
  const dangerousPredicates = [
    'call', 'assert', 'assertz', 'asserta', 'retract', 'retractall', 
    'abolish', 'system', 'shell', 'halt'
  ];
  
  for (const predicate of dangerousPredicates) {
    if (lowerFact.includes(predicate + '(')) {
      return {
        safe: false,
        error: {
          type: 'dangerous_operation',
          message: `Security Error: Operation blocked - contains dangerous predicate '${predicate}'`,
          dangerousPredicate: predicate
        }
      };
    }
  }
  
  return { safe: true };
}

/**
 * Gets the allowed directory path
 */
export function getAllowedDirectory(): string {
  return ALLOWED_DIR;
}