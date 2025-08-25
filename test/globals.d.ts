/**
 * TypeScript declarations for global test utilities
 */

declare global {
  namespace jest {
    interface Matchers<R> {
      toBeValidPrologResponse(): R;
    }
  }
}

export {};
