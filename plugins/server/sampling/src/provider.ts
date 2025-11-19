/**
 * MCP Sampling Provider Abstraction
 *
 * This module provides an isolated, generic, reusable sampling infrastructure
 * for MCP protocol-based sampling. Implementations use the SDK's createMessage()
 * method to send sampling requests via the protocol.
 */

import type { SamplingRequestOptions, SamplingResult } from "./types.js";

/**
 * Core abstraction for sampling functionality
 *
 * This interface decouples sampling from specific server implementations,
 * making it testable, mockable, and reusable across different MCP servers.
 */
export interface SamplingProvider {
  /**
   * Check if sampling is currently available
   *
   * @returns true if sampling requests can be made, false otherwise
   */
  isAvailable(): boolean;

  /**
   * Request sampling from the client
   *
   * @param options - Sampling request parameters
   * @returns Promise resolving to sampling result
   * @throws Never throws - returns error result instead
   */
  requestSampling(options: SamplingRequestOptions): Promise<SamplingResult>;
}

/**
 * No-op sampling provider used when sampling is not available
 *
 * This is the default fallback when the client doesn't support sampling
 * or before the provider is initialized. It always reports unavailable
 * and returns appropriate error results.
 */
export class NoOpSamplingProvider implements SamplingProvider {
  isAvailable(): boolean {
    return false;
  }

  async requestSampling(
    _options: SamplingRequestOptions
  ): Promise<SamplingResult> {
    console.warn("[NoOpSamplingProvider] Sampling requested but not available");
    return {
      success: false,
      error: "Sampling is not available - client may not support it or provider not initialized",
    };
  }
}
