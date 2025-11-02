/**
 * Validate string input for tool parameters
 */
export function validateStringInput(
  name: string,
  value: unknown,
  maxLength: number
): { ok: true } | { ok: false; error: string; code: string } {
  if (typeof value !== "string" || value.length === 0) {
    return {
      ok: false,
      error: `${name} parameter is required and must be a string`,
      code: "invalid_input"
    };
  }
  if (value.length > maxLength) {
    const label = name.toLowerCase();
    return {
      ok: false,
      error: `${label} too long (max ${maxLength} characters)`,
      code: `${label}_too_long`
    };
  }
  return { ok: true };
}
