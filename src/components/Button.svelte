<script lang="ts">
  import type { Snippet } from "svelte";

  type Props = {
    variant?: "green" | "yellow" | "red" | "blue" | "gray";
    disabled?: boolean;
    onclick?: () => void;
    children?: Snippet;
  }

  let { variant = "green", disabled = false, onclick, children }: Props = $props();
</script>

<button class="button-{variant}" {disabled} {onclick}>
  {@render children?.()}
</button>

<style>
  button {
    --button-bg: #e2e8f0;
    --button-bg-2: #cbd5e1;
    --button-border: #94a3b8;
    --button-text: #0f172a;
    --button-shadow: rgb(15 23 42 / 0.18);

    min-width: 8rem;
    padding: 0.75rem 1.15rem;

    border: 2px solid var(--button-border);
    border-radius: 999px;

    background: linear-gradient(135deg, var(--button-bg), var(--button-bg-2));
    color: var(--button-text);

    font-size: 1rem;
    font-weight: 800;
    letter-spacing: 0.02em;

    cursor: pointer;
    user-select: none;

    box-shadow:
      inset 0 -4px 0 rgb(0 0 0 / 0.12),
      0 6px 14px var(--button-shadow);

    transition:
      transform 120ms ease,
      box-shadow 120ms ease,
      filter 120ms ease;
  }

  button:hover:not(:disabled) {
    transform: translateY(-2px);
    filter: brightness(1.04);
    box-shadow:
      inset 0 -4px 0 rgb(0 0 0 / 0.12),
      0 10px 20px var(--button-shadow);
  }

  button:active:not(:disabled) {
    transform: translateY(1px);
    box-shadow:
      inset 0 -2px 0 rgb(0 0 0 / 0.16),
      0 3px 8px var(--button-shadow);
  }

  button:disabled {
    opacity: 0.45;
    cursor: default;
    filter: grayscale(0.25);
  }

  button.button-green {
    --button-bg: var(--green-100);
    --button-bg-2: var(--green-300);
    --button-border: var(--green-500);
    --button-text: var(--green-900);
    --button-shadow: color-mix(in srgb, var(--green-500) 28%, transparent);
  }

  button.button-yellow {
    --button-bg: var(--amber-100);
    --button-bg-2: var(--amber-300);
    --button-border: var(--amber-500);
    --button-text: var(--amber-900);
    --button-shadow: color-mix(in srgb, var(--amber-500) 28%, transparent);
  }

  button.button-red {
    --button-bg: var(--red-100);
    --button-bg-2: var(--red-300);
    --button-border: var(--red-500);
    --button-text: var(--red-900);
    --button-shadow: color-mix(in srgb, var(--red-500) 28%, transparent);
  }

  button.button-blue {
    --button-bg: var(--blue-100);
    --button-bg-2: var(--blue-300);
    --button-border: var(--blue-500);
    --button-text: var(--blue-900);
    --button-shadow: color-mix(in srgb, var(--blue-500) 28%, transparent);
  }
</style>