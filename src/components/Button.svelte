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
    --button-bg: #dcfce7;
    --button-bg-2: #86efac;
    --button-border: #22c55e;
    --button-text: #14532d;
    --button-shadow: rgb(34 197 94 / 0.28);
  }

  button.button-yellow {
    --button-bg: #fef3c7;
    --button-bg-2: #fcd34d;
    --button-border: #f59e0b;
    --button-text: #78350f;
    --button-shadow: rgb(245 158 11 / 0.28);
  }

  button.button-red {
    --button-bg: #fee2e2;
    --button-bg-2: #fca5a5;
    --button-border: #ef4444;
    --button-text: #7f1d1d;
    --button-shadow: rgb(239 68 68 / 0.28);
  }

  button.button-blue {
    --button-bg: #dbeafe;
    --button-bg-2: #93c5fd;
    --button-border: #3b82f6;
    --button-text: #1e3a8a;
    --button-shadow: rgb(59 130 246 / 0.28);
  }
</style>