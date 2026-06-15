<script lang="ts">
    import { range } from "@gbagan/utils";
  import { isPaletteBlock, isRegisterBlock, type DraggedBlock } from "../lib/types";

  const LABEL = {
    input: "input",
    output: "output",
    jump: "jump",
    "jump-if-zero": "jump if zero",
    "jump-if-negative": "jump if < 0",
    "copy-from": "copy from",
    "copy-to": "copy to",
    "add": "add",
    "sub": "sub"
  }

  type Props = {
    block: DraggedBlock;
    registerCount?: number;
    registerPopupBlockId?: string | null;
    openRegisterPopup?: (id: string) => void;
    onRegisterClick?: (id: string, index: number) => void;
  }

  let { block, registerCount = 0, registerPopupBlockId = null,
    openRegisterPopup, onRegisterClick }: Props = $props();
</script>

{#if isPaletteBlock(block)}
  <div class="palette-block" data-type={block.type}>
    {LABEL[block.type]}
  </div>
{:else}
  <div
    class:program-block={block.kind === "instruction"}
    class:jump-target-block={block.kind === "jump-target"}
    class:has-open-popup={registerPopupBlockId !== undefined}
    data-block-id={block.id}
    data-type={block.kind === "instruction" ? block.type : ""}
  >
    <div class="block-label">
        {#if block.kind === "instruction"}
          <strong>{LABEL[block.type]}</strong>
          {#if isRegisterBlock(block)}
            <button
              class="register-badge"
              type="button"
              onclick={() => openRegisterPopup?.(block.id)}
              aria-label="Changer le numéro de registre"
            >
              {block.register ?? 0}
            </button>
          {/if}
        {/if}
    </div>
    {#if block.kind === "instruction" 
      && isRegisterBlock(block)
      && registerPopupBlockId === block.id
    }
      <div class="register-popup">
        {#each range(0, registerCount) as i}
          <button
            type="button"
            class:selected-register={block.register === i}
            onclick={() => onRegisterClick?.(block.id, i)}
          >
            {i}
          </button>
        {/each}
      </div>
    {/if}
  </div>
{/if}

<style>
  .block-label {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    min-width: 0;
  }

  .palette-block {
    border-radius: 10px;
    width: 7rem;
    height: 2.5rem;
    border: 2px solid #bbb;
    cursor: grab;
    user-select: none;
    font-weight: 600;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .program-block,
  .jump-target-block {
    cursor: grab;
    position: relative;
    z-index: 2;
    border-radius: 10px;
    box-shadow: 0 2px 6px rgb(0 0 0 / 0.12);
  }

  .program-block {
    width: 8rem;
    height: 2.5rem;
    border: 2px solid #ccc;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .program-block.has-open-popup {
    z-index: 100;
  }

  .palette-block:active,
  .program-block:active,
  .jump-target-block:active  {
    cursor: grabbing;
  }

  .program-block[data-type="input"],
  .palette-block[data-type="input"],
  .program-block[data-type="output"],
  .palette-block[data-type="output"] {
    background: var(--io-bg);
    border-color: var(--io-border);
    color: var(--io-color);
  }

  .program-block[data-type="jump"],
  .palette-block[data-type="jump"],
  .program-block[data-type="jump-if-zero"],
  .palette-block[data-type="jump-if-zero"] {
    background: var(--jump-bg);
    border-color: var(--jump-border);
    color: var(--jump-color);
  }

  .program-block[data-type="add"],
  .palette-block[data-type="add"],
  .program-block[data-type="sub"],
  .palette-block[data-type="sub"] {
    background: var(--arith-bg);
    border-color: var(--arith-border);
    color: var(--arith-color);
  }

  .program-block[data-type="copy-from"],
  .palette-block[data-type="copy-from"],
  .program-block[data-type="copy-to"],
  .palette-block[data-type="copy-to"] {
    background: var(--mem-bg);
    border-color: var(--mem-border);
    color: var(--mem-color);
  }

  .jump-target-block {
    width: 6rem;
    height: 2rem;
    margin-left: 1rem;
    border: 2px dashed #777;
    background: linear-gradient(135deg, #f8fafc, #e2e8f0);
    border-color: #94a3b8;
    color: #334155;
  }

   .register-badge {
    min-width: 1.8rem;
    height: 1.8rem;
    padding: 0 0.45rem;
    border: 2px solid rgb(0 0 0 / 0.18);
    border-radius: 999px;
    background: rgb(255 255 255 / 0.75);
    color: inherit;
    font-weight: 700;
    cursor: pointer;
    line-height: 1;
  }

  .register-badge:hover {
    background: white;
    transform: scale(1.06);
  }

  .register-popup {
    position: absolute;
    z-index: 20;
    top: 2.7rem;
    left: 3rem;
    display: grid;
    grid-template-columns: repeat(4, 2rem);
    gap: 0.35rem;
    padding: 0.5rem;
    border: 2px solid rgb(0 0 0 / 0.15);
    border-radius: 0.75rem;
    background: white;
    box-shadow: 0 8px 20px rgb(0 0 0 / 0.18);
  }

  .register-popup button {
    width: 2rem;
    height: 2rem;
    border: 1px solid #ccc;
    border-radius: 0.5rem;
    background: #f8fafc;
    cursor: pointer;
    font-weight: 700;
  }

  .register-popup button:hover {
    background: #e2e8f0;
  }

  .register-popup button.selected-register {
    border-color: #2563eb;
    background: #dbeafe;
    color: #1e3a8a;
  }
</style>