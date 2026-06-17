<script lang="ts">
  import { range } from "@gbagan/utils";
  import { isPaletteBlock, isRegisterBlock, type DraggedBlock } from "../lib/types";

  const LABEL = {
    input: "input",
    output: "output",
    jump: "jump",
    "jump-if-zero": "jump if zero",
    "jump-if-negative": "jump if < 0",
    "copy-from": "get",
    "copy-to": "put",
    "add": "add",
    "sub": "sub",
    "inc": "inc",
    "dec": "dec",
  }

  type Props = {
    block: DraggedBlock;
    registerCount?: number;
    registerPopupPosition?: "below" | "above";
    registerPopupBlockId?: string | null;
    openRegisterPopup?: (id: string) => void;
    onRegisterClick?: (id: string, index: number) => void;
    toggleIndirect?: () => void;
  }

  let { block, registerCount = 0, registerPopupBlockId = null, registerPopupPosition = "below",
    openRegisterPopup, onRegisterClick, toggleIndirect }: Props = $props();
</script>

{#if isPaletteBlock(block)}
  <div class="palette-block" data-type={block.type}>
    {LABEL[block.type]}
  </div>
{:else}
  <div
    class:program-block={block.kind === "instruction"}
    class:jump-target-block={block.kind === "jump-target"}
    class:has-open-popup={registerPopupBlockId !== null}
    data-block-id={block.id}
    data-type={block.kind === "instruction" ? block.type : ""}
  >
    <div class="block-label">
      {#if block.kind === "instruction"}
        <strong>{LABEL[block.type]}</strong>
        {#if isRegisterBlock(block)}
          <div class="register-controls">
            {#if block.indirect !== undefined}
              <button
                class="indirect-button"
                class:indirect-enabled={block.indirect}
                type="button"
                onclick={event => {
                  event.stopPropagation();
                  toggleIndirect?.();
                }}
                aria-label={block.indirect ? "Indirection activée" : "Indirection désactivée"}
              >
                {block.indirect ? "*" : ""}
                <span class="indirect-tooltip">
                  {block.indirect ? "Indirection activée" : "Indirection désactivée"}
                </span>
              </button>
            {/if}
            <button
              class="register-badge"
              type="button"
              onclick={() => openRegisterPopup?.(block.id)}
              aria-label="Changer le numéro de registre"
            >
              {block.register ?? 0}
            </button>
          </div>
        {/if}
      {/if}
    </div>
    {#if block.kind === "instruction" 
      && isRegisterBlock(block)
      && registerPopupBlockId === block.id
    }
      <div
        class="register-popup"
        class:popup-above={registerPopupPosition === "above"}
      >
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
  .palette-block[data-type="jump-if-zero"],
  .program-block[data-type="jump-if-negative"],
  .palette-block[data-type="jump-if-negative"] {
    background: var(--jump-bg);
    border-color: var(--jump-border);
    color: var(--jump-color);
  }

  .program-block[data-type="add"],
  .palette-block[data-type="add"],
  .program-block[data-type="sub"],
  .palette-block[data-type="sub"],
  .program-block[data-type="inc"],
  .palette-block[data-type="inc"],
  .program-block[data-type="dec"],
  .palette-block[data-type="dec"] {
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
    background: linear-gradient(135deg, var(--slate-50), var(--slate-200));
    border-color: var(--slate-400);
    color: var(--slate-700);
  }

  .register-controls {
    position: relative;
    display: inline-flex;
    align-items: center;
    gap: 0.35rem;
  }

  .indirect-button {
    position: relative;

    width: 1.8rem;
    height: 1.8rem;

    display: grid;
    place-items: center;

    border-radius: 999px;
    border: 2px solid var(--slate-300);
    background: linear-gradient(135deg, var(--slate-50), var(--slate-200));
    color: var(--slate-500);

    font-size: 1.35rem;
    font-weight: 900;
    line-height: 1;

    cursor: pointer;

    box-shadow:
      inset 0 -3px 0 rgb(0 0 0 / 0.08),
      0 3px 8px rgb(15 23 42 / 0.12);
  }

  .indirect-button:hover {
    transform: translateY(-1px);
  }

  .indirect-button.indirect-enabled {
    border-color: var(--amber-500);
    background: linear-gradient(135deg, var(--amber-100), var(--amber-400));
    color: var(--amber-900);

    box-shadow:
      inset 0 -3px 0 rgb(0 0 0 / 0.12),
      0 0 0 4px rgb(245 158 11 / 0.18),
      0 4px 10px rgb(120 53 15 / 0.18);
  }

  .indirect-tooltip {
    position: absolute;
    left: 50%;
    bottom: calc(100% + 0.45rem);
    z-index: 200;
    transform: translateX(-50%) translateY(4px);
    width: max-content;
    padding: 0.35rem 0.55rem;
    border-radius: 10px;
    background: var(--slate-900);
    color: white;
    font-size: 0.75rem;
    font-weight: 800;
    line-height: 1.2;
    opacity: 0;
    pointer-events: none;

    transition:
      opacity 140ms ease,
      transform 140ms ease;
  }

  .indirect-tooltip::after {
    content: "";
    position: absolute;
    left: 50%;
    top: 100%;
    transform: translateX(-50%);
    border-width: 5px;
    border-style: solid;
    border-color: var(--slate-900) transparent transparent transparent;
  }

  .indirect-button:hover .indirect-tooltip,
  .indirect-button:focus-visible .indirect-tooltip {
    opacity: 1;
    transform: translateX(-50%) translateY(0);
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
    
    &:hover {
      background: white;
      transform: scale(1.06);
    }
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
  
    &.popup-above {
      top: auto;
      bottom: calc(100% + 0.4rem);
    }

    & button {
      width: 2rem;
      height: 2rem;
      border: 1px solid #ccc;
      border-radius: 0.5rem;
      background: #f8fafc;
      cursor: pointer;
      font-weight: 700;
    }

    & button:hover {
      background: var(--slate-200);
    }

    & button.selected-register {
      border-color: var(--blue-600);
      background: var(--blue-100);
      color: var(--blue-900);
    }
  }
</style>