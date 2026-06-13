<script lang="ts">
  import { sleep } from "@gbagan/utils";
  import { cubicOut } from "svelte/easing";
  import { flip } from "svelte/animate";
  import { draggable, droppable, type DragDropState } from "@thisux/sveltednd";
  import Arrows from "./Arrows.svelte";
  import type {
    ProgramBlock,
    PaletteBlock,
    DraggedBlock,
    InstructionType,

    InstructionBlock

  } from "../lib/types";
    import ExecutionPointer from "./ExecutionPointer.svelte";
    import { tick } from "svelte";

  const FLIP_DURATION = 220;
  const LABEL = {
    input: "input",
    output: "output",
    jump: "jump",
    "jump-if-zero": "jump if zero",
    "jump-if-negative": "jump if < 0",
    "copy-from": "copy from",
    "copy-to": "copy to"
  }

  type Props = {
    registers: string [];
  }

  let { registers }: Props = $props();

  let container: HTMLDivElement | undefined = $state();

  let isAnimatingLayout = $state(false);
  let layoutVersion = $state(0);
  let draggingSource = $state<"palette" | "program" | null>(null);
  let currentInstructionId = $state<string | null>("b2"); // todo

  let palette = $state.raw<PaletteBlock[]>([
    {
      id: "palette-input",
      type: "input",
      fromPalette: true
    },
    {
      id: "palette-output",
      type: "output",
      fromPalette: true
    },
    {
      id: "palette-jump",
      type: "jump",
      fromPalette: true
    },
    {
      id: "palette-jump-if-zero",
      type: "jump-if-zero",
      fromPalette: true
    },
    {
      id: "palette-copy-from",
      type: "copy-from",
      fromPalette: true
    },
    {
      id: "palette-copy-to",
      type: "copy-to",
      fromPalette: true
    }
  ]);

  let blocks = $state.raw<ProgramBlock[]>([
    {
      id: "b1",
      kind: "instruction",
      type: "input"
    },
    {
      id: "b2",
      kind: "instruction",
      type: "output"
    }
  ]);

  function isPaletteBlock(block: DraggedBlock): block is PaletteBlock {
    return "fromPalette" in block;
  }

  function createInstructionBlock(type: InstructionType): ProgramBlock {
    const block: InstructionBlock = {
      id: crypto.randomUUID(),
      kind: "instruction",
      type
    };
    if (type === "copy-from" || type === "copy-to") {
      return { ...block, register: 0 };
    }
    return block;
  }

  function createJumpPair(type: "jump" | "jump-if-zero"): ProgramBlock[] {
    const jumpId = crypto.randomUUID();
    const targetId = crypto.randomUUID();
    return [
      {
        id: jumpId,
        kind: "instruction",
        type,
        targetId
      },
      {
        id: targetId,
        kind: "jump-target",
        ownerJumpId: jumpId
      }
    ];
  }

  function createBlocksFromPalette(block: PaletteBlock): ProgramBlock[] {
    if (block.type === "jump" || block.type === "jump-if-zero") {
      return createJumpPair(block.type);
    } else { 
      return [ createInstructionBlock(block.type) ];
    }
  }

  function insertBlocksAt(index: number, block: PaletteBlock) {
    blocks = [
      ...blocks.slice(0, index),
      ...createBlocksFromPalette(block),
      ...blocks.slice(index)
    ];
    startLayoutAnimation();
  }

  function moveBlock(draggedBlock: ProgramBlock, dropIndex: number) {
    const dragIndex = blocks.findIndex(block => block.id === draggedBlock.id);

    if (dragIndex === -1 || dropIndex === dragIndex || dropIndex === dragIndex + 1) return;

    const nextBlocks = [...blocks];
    const [movedBlock] = nextBlocks.splice(dragIndex, 1);

    const adjustedDropIndex =
      dragIndex < dropIndex ? dropIndex - 1 : dropIndex;

    nextBlocks.splice(adjustedDropIndex, 0, movedBlock);

    blocks = nextBlocks;
    startLayoutAnimation();
  }

  function handleDropZoneDrop(state: DragDropState<DraggedBlock>) {
    const { draggedItem, targetContainer } = state;
    const dropIndex = Number.parseInt(String(targetContainer), 10);

    if (Number.isNaN(dropIndex)) return;

    if (isPaletteBlock(draggedItem)) {
      insertBlocksAt(dropIndex, draggedItem);
    } else {
      moveBlock(draggedItem, dropIndex);
    }
  }

  function removeDraggedBlock(draggedItem: DraggedBlock) {
    if (isPaletteBlock(draggedItem)) return;

    // Si on supprime un jump, on supprime aussi sa cible.
    if (draggedItem.kind === "instruction") {
      blocks = blocks.filter(b =>
        b.id !== draggedItem.id && b.id !== draggedItem.targetId
      );
      startLayoutAnimation();
    } else if (draggedItem.kind === "jump-target") {
      blocks = blocks.filter(b =>
        b.id !== draggedItem.id && b.id !== draggedItem.ownerJumpId
      );
      startLayoutAnimation();
    }
  }

  function handleTrashDrop(state: DragDropState<DraggedBlock>) {
    removeDraggedBlock(state.draggedItem);
  }

  async function startLayoutAnimation() {
    isAnimatingLayout = true;
    await sleep(FLIP_DURATION+20);
    isAnimatingLayout = false;
    layoutVersion++;
  }

  let registerPopupBlockId = $state<string | undefined>();
  let isRegisterPopupOpen = $derived(registerPopupBlockId !== undefined);

  function openRegisterPopup(blockId: string) {
    registerPopupBlockId = blockId;
  }

  function closeRegisterPopup() {
    registerPopupBlockId = undefined;
  }

  function setRegister(blockId: string, register: number) {
    blocks = blocks.map(block => {
      if (block.kind !== "instruction") return block;
      if (block.id !== blockId) return block;
      return { ...block, register };
    });
    closeRegisterPopup();
  }

  function handleWindowPointerDown(event: PointerEvent) {
    const target = event.target;
    if (!(target instanceof HTMLElement)) return;
    const clickedInsidePopup = target.closest(".register-popup");
    const clickedRegisterButton = target.closest(".register-badge");
    if (clickedInsidePopup || clickedRegisterButton) {
      return;
    }
    closeRegisterPopup();
  }

  async function scrollCurrentInstructionIntoView() {
    await tick();
    if (!container || currentInstructionId === null) return;
    const blockEl = container.querySelector<HTMLElement>(
      `[data-block-id="${currentInstructionId}"]`
    );

    if (!blockEl) return;
    const containerRect = container.getBoundingClientRect();
    const blockRect = blockEl.getBoundingClientRect();
    const padding = 24;
    const blockTop = blockRect.top - containerRect.top;
    const blockBottom = blockRect.bottom - containerRect.top;
    const visibleTop = padding;
    const visibleBottom = container.clientHeight - padding;

    if (blockTop < visibleTop) {
      container.scrollBy({
        top: blockTop - visibleTop,
        behavior: "smooth"
      });
    } else if (blockBottom > visibleBottom) {
      container.scrollBy({
        top: blockBottom - visibleBottom,
        behavior: "smooth"
      });
    }
  }

  $effect(() => {
    currentInstructionId;
    scrollCurrentInstructionIntoView();
  });
</script>

{#snippet blockView(block: ProgramBlock, index: number)}
  <div
    class="between-drop-zone"
    use:droppable={{
      container: index.toString(),
      direction: "vertical",
      callbacks: {
        onDrop: handleDropZoneDrop
      }
    }}
  ></div>
  <div
    class:program-block={block.kind === "instruction"}
    class:jump-target-block={block.kind === "jump-target"}
    class:has-open-popup={registerPopupBlockId === block.id}
    data-block-id={block.id}
    data-type={block.kind === "instruction" ? block.type : ""}
    use:draggable={{
      container: `block-${block.id}`,
      disabled: isRegisterPopupOpen,
      dragData: block,
      interactive: [ "button" ],
      callbacks: {
        onDragStart: () => draggingSource = "program",
        onDragEnd: () => draggingSource = null
      }
    }}
  >
    <div class="block-label">
        {#if block.kind === "instruction"}
          <strong>{LABEL[block.type]}</strong>
          {#if block.type === "copy-from" || block.type === "copy-to"}
            <button
              class="register-badge"
              type="button"
              onclick={() => openRegisterPopup(block.id)}
              aria-label="Changer le numéro de registre"
            >
              {registers[block.register ?? 0]}
            </button>
          {/if}
        {/if}
    </div>
    {#if block.kind === "instruction" 
      && (block.type === "copy-from" || block.type === "copy-to")
      && registerPopupBlockId === block.id
    }
      <div class="register-popup">
        {#each registers as register, i}
          <button
            type="button"
            class:selected-register={block.register === i}
            onclick={() => setRegister(block.id, i)}
          >
            {register}
          </button>
        {/each}
      </div>
    {/if}
  </div>
{/snippet}

<svelte:window onpointerdown={handleWindowPointerDown} />

<div class="layout">
  <aside class="palette">
    <h2>Blocs</h2>

    <div class="palette-list">
      {#each palette as block (block.id)}
        <div
          class="palette-block"
          data-type={block.type}
          use:draggable={{
            container: "palette",
            disabled: isRegisterPopupOpen,
            dragData: block,
            callbacks: {
              onDragStart: () => draggingSource = "palette",
              onDragEnd: () => draggingSource = null
            }
          }}
        >
          {LABEL[block.type]}
        </div>
      {/each}
    </div>
    <div
      class="trash-drop-zone"
      class:trash-visible={draggingSource !== null}
      use:droppable={{
        container: "trash",
        callbacks: {
          onDrop: handleTrashDrop
        }
      }}
    >
      Déposer ici pour supprimer
    </div>
  </aside>

  <main class="editor-panel">
    <h2>Programme</h2>

    <div class="editor" bind:this={container}>
      <ExecutionPointer {container} blockId={currentInstructionId} {layoutVersion} />
      <Arrows {blocks} {container} hidden={isAnimatingLayout} {layoutVersion} />

      <div class="program">
        {#if blocks.length === 0}
          <div
            class="between-drop-zone empty-drop-zone"
            use:droppable={{
              container: "0",
              direction: "vertical",
              callbacks: {
                onDrop: handleDropZoneDrop
              }
            }}
          >
            Glisse un bloc ici pour commencer.
          </div>
        {:else}
          {#each blocks as block, index (block.id)}
            <div
              animate:flip={{
                duration: FLIP_DURATION,
                easing: cubicOut
              }}
            >
              {@render blockView(block, index)}
            </div>
          {/each}
          <div
            class="between-drop-zone end-drop-zone"
            use:droppable={{
              container: blocks.length.toString(),
              direction: "vertical",
              callbacks: {
                onDrop: handleDropZoneDrop
              }
          }}
          >
            Déposer ici pour placer en bas
          </div>
        {/if}
      </div>
    </div>
  </main>
</div>

<style>
  .layout {
    display: grid;
    grid-template-columns: 10rem minmax(0, 1fr);
    gap: 1rem;
    height: 45rem;
  }

  h2 {
    margin: 0 0 0.75rem;
    font-size: 1rem;
  }

  .palette {
    border: 2px solid #ddd;
    border-radius: 12px;
    padding: 1rem;
    background: rgba(255, 255, 255, 0.7);
  }

  .palette-list {
    display: grid;
    gap: 0.5rem;
  }

  .palette-block {
    border-radius: 10px;
    height: 2.5rem;
    border: 2px solid #bbb;
    cursor: grab;
    user-select: none;
    font-weight: 600;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .trash-drop-zone {
    margin-top: 1rem;
    min-height: 72px;
    padding: 0.75rem;
    border: 2px dashed transparent;
    border-radius: 12px;
    color: transparent;
    background: transparent;
    display: grid;
    place-items: center;
    text-align: center;
    font-size: 0.9rem;
    transition:
      color 120ms ease,
      background 120ms ease,
      border-color 120ms ease,
      transform 120ms ease;
    }

  .trash-drop-zone.trash-visible {
    color: #7f1d1d;
    border-color: #fca5a5;
    background: #fee2e2;
  }

  .trash-drop-zone:global(.drag-over) {
    color: #450a0a;
    border-color: #ef4444;
    background: #fecaca;
    transform: scale(1.03);
  }

  .editor-panel {
    min-height: 0;
  }

  .editor {
    position: relative;
    height: calc(100% - 2rem);
    min-height: 0;
    width: 20rem;
    overflow: auto;
    border: 2px solid #ddd;
    border-radius: 0.7rem;
    background: #f7f7fb;
    overflow-y: auto;
    overflow-x: hidden;
  }

  .program {
    position: relative;
    z-index: 2;
    height: 100%;
    padding: 1rem 6rem 1rem 3rem;
    display: flex;
    flex-direction: column;
  }

  .between-drop-zone {
    width: 260px;
    height: 14px;
    border-radius: 999px;
    transition:
      height 120ms ease,
      background 120ms ease,
      border-color 120ms ease;
  }

  .between-drop-zone {
    width: 260px;
    height: 12px;
    border-radius: 999px;
    border: 2px solid transparent;
    transition:
      height 120ms ease,
      background 120ms ease,
      border-color 120ms ease;
  }

  .between-drop-zone:global(.drag-over) {
    height: 42px;
    border-color: #3b82f6;
    border-style: dashed;
    background: rgb(255 255 255 / 0.85);
  }

  .end-drop-zone {
    height: 2.5rem;
    border: 2px dashed #bbb;
    color: #888;
    background: rgb(255 255 255 / 0.5);
    display: grid;
    place-items: center;
    font-size: 0.85rem;
  }

  .empty-drop-zone {
    width: 260px;
    height: 80px;
    border: 2px dashed #aaa;
    border-radius: 12px;
    color: #666;
    background: white;
    display: grid;
    place-items: center;
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
    background: linear-gradient(135deg, #fef3c7, #fde68a);
    border-color: #f59e0b;
    color: #78350f;
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
  .program-block[data-type="sub"],
  .program-block[data-type="mul"],
  .program-block[data-type="div"] {
    background: linear-gradient(135deg, #dcfce7, #bbf7d0);
    border-color: #22c55e;
    color: #14532d;
  }

  .program-block[data-type="copy-from"],
  .palette-block[data-type="copy-from"],
  .program-block[data-type="copy-to"],
  .palette-block[data-type="copy-to"] {
    background: linear-gradient(135deg, #ede9fe, #ddd6fe);
    border-color: #8b5cf6;
    color: #3b0764;
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

  .end-drop-zone {
    width: 260px;
    min-height: 2.5rem;
    border: 2px dashed #bbb;
    border-radius: 10px;
    color: #888;
    background: rgb(255 255 255 / 0.5);
    display: grid;
    place-items: center;
    font-size: 0.85rem;
  }

  :global(.drop-before::before),
  :global(.drop-after::after) {
    display: none;
    content: none;
  }

  :global(.dragging) {
    opacity: 0.5;
  }

  :global(.drag-over) {
    outline: 2px dashed #4caf50;
  }

  .block-label {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    min-width: 0;
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