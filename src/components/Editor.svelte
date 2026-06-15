<script lang="ts">
  import { sleep } from "@gbagan/utils";
  import { cubicOut } from "svelte/easing";
  import { flip } from "svelte/animate";
  import { draggable, droppable, type DragDropState } from "@thisux/sveltednd";
  import Arrows from "./Arrows.svelte";
  import Block from "./Block.svelte";
  import {
    type ProgramBlock,
    type PaletteBlock,
    type DraggedBlock,
    type InstructionType,
    isPaletteBlock,
  } from "../lib/types";
  import ExecutionPointer from "./ExecutionPointer.svelte";
  import { tick } from "svelte";
    import { playDragSound, playDropSound } from "../lib/sound";

  const FLIP_DURATION = 220;

  type Props = {
    registerCount: number;
    palette: InstructionType[];
    program: ProgramBlock[];
    currentInstructionId?: string;
    insertBlocksAt: (index: number, block: PaletteBlock) => void;
    moveBlock: (fromBlock: ProgramBlock, toIndex: number) => boolean;
    removeBlock: (draggedItem: DraggedBlock) => boolean;
    setRegister: (blockId: string, register: number) => void;
  }

  let { registerCount, palette: rawPalette, program, currentInstructionId,
    insertBlocksAt, moveBlock, removeBlock, setRegister }: Props = $props();

  let container: HTMLDivElement | undefined = $state.raw();
  let isAnimatingLayout = $state.raw(false);
  let layoutVersion = $state.raw(0);
  let draggingSource = $state.raw<"palette" | "program" | null>(null);

  let registerPopupBlockId: string | null = $state.raw(null);
  let registerPopupPosition: "below" | "above" = $state.raw("below");
  let isRegisterPopupOpen = $derived(registerPopupBlockId !== null);

  let palette: PaletteBlock[] = $derived(rawPalette.map(type => ({
    id: `palette-${type}`,
    type,
    fromPalette: true
  })));

  let isRunning = $derived(currentInstructionId !== null);

  function handleDropZoneDrop(state: DragDropState<DraggedBlock>) {
    const { draggedItem, targetContainer } = state;
    const dropIndex = Number.parseInt(String(targetContainer), 10);

    if (Number.isNaN(dropIndex)) return;

    if (isPaletteBlock(draggedItem)) {
      insertBlocksAt(dropIndex, draggedItem);
      startLayoutAnimation();
    } else {
      moveBlock(draggedItem, dropIndex) && startLayoutAnimation();
    }
  }

  function handleTrashDrop(state: DragDropState<DraggedBlock>) {
    removeBlock(state.draggedItem) && startLayoutAnimation();
  }

  async function startLayoutAnimation() {
    isAnimatingLayout = true;
    await sleep(FLIP_DURATION+20);
    isAnimatingLayout = false;
    layoutVersion++;
  }

  function openRegisterPopup(blockId: string) {
    if (!container) return;

    const blockElement = container.querySelector<HTMLElement>(
      `[data-block-id="${blockId}"]`
    );

    if (!blockElement) return;

    const editorRect = container.getBoundingClientRect();
    const blockRect = blockElement.getBoundingClientRect();
    const spaceBelow = (editorRect.bottom - blockRect.bottom) / editorRect.height;
    registerPopupPosition =
      spaceBelow < 0.25 ? "above" : "below";

    registerPopupBlockId = blockId;
  }

  function closeRegisterPopup() {
    registerPopupBlockId = null;
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
    use:draggable={{
      container: `block-${block.id}`,
      disabled: isRegisterPopupOpen || isRunning,
      dragData: block,
      interactive: [ "button" ],
      callbacks: {
        onDragStart: () => { playDragSound(); draggingSource = "program"},
        onDragEnd: () => { playDropSound(); draggingSource = null }
      }
    }}
  >
    <Block
      {block}
      {registerPopupBlockId}
      {registerPopupPosition}
      {registerCount}
      {openRegisterPopup}
      onRegisterClick={(id: string, index: number) => {
        setRegister(id, index);
        closeRegisterPopup();
      }}
    />
  </div>
{/snippet}

<svelte:window onpointerdown={handleWindowPointerDown} />

<div class="layout">
  <aside class="palette">
    <h2>Blocs</h2>

    <div class="palette-list">
      {#each palette as block (block.id)}
        <div
          use:draggable={{
            container: "palette",
            disabled: isRegisterPopupOpen || isRunning,
            dragData: block,
            callbacks: {
              onDragStart: () => { playDragSound(); draggingSource = "palette"},
              onDragEnd: () => { playDropSound(); draggingSource = null }
            }
          }}
        >
          <Block {block} />
        </div>
      {/each}
    </div>
    <div
      class="trash-drop-zone"
      class:trash-visible={draggingSource === "program"}
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
      <Arrows {program} {container} hidden={isAnimatingLayout} {layoutVersion} />

      <div class="program">
        {#if program.length === 0}
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
          {#each program as block, index (block.id)}
            <div
              class="program-row"
              class:has-open-popup={registerPopupBlockId === block.id}
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
              container: program.length.toString(),
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
    transform: scale(1.05);
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

  .program-row {
    position: relative;
    z-index: 1;
  }

  .program-row.has-open-popup {
    z-index: 100;
  }

  .between-drop-zone {
    width: 12rem;
    height: 1rem;
    border-radius: 999px;
    transition:
      height 120ms ease,
      background 120ms ease,
      border-color 120ms ease;
  }

  .between-drop-zone:global(.drag-over) {
    height: 2.5rem;
    border-color: #3b82f6;
    border-style: dashed;
    background: rgb(255 255 255 / 0.85);
  }

  .empty-drop-zone {
    width: 12rem;
    height: 5rem;
    border: 2px dashed #aaa;
    border-radius: 0.75rem;
    color: #666;
    background: white;
    display: grid;
    place-items: center;
  }

  .end-drop-zone {
    width: 12rem;
    height: 2.5rem;
    border: 2px dashed #bbb;
    border-radius: 10px;
    color: #888;
    background: rgb(255 255 255 / 0.5);
    display: grid;
    place-items: center;
    font-size: 0.85rem;
  }

  .end-drop-zone:global(.drag-over) {
    height: 3rem;
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
</style>