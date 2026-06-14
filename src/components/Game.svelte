<script lang="ts">
  import { isPaletteBlock, isRegisterBlock, type DraggedBlock, type InstructionBlock,
        type InstructionType, type PaletteBlock, type ProgramBlock } from "../lib/types";
  import Editor from "./Editor.svelte";
  import Execution from "./Execution.svelte";
  import RetroBackground from "./RetroBackground.svelte";
  import { LEVELS } from "../lib/levels";

  type Props = {
    levelId: string;
  }

  let { levelId }: Props = $props();

  let { palette, registers, objective } = $derived(LEVELS.find(lvl => lvl.id === levelId)!);

  let registerNames = $derived(registers.map((_, i) => 'R' + (i+1)));

  let program = $state.raw<ProgramBlock[]>([]);
  let showObjectiveDialog = $state(false);
  let pc = $state(0);

  function createInstructionBlock(type: InstructionType): ProgramBlock {
    const block: InstructionBlock = {
      id: crypto.randomUUID(),
      kind: "instruction",
      type
    };
    if (isRegisterBlock(block)) {
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
    program = [
      ...program.slice(0, index),
      ...createBlocksFromPalette(block),
      ...program.slice(index)
    ];
  }

  function moveBlock(draggedBlock: ProgramBlock, dropIndex: number): boolean {
    const dragIndex = program.findIndex(block => block.id === draggedBlock.id);

    if (dragIndex === -1 || dropIndex === dragIndex || dropIndex === dragIndex + 1) return false;

    const nextProgram = [...program];
    const [movedBlock] = nextProgram.splice(dragIndex, 1);

    const adjustedDropIndex =
      dragIndex < dropIndex ? dropIndex - 1 : dropIndex;

    nextProgram.splice(adjustedDropIndex, 0, movedBlock);
    program = nextProgram;
    return true;
  }



  function removeBlock(item: DraggedBlock) {
    if (isPaletteBlock(item)) return false;

    // Si on supprime un jump, on supprime aussi sa cible.
    if (item.kind === "instruction") {
      program = program.filter(b =>
        b.id !== item.id && b.id !== item.targetId
      );
      return true;
    } else if (item.kind === "jump-target") {
      program = program.filter(b =>
        b.id !== item.id && b.id !== item.ownerJumpId
      );
      return true;
    }
    return false;
  }

  function setRegister(blockId: string, register: number) {
    program = program.map(block => {
      if (block.kind !== "instruction") return block;
      if (block.id !== blockId) return block;
      return { ...block, register };
    });
  }

  function setProgramCounter(r: number) {
    pc = r;
  }

  function openObjectiveDialog() {
    showObjectiveDialog = true;
  }

  function closeObjectiveDialog() {
    showObjectiveDialog = false;
  }

</script>

<RetroBackground />
<div class="screen">
  <div class="game">
    <Execution
      {program}
      initialInput={[3, 4, 8, 12]}
      {registers}
      {registerNames}
      {setProgramCounter}
    />
    <div>
      <div class="editor-topbar">
        <button
          class="button objective-button"
          type="button"
          onclick={openObjectiveDialog}
        >
          🎯 Objectif
        </button>
        <button
          class="button quit-level-button"
          type="button"
          onclick={() => {
            // TODO: quitter le niveau actuel
            console.log("Quitter le niveau");
          }}
        >
          🚪 Quitter
        </button>
      </div>
      <Editor
        {palette}
        registers={["1", "2", "3", "4"]}
        {program}
        currentInstructionId={program[pc]?.id}
        {insertBlocksAt}
        {moveBlock}
        {removeBlock}
        {setRegister}
      />
    </div>
  </div>
</div>

{#if showObjectiveDialog}
  <div
    class="dialog-backdrop"
    role="presentation"
    onclick={closeObjectiveDialog}
  >
    <dialog
      class="objective-dialog"
      open
      aria-labelledby="objective-title"
      onclick={(event) => event.stopPropagation()}
    >
      <header class="dialog-header">
        <h2 id="objective-title">Objectif du niveau</h2>

        <button
          class="dialog-close-button"
          type="button"
          onclick={closeObjectiveDialog}
          aria-label="Fermer"
        >
          ×
        </button>
      </header>

      <div class="dialog-content">
        <p>{objective}</p>
      </div>

      <footer class="dialog-footer">
        <button
          class="dialog-ok-button"
          type="button"
          onclick={closeObjectiveDialog}
        >
          Compris !
        </button>
      </footer>
    </dialog>
  </div>
{/if}

<style>
  .screen {
    width: 100vw;
    height: 100vh;
    display: flex;
    align-items: center;
    justify-content: center;
    overflow: hidden;
  }
  
  .game {
    aspect-ratio: 16 / 9;
    width: min(100vw, calc(100vh * 16 / 9));
    height: min(100vh, calc(100vw * 9 / 16));
    display: flex;
    align-items: center;
    justify-content: space-around;
  }

  .editor-topbar {
    display: flex;
    justify-content: flex-end;
    align-items: center;
    margin-bottom: 0.75rem;
  }

  .button {
    padding: 0.6rem 1rem;
    font-weight: 800;
    font-size: 0.95rem;
    border-radius: 999px;
    cursor: pointer;
    user-select: none;
    transition:
      transform 120ms ease,
      box-shadow 120ms ease,
      filter 120ms ease;
  }

  .objective-button {
    border: 2px solid #f59e0b;

    background: linear-gradient(135deg, #fef3c7, #fde68a);
    color: #78350f;

    box-shadow:
      inset 0 -3px 0 rgb(0 0 0 / 0.12),
      0 4px 10px rgb(245 158 11 / 0.25);
  }  

  .objective-button:hover {
    transform: translateY(-2px);
    filter: brightness(1.03);
    box-shadow:
      inset 0 -3px 0 rgb(0 0 0 / 0.12),
      0 8px 16px rgb(245 158 11 / 0.28);
  }

  .objective-button:active {
    transform: translateY(1px);
    box-shadow:
      inset 0 -1px 0 rgb(0 0 0 / 0.16),
      0 2px 6px rgb(245 158 11 / 0.22);
  }

  .quit-level-button {
    border: 2px solid #64748b;
    background: linear-gradient(135deg, #f8fafc, #cbd5e1);
    color: #334155;
    box-shadow:
      inset 0 -3px 0 rgb(0 0 0 / 0.12),
      0 4px 10px rgb(100 116 139 / 0.25);
  }

  .quit-level-button:hover {
    transform: translateY(-2px);
    filter: brightness(1.03);
    box-shadow:
      inset 0 -3px 0 rgb(0 0 0 / 0.12),
      0 8px 16px rgb(100 116 139 / 0.28);
  }

  .quit-level-button:active {
    transform: translateY(1px);
    box-shadow:
      inset 0 -1px 0 rgb(0 0 0 / 0.16),
      0 2px 6px rgb(100 116 139 / 0.22);
  }



  .dialog-backdrop {
    position: fixed;
    inset: 0;
    z-index: 1000;

    display: grid;
    place-items: center;

    background: rgb(15 23 42 / 0.55);
    backdrop-filter: blur(3px);
  }

  .objective-dialog {
    width: min(520px, calc(100vw - 2rem));
    max-height: min(420px, calc(100vh - 2rem));

    margin: 0;
    padding: 0;

    border: 3px solid #f59e0b;
    border-radius: 22px;

    background: linear-gradient(135deg, #fff7ed, #fffbeb);
    color: #431407;

    box-shadow:
      0 20px 50px rgb(15 23 42 / 0.35),
      inset 0 -5px 0 rgb(0 0 0 / 0.08);

    overflow: hidden;

    animation: dialog-pop 160ms ease-out;
  }

  .dialog-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 1rem;

    padding: 1rem 1.25rem;

    background: linear-gradient(135deg, #fef3c7, #fde68a);
    border-bottom: 2px solid #f59e0b;
  }

  .dialog-header h2 {
    margin: 0;
    font-size: 1.25rem;
    color: #78350f;
  }

  .dialog-close-button {
    width: 2rem;
    height: 2rem;

    border: none;
    border-radius: 999px;

    background: rgb(255 255 255 / 0.65);
    color: #78350f;

    font-size: 1.4rem;
    font-weight: 900;
    line-height: 1;

    cursor: pointer;
  }

  .dialog-close-button:hover {
    background: white;
  }

  .dialog-content {
    padding: 1.25rem;
    font-size: 1rem;
    line-height: 1.5;
  }

  .dialog-content p {
    margin: 0;
  }

  .dialog-footer {
    display: flex;
    justify-content: flex-end;

    padding: 1rem 1.25rem;
    border-top: 1px solid rgb(245 158 11 / 0.25);
  }

  .dialog-ok-button {
    padding: 0.6rem 1rem;

    border: 2px solid #22c55e;
    border-radius: 999px;

    background: linear-gradient(135deg, #dcfce7, #86efac);
    color: #14532d;

    font-weight: 800;
    cursor: pointer;

    box-shadow:
      inset 0 -3px 0 rgb(0 0 0 / 0.12),
      0 4px 10px rgb(34 197 94 / 0.22);
  }

  .dialog-ok-button:hover {
    filter: brightness(1.03);
  }

  @keyframes dialog-pop {
    from {
      opacity: 0;
      transform: scale(0.92) translateY(8px);
    }

    to {
      opacity: 1;
      transform: scale(1) translateY(0);
    }
  }
</style>