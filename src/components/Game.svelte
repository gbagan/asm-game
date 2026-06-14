<script lang="ts">
  import { isPaletteBlock, isRegisterBlock, type DraggedBlock, type InstructionBlock,
        type InstructionType, type LevelInfo, type PaletteBlock, type ProgramBlock } from "../lib/types";
  import Editor from "./Editor.svelte";
  import Execution from "./Execution.svelte";
  import RetroBackground from "./RetroBackground.svelte";
  import { LEVELS } from "../lib/levels";

  type Props = {
    levelId: string;
    levelInfo: LevelInfo;
    saveInfo: (fn: (info: LevelInfo) => LevelInfo) => void;
    onQuitLevel: () => void;
  }

  let { levelId, levelInfo, saveInfo, onQuitLevel }: Props = $props();

  let { input, palette, registers, objective, expectedOutput } = $derived(LEVELS.find(lvl => lvl.id === levelId)!);

  let registerNames = $derived(registers.map((_, i) => 'R' + (i+1)));

  let program = $derived(levelInfo.program);
  let dialog = $state<"objective" | "help" | null>(null);
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
    dialog = "objective";
  }

  function openHelpDialog() {
    dialog = "help";
  }

  function closeDialog() {
    dialog = null;
  }

  function handleQuitLevel() {
    saveInfo(info => ({...info, program}));
    onQuitLevel();
  }

</script>

<RetroBackground />
<div class="screen">
  <div class="game">
    <Execution
      {program}
      initialInput={input}
      {registers}
      {registerNames}
      {expectedOutput}
      {setProgramCounter}
      {saveInfo}
      {onQuitLevel}
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
          class="button help-button"
          type="button"
          onclick={openHelpDialog}
        >
          ❔ Aide
        </button>
        <button
          class="button quit-level-button"
          type="button"
          onclick={handleQuitLevel}
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

{#if dialog === "objective"}
  <div
    class="dialog-backdrop"
    role="presentation"
    onclick={closeDialog}
  >
    <dialog
      class="dialog objective-dialog"
      open
      aria-labelledby="objective-title"
      onclick={(event) => event.stopPropagation()}
    >
      <header class="dialog-header">
        <h2 id="objective-title">Objectif du niveau</h2>

        <button
          class="dialog-close-button"
          type="button"
          onclick={closeDialog}
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
          onclick={closeDialog}
        >
          Compris !
        </button>
      </footer>
    </dialog>
  </div>
{:else if dialog === "help"}
  <div
    class="dialog-backdrop"
    role="presentation"
    onclick={closeDialog}
  >
    <dialog
      class="dialog help-dialog"
      open
      aria-labelledby="help-title"
      onclick={(event) => event.stopPropagation()}
    >
      <header class="dialog-header help-header">
        <h2 id="help-title">Aide : les instructions</h2>

        <button
          class="dialog-close-button"
          type="button"
          onclick={closeDialog}
          aria-label="Fermer"
        >
          ×
        </button>
      </header>

      <div class="help-content">
        <div class="game-help-intro">
          <h3>Principe du jeu</h3>
          <p>Le but du jeu est de construire un programme qui réalise l’objectif du niveau.</p>
          <p>
            Pour créer ton programme, fais glisser les blocs d’instructions dans la zone
            d’édition, puis lance l’exécution pour vérifier le résultat. Tu peux déplacer
            les blocs, ajouter des sauts avec des flèches, et utiliser les registres pour
            mémoriser des valeurs.
          </p>
        </div>
        <section class="instruction-help io-help">
          <h3>Input</h3>
          <p>
            Prend le premier nombre de l’input et le place dans la valeur courante.
          </p>
        </section>

        <section class="instruction-help io-help">
          <h3>Output</h3>
          <p>
            Envoie la valeur courante vers l’output.
          </p>
        </section>

        <section class="instruction-help jump-help">
          <h3>Jump</h3>
          <p>
            Saute directement vers la cible indiquée par la flèche.
          </p>
        </section>
        <section class="instruction-help jump-help">
          <h3>Jump If Zero</h3>
          <p>
            Saute directement vers la cible indiquée par la flèche si la valeur courante est égale à 0.
          </p>
        </section>
        <section class="instruction-help jump-help">
          <h3>Jump If Negative</h3>
          <p>
            Saute directement vers la cible indiquée par la flèche si la valeur courante est inférieure à 0.
          </p>
        </section>

        <section class="instruction-help memory-help">
          <h3>Copy From <span class="instruction-argument">i</span></h3>
          <p>
            Copie la valeur du registre <strong>i</strong> dans la valeur courante.
          </p>
        </section>

        <section class="instruction-help memory-help">
          <h3>Copy To <span class="instruction-argument">i</span></h3>
          <p>
            Copie la valeur courante dans le registre <strong>i</strong>.
          </p>
        </section>

        <section class="instruction-help arithmetic-help">
          <h3>Add <span class="instruction-argument">i</span></h3>
          <p>
            Additionne la valeur courante avec la valeur du registre <strong>i</strong>,
            puis place le résultat dans la valeur courante.
          </p>
        </section>
        <section class="instruction-help arithmetic-help">
          <h3>Sub <span class="instruction-argument">i</span></h3>
          <p>
            Soustrait la valeur du registre <strong>i</strong> à la valeur courante,
            puis place le résultat dans la valeur courante.
          </p>
        </section>
      </div>

      <footer class="dialog-footer">
        <button
          class="dialog-ok-button"
          type="button"
          onclick={closeDialog}
        >
          Compris !
        </button>
      </footer>
    </dialog>
  </div>
{/if}

<style>
  .screen {
    position: fixed;
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
    gap: 1rem;
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

  .help-button {
    border: 2px solid #3b82f6;
    background: linear-gradient(135deg, #dbeafe, #93c5fd);
    color: #1e3a8a;

    box-shadow:
      inset 0 -3px 0 rgb(0 0 0 / 0.12),
      0 4px 10px rgb(59 130 246 / 0.25);
  }

  .help-button:hover {
    transform: translateY(-2px);
    filter: brightness(1.03);
    box-shadow:
      inset 0 -3px 0 rgb(0 0 0 / 0.12),
      0 8px 16px rgb(59 130 246 / 0.28);
  }

  .help-button:active {
    transform: translateY(1px);
    box-shadow:
      inset 0 -1px 0 rgb(0 0 0 / 0.16),
      0 2px 6px rgb(59 130 246 / 0.22);
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

  .dialog {
    position: fixed;
    left: 50%;
    top: 50%;
    transform: translate(-50%, -50%);  
    overflow: hidden;
    animation: dialog-pop 160ms ease-out;
  }

  .objective-dialog {
    width: 30rem;
    border: 3px solid #f59e0b;
    border-radius: 1.2rem;

    background: linear-gradient(135deg, #fff7ed, #fffbeb);
    color: #431407;

    box-shadow:
      0 20px 50px rgb(15 23 42 / 0.35),
      inset 0 -5px 0 rgb(0 0 0 / 0.08);

    overflow: hidden;
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

  .dialog-footer {
    display: flex;
    justify-content: flex-end;

    padding: 1rem 1.25rem;
    border-top: 1px solid rgb(245 158 11 / 0.25);
  }

  .dialog-ok-button {
    font-size: 1rem;
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
      transform: translate(-50%, calc(-50%-0.5rem)) scale(0.92);
    }

    to {
      opacity: 1;
      transform: translate(-50%, -50%) translateY(0);
    }
  }

  .help-dialog {
    width: 75rem;
    height: 45rem;

    border: 3px solid #3b82f6;
    border-radius: 22px;

    background: linear-gradient(135deg, #eff6ff, #f8fafc);
    color: #0f172a;

    box-shadow:
      0 20px 50px rgb(15 23 42 / 0.35),
      inset 0 -5px 0 rgb(0 0 0 / 0.08);

    overflow: hidden;

    animation: dialog-pop 160ms ease-out;
  }

  .help-header {
    background: linear-gradient(135deg, #dbeafe, #93c5fd);
    border-bottom-color: #3b82f6;
  }

  .help-header h2 {
    color: #1e3a8a;
  }

  .help-content {
    padding: 1.25rem;

    display: grid;
    grid-template-columns: repeat(4, minmax(0, 1fr));
    gap: 0.85rem;

    overflow-y: auto;
  }

  .game-help-intro {
    grid-column: 1 / -1;
    padding: 1rem 1.1rem;
    border-radius: 1rem;
    border: 2px solid #3b82f6;
    background: linear-gradient(135deg, #eff6ff, #dbeafe);
    color: #1e3a8a;
    box-shadow:
      inset 0 -3px 0 rgb(0 0 0 / 0.06),
      0 4px 10px rgb(15 23 42 / 0.08);
  }

  .game-help-intro h3 {
    margin: 0 0 0.5rem;
    font-size: 1.1rem;
    font-weight: 900;
  }

  .game-help-intro p {
    line-height: 1.5;
    font-weight: 600;
  }

  .game-help-intro p + p {
    margin-top: 0.5rem;
  }

  .instruction-help {
    padding: 0.9rem 1rem;
    border-radius: 0.9rem;
    border: 2px solid #cbd5e1;
    background: white;

    box-shadow:
      inset 0 -3px 0 rgb(0 0 0 / 0.06),
      0 4px 10px rgb(15 23 42 / 0.08);
  }

  .instruction-help h3 {
    margin: 0 0 0.4rem;

    display: flex;
    align-items: center;
    gap: 0.4rem;

    font-size: 1.05rem;
    font-weight: 900;
  }

  .instruction-help p {
    margin: 0;
    line-height: 1.45;
    color: #334155;
  }

  .instruction-argument {
    min-width: 1.5rem;
    height: 1.5rem;
    padding: 0 0.35rem;

    display: inline-grid;
    place-items: center;

    border-radius: 999px;
    background: rgb(255 255 255 / 0.75);
    border: 2px solid rgb(0 0 0 / 0.12);

    font-size: 0.85rem;
    font-weight: 900;
  }

  .io-help {
    border-color: var(--io-border);
    background: var(--io-bg);
    color: var(--io-color);
  }

  .jump-help {
    border-color: var(--jump-border);
    background: var(--jump-bg);
    color: var(--jump-color);
  }

  .memory-help {
    border-color: #8b5cf6;
    background: linear-gradient(135deg, #ede9fe, #ddd6fe);
    color: #3b0764;
  }

  .arithmetic-help {
    border-color: var(--arith-border);
    background: var(--arith-bg);
    color: var(--arith-color);
  }
</style>