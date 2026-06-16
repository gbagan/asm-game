<script lang="ts">
  import { isJumpBlock, isPaletteBlock, isRegisterBlock, type DraggedBlock, type InstructionBlock,
        type InstructionType, type LevelInfo, type PaletteBlock, type ProgramBlock } from "../lib/types";
  import { LEVELS } from "../lib/levels";
  import Button from "./Button.svelte";
  import Editor from "./Editor.svelte";
  import Execution from "./Execution.svelte";
  import RetroBackground from "./RetroBackground.svelte";
  import HelpDialog from "./HelpDialog.svelte";
  import ObjectiveDialog from "./ObjectiveDialog.svelte";


  type Props = {
    levelId: string;
    levelInfo: LevelInfo;
    saveInfo: (fn: (info: LevelInfo) => LevelInfo) => void;
    onQuitLevel: () => void;
  }

  let { levelId, levelInfo, saveInfo, onQuitLevel }: Props = $props();

  let { input, palette, registers, objective, expectedOutput, allowIndirect: indirectMode } = $derived(LEVELS.find(lvl => lvl.id === levelId)!);

  let program = $derived(levelInfo.program);
  let dialog: "objective" | "help" | null = $state.raw(null);
  let programCounter: number | null = $state.raw(null);

  function createInstructionBlock(type: InstructionType): ProgramBlock {
    const block: InstructionBlock = {
      id: crypto.randomUUID(),
      kind: "instruction",
      type
    };
    if (isRegisterBlock(block)) {
      const indirect = indirectMode ? false : undefined;
      return { ...block, register: 0, indirect };
    }
    return block;
  }

  function createJumpPair(type: InstructionType): ProgramBlock[] {
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
    if (isJumpBlock(block)) {
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

  function toggleIndirect(blockId: string) {
    program = program.map(block => {
      if (block.id !== blockId) return block;
      if (block.kind !== "instruction") return block;
      if (!isRegisterBlock(block)) return block;
      return { ...block, indirect: !block.indirect };
    });
  }

  function setProgramCounter(pc: number | null) {
    programCounter = pc;
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
      {programCounter}
      initialInput={input}
      {registers}
      {expectedOutput}
      {setProgramCounter}
      {saveInfo}
      {onQuitLevel}
    />
    <div>
      <div class="editor-topbar">
        <Button variant="yellow" onclick={openObjectiveDialog}>
          🎯 Objectif
        </Button>
        <Button variant="blue" onclick={openHelpDialog}>
          ❔ Aide
        </Button>
        <Button variant="gray" disabled={programCounter !== null}  onclick={handleQuitLevel}>
          🚪 Quitter
        </Button>
      </div>
      <Editor
        {palette}
        registerCount={registers.length}
        {program}
        currentInstructionId={programCounter === null ? undefined : program[programCounter]?.id}
        {insertBlocksAt}
        {moveBlock}
        {removeBlock}
        {setRegister}
        {toggleIndirect}
      />
    </div>
  </div>
</div>

{#if dialog === "objective"}
  <ObjectiveDialog {objective} {closeDialog} />
{:else if dialog === "help"}
  <HelpDialog {palette} {closeDialog} />
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
</style>