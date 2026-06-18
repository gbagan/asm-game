<script lang="ts">
  import type { Level, LevelInfo } from "../lib/types";
  import Block from "./Block.svelte";
  import Button from "./Button.svelte";
    import ClearSavesDialog from "./ClearSavesDialog.svelte";
  import LevelBackground from "./LevelBackground.svelte";

  type Props = {
    levels: Level[];
    levelInfos: Record<string, LevelInfo>;
    previousLevel: string | null;
    onSelectLevel: (level: Level) => void;
    clearSaves: () => void;
  };

  let { levels, levelInfos, previousLevel, onSelectLevel, clearSaves }: Props = $props();
  let showClearDialog = $state.raw(false);

  let selectedLevelId: string | null = $derived(previousLevel ?? levels[0].id);

  let selectedLevel = $derived(
    levels.find(level => level.id === selectedLevelId)!
  );

  function selectLevel(level: Level) {
    selectedLevelId = level.id;
  }

  function startSelectedLevel() {
    onSelectLevel(selectedLevel);
  }


  function openClearDialog() {
    showClearDialog = true;
  }

  function closeClearDialog() {
    showClearDialog = false;
  }

  function confirmClearSaves() {
    showClearDialog = false;
    clearSaves();
  }
</script>

<div class="screen">
  <LevelBackground />
  <div class="level-selector">
    <button
      type="button"
      class="clear-saves-button"
      onclick={openClearDialog}
      title="Effacer les sauvegardes"
    >
      🗑
    </button>
    <aside class="level-list">
      <h2>Choisir un niveau</h2>

      <div class="level-buttons">
        {#each levels as level}
          <button
            type="button"
            class="level-button"
            class:selected={level.id === selectedLevelId}
            onclick={() => selectLevel(level)}
          >
            <span class="level-title-row">
              <span class="level-title">{level.title}</span>
              {#if levelInfos[level.id].completed}
                <span class="level-status completed" title="Niveau terminé">
                  ✓
                </span>
              {:else}
                <span class="level-status not-completed" title="Niveau non terminé">
                  ○
                </span>
              {/if}
            </span>
          </button>
        {/each}
      </div>
    </aside>

    <section class="level-preview">
      <header class="preview-header">
        <h2>{selectedLevel.title}</h2>

        <Button variant="green" onclick={startSelectedLevel}>
          ▶ Commencer
        </Button>
      </header>

      <div class="objective-card">
        <h3>Objectif</h3>
        <p>{selectedLevel.objective}</p>
      </div>

      <div class="preview-card">
        <h3>
          Blocs disponibles
          <span
            class="indirection-badge"
            class:enabled={selectedLevel.allowIndirect}
          >
          * Indirection {selectedLevel.allowIndirect ? "autorisée" : "désactivée"}
          </span>
        </h3>

        <div class="palette-row">
          {#each selectedLevel.palette as instruction}
            <span>
              <Block block={{id:"", type: instruction, fromPalette: true}} />
            </span>
          {/each}
        </div>
      </div>
      <div class="records-card">
        <h3>Records</h3>
        <div class="record-preview-grid">
          <div class="record-preview">
            <span class="record-icon">🏆 📜</span>
            <span class="record-label">Instructions</span>
            <strong>{levelInfos[selectedLevel.id].instructionCount || "—"}</strong>
          </div>

          <div class="record-preview">
            <span class="record-icon">🏆 ⏱</span>
            <span class="record-label">Étapes</span>
            <strong>{levelInfos[selectedLevel.id].stepCount || "—"}</strong>
          </div>
        </div>
      </div>
    </section>
  </div>
</div>
{#if showClearDialog}
  <ClearSavesDialog onOk={confirmClearSaves} closeDialog={closeClearDialog} />
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

  .level-selector {
    position: relative;
    width: 75rem;
    margin: 0 auto;
    padding: 1.5rem;

    display: flex;
    gap: 1.25rem;

    box-sizing: border-box;
  }

  .level-list,
  .level-preview {
    border-radius: 1.2rem;
    background: rgb(255 255 255 / 0.78);
    box-shadow: 0 10px 30px rgb(15 23 42 / 0.14);
    border: 2px solid rgb(203 213 225 / 0.8);
  }

  .level-list {
    padding: 1rem;
  }

  .level-list h2,
  .level-preview h2,
  .preview-card h3,
  .objective-card h3 {
    color: #1e293b;
  }

  .objective-card {
    height: 10rem;
  }

  .level-list h2 {
    margin-bottom: 1rem;
    font-size: 1.15rem;
    text-align: center;
  }

  .level-buttons {
    overflow-y: auto;
    overflow-x: hidden;
    height: 40rem;
    display: grid;
    grid-template-columns: 1fr;
    gap: 0.6rem;
  }

  .level-button {
    width: 11rem;
    padding: 0.75rem 0.85rem;

    display: grid;
    gap: 0.2rem;

    border: 2px solid var(--slate-300);
    border-radius: 1rem;
    background: linear-gradient(135deg, var(--slate-50), var(--slate-200));
    color: var(--slate-700);

    cursor: pointer;
    text-align: left;

    box-shadow:
      inset 0 -3px 0 rgb(0 0 0 / 0.08),
      0 4px 10px rgb(15 23 42 / 0.08);

    transition:
      transform 120ms ease,
      box-shadow 120ms ease,
      border-color 120ms ease,
      filter 120ms ease;
  
    &:hover {
      filter: brightness(1.02);
      transform: translateY(-2px);
      box-shadow:
        inset 0 -3px 0 rgb(0 0 0 / 0.08),
        0 8px 18px rgb(15 23 42 / 0.12);
    }
  
    &.selected {
      border-color: var(--blue-500);
      background: linear-gradient(135deg, var(--blue-100), var(--blue-300));
      color: var(--blue-900);
    }
  }

  .level-title {
    font-weight: 900;
    font-size: 1rem;
  }

  .level-title-row {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 0.5rem;
  }

  .level-status {
    width: 1.5rem;
    height: 1.5rem;
    border-radius: 999px;

    display: grid;
    place-items: center;
 
    flex: 0 0 auto;

    font-size: 0.9rem;
    font-weight: 900;

    box-shadow:
      inset 0 -2px 0 rgb(0 0 0 / 0.12),
      0 2px 6px rgb(15 23 42 / 0.12);
  }

  .level-status.completed {
    border: 2px solid var(--green-500);
    background: linear-gradient(135deg, var(--green-100), var(--green-300));
    color: var(--green-900);
  }

  .level-status.not-completed {
    border: 2px solid var(--slate-400);
    background: linear-gradient(135deg, var(--slate-50), var(--slate-200));
    color: var(--slate-500);
  }

  .palette-row {
    display: flex;
    flex-wrap: wrap;
    gap: 0.45rem;
    height: 8.5rem;
    pointer-events: none;
  }

  .level-preview {
    width: 45rem;
    padding: 1.25rem;
  }

  .preview-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 1rem;
    margin-bottom: 1rem;
  }

  .preview-header h2 {
    font-size: 1.6rem;
  }

  .objective-card,
  .preview-card,
  .records-card {
    border-radius: 1rem;
    border: 2px solid rgb(226 232 240 / 0.9);
    background: rgb(248 250 252 / 0.8);
    padding: 1rem;
  }

  .objective-card,
  .preview-card {
    margin-bottom: 1rem;
  }

  .objective-card h3,
  .preview-card h3,
  .records-card h3 {
    margin-bottom: 0.65rem;
    font-size: 1rem;
  }

  .objective-card p {
    line-height: 1.5;
    color: var(--slate-700);
  }

  .record-preview-grid {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 0.75rem;
  }

  .record-preview {
    padding: 0.85rem;

    border-radius: 0.9rem;
    border: 2px solid  var(--slate-300);

    background: linear-gradient(135deg,  var(--slate-50),  var(--slate-200));

    display: grid;
    justify-items: center;
    gap: 0.25rem;

    box-shadow:
      inset 0 -3px 0 rgb(0 0 0 / 0.06),
      0 4px 10px rgb(15 23 42 / 0.08);
  
    & strong {
      font-size: 1.8rem;
      line-height: 1;
      color: var(--slate-800);
      font-weight: 800;
    }
  }

  .record-icon {
    font-size: 1.4rem;
  }

  .record-label {
    font-size: 0.8rem;
    color: var(--slate-600);
    font-weight: 800;
  }

  .indirection-badge {
    display: inline-flex;
    align-items: center;
    gap: 0.35rem;
    width: fit-content;
    padding: 0.3rem 0.6rem;
    border-radius: 999px;
    background: var(--slate-100);
    border: 1px solid var(--slate-300);
    color: var(--slate-500);
    font-size: 0.8rem;
    font-weight: 900;
    &.enabled { 
      background: var(--amber-100);
      border-color: var(--amber-500);
      color: var(--amber-900);
    }
  }

  .clear-saves-button {
    position: absolute;
    left: -2rem;
    top: 1rem;
    z-index: 20;

    width: 3.5rem;
    height: 3.5rem;

    border: 2px solid var(--red-200);
    border-radius: 16px;

    background: linear-gradient(135deg, var(--red-100), var(--red-200));
    color: var(--red-800);

    font-size: 2rem;
    font-weight: 900;

    cursor: pointer;

    box-shadow:
      inset 0 -4px 0 rgb(0 0 0 / 0.1),
      0 6px 14px rgb(127 29 29 / 0.18);
  }

  .clear-saves-button:hover {
    transform: translateY(-1px);
    background: linear-gradient(135deg, var(--red-200), var(--red-300));
  }
</style>