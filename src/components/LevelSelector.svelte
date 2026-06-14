<script lang="ts">
  import type { Level, LevelInfo } from "../lib/types";
  import Block from "./Block.svelte";
  import LevelBackground from "./LevelBackground.svelte";

  type Props = {
    levels: Level[];
    levelInfos: Record<string, LevelInfo>
    onSelectLevel?: (level: Level) => void;
  };

  let {
    levels,
    levelInfos,
    onSelectLevel = () => {}
  }: Props = $props();

  let selectedLevelId: string | null = $derived(levels[0]?.id ?? null);

  let selectedLevel = $derived(
    levels.find((level) => level.id === selectedLevelId) ?? levels[0]
  );

  function selectLevel(level: Level) {
    selectedLevelId = level.id;
  }

  function startSelectedLevel() {
    if (!selectedLevel) return;
    onSelectLevel(selectedLevel);
  }
</script>

<div class="screen">
<LevelBackground />
<div class="level-selector">
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

  {#if selectedLevel}
    <section class="level-preview">
      <header class="preview-header">
        <div>
          <h2>{selectedLevel.title}</h2>
        </div>

        <button
          type="button"
          class="start-button"
          onclick={startSelectedLevel}
        >
          ▶ Commencer
        </button>
      </header>

      <div class="objective-card">
        <h3>Objectif</h3>
        <p>{selectedLevel.objective}</p>
      </div>

      <div class="preview-card">
        <h3>Blocs disponibles</h3>

        <div class="palette-row">
          {#each selectedLevel.palette as instruction}
            <span>
              <Block block={{id:"", type: instruction, fromPalette: true}} />
            </span>
          {/each}
        </div>
      </div>
          <div class="preview-card records-card">
      <h3>Records</h3>
      <div class="record-preview-grid">
        <div class="record-preview">
          <span class="record-icon">🏆 📜</span>
          <span class="record-label">Instructions</span>
          <strong>{levelInfos[selectedLevel.id].instructionCount && "—"}</strong>
        </div>

        <div class="record-preview">
          <span class="record-icon">🏆 ⏱</span>
         <span class="record-label">Étapes</span>
         <strong>{levelInfos[selectedLevel.id].stepCount && "—"}</strong>
        </div>
      </div>
    </div>
    </section>
  {:else}
    <section class="level-preview empty-preview">
      Aucun niveau disponible.
    </section>
  {/if}
</div>
</div>

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
  width: 60rem;
  margin: 0 auto;
  padding: 1.5rem;

  display: grid;
  grid-template-columns: 260px minmax(0, 1fr);
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
  margin: 0;
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
  display: grid;
  gap: 0.6rem;
}

.level-button {
  width: 100%;
  padding: 0.75rem 0.85rem;

  display: grid;
  gap: 0.2rem;

  border: 2px solid #cbd5e1;
  border-radius: 16px;
  background: linear-gradient(135deg, #f8fafc, #e2e8f0);
  color: #334155;

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
}

.level-button:hover {
    filter: brightness(1.02);
  transform: translateY(-2px);
  box-shadow:
    inset 0 -3px 0 rgb(0 0 0 / 0.08),
    0 8px 18px rgb(15 23 42 / 0.12);
}

.level-button.selected {
  border-color: #3b82f6;
  background: linear-gradient(135deg, #dbeafe, #93c5fd);
  color: #1e3a8a;
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
  border: 2px solid #22c55e;
  background: linear-gradient(135deg, #dcfce7, #86efac);
  color: #14532d;
}

.level-status.not-completed {
  border: 2px solid #94a3b8;
  background: linear-gradient(135deg, #f8fafc, #e2e8f0);
  color: #64748b;
}

.palette-row {
  display: flex;
  flex-wrap: wrap;
  gap: 0.45rem;
  height: 7rem;
}

.preview-status {
  display: inline-flex;
  align-items: center;
  gap: 0.25rem;

  margin-left: 0.5rem;
  padding: 0.25rem 0.55rem;

  border-radius: 999px;

  font-size: 0.8rem;
  font-weight: 900;
  vertical-align: middle;
}

.preview-status.completed {
  border: 2px solid #22c55e;
  background: #dcfce7;
  color: #14532d;
}

.preview-status.not-completed {
  border: 2px solid #94a3b8;
  background: #f1f5f9;
  color: #475569;
}

.level-preview {
  padding: 1.25rem;
  min-width: 0;
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

.start-button {
  padding: 0.75rem 1.15rem;
  border: 2px solid #22c55e;
  border-radius: 999px;

  background: linear-gradient(135deg, #dcfce7, #86efac);
  color: #14532d;

  font-size: 1rem;
  font-weight: 900;

  cursor: pointer;

  box-shadow:
    inset 0 -4px 0 rgb(0 0 0 / 0.12),
    0 6px 14px rgb(34 197 94 / 0.25);

  transition:
    transform 120ms ease,
    box-shadow 120ms ease,
    filter 120ms ease;
}

.start-button:hover {
  transform: translateY(-2px);
  filter: brightness(1.03);
  box-shadow:
    inset 0 -4px 0 rgb(0 0 0 / 0.12),
    0 10px 20px rgb(34 197 94 / 0.3);
}

.objective-card,
.preview-card {
  border-radius: 1rem;
  border: 2px solid rgb(226 232 240 / 0.9);
  background: rgb(248 250 252 / 0.8);
}

.objective-card {
  padding: 1rem;
  margin-bottom: 1rem;
}

.objective-card h3,
.preview-card h3 {
  margin-bottom: 0.65rem;
  font-size: 1rem;
}

.objective-card p {
  margin: 0;
  line-height: 1.5;
  color: #334155;
}

.preview-card {
  padding: 1rem;
  min-width: 0;
}

.empty-preview {
  display: grid;
  place-items: center;
  color: #64748b;
  font-weight: 800;
}

.record-preview-grid {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 0.75rem;
}

.record-preview {
  padding: 0.85rem;

  border-radius: 16px;
  border: 2px solid #cbd5e1;

  background: linear-gradient(135deg, #f8fafc, #e2e8f0);

  display: grid;
  justify-items: center;
  gap: 0.25rem;

  box-shadow:
    inset 0 -3px 0 rgb(0 0 0 / 0.06),
    0 4px 10px rgb(15 23 42 / 0.08);
}

.record-icon {
  font-size: 1.4rem;
}

.record-label {
  font-size: 0.8rem;
  color: #475569;
  font-weight: 800;
}

.record-preview strong {
  font-size: 1.8rem;
  line-height: 1;
  color: #1e293b;
}
</style>