<script lang="ts">
  import { fade } from "svelte/transition";
  import Game from "./components/Game.svelte";
  import LevelSelector from "./components/LevelSelector.svelte";
  import { LEVELS } from "./lib/levels";
  import type { LevelInfo } from "./lib/types";

  let home = $state.raw(true);
  let levelId = $state.raw<string | null>(null); 
  let previousLevel: string | null = $state.raw(null);

  function startGame() {
    home = false;
  }

  const infos: Record<string, LevelInfo> = {};
  for (const level of LEVELS) {
    const data = localStorage.getItem(`coding-game--${level.id}`);
    if (data !== null) {
      const info = JSON.parse(data);
      infos[level.id] = info;
    } else {
      infos[level.id] = { completed: false, program: [], instructionCount: 0, stepCount: 0 };
    }
  }

  let levelInfos = $state.raw(infos);

  function saveInfo(fn: (info: LevelInfo) => LevelInfo) {
    if (levelId !== null) {
      const info = fn(levelInfos[levelId]);
      localStorage.setItem(`coding-game--${levelId}`, JSON.stringify(info));
      levelInfos = {...levelInfos, [levelId]: info }
    }
  }

  function handleQuitLevel() {
    previousLevel = levelId;
    levelId = null;
  }
  $inspect("previousLevel", previousLevel);

</script>

{#if home}
  <!-- svelte-ignore a11y_click_events_have_key_events -->
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <div transition:fade class="home" onclick={startGame}></div>
{:else if levelId === null}
  <div transition:fade>
    <LevelSelector
      levels={LEVELS}
      {previousLevel}
      {levelInfos}
      onSelectLevel={level => levelId = level.id}
    />
  </div>
{:else}
  <div transition:fade>
    <Game
      {levelId}
      levelInfo={levelInfos[levelId]}
      {saveInfo}
      onQuitLevel={handleQuitLevel}
    />
  </div>
{/if}

<style>
  .home {
    position: fixed;
    width: 100vw;
    height: 100dvh;
    background: url("../home.avif");
    background-size: cover;
    background-position: center;
    background-repeat: no-repeat;
  }
</style>