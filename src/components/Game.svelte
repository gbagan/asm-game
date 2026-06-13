<script lang="ts">
  import Editor from "./Editor.svelte";
  import Execution from "./Execution.svelte";
  import RetroBackground from "./RetroBackground.svelte";
</script>

<RetroBackground />
<div class="screen">
  <div class="game">
    <Execution />
    <Editor registers={["1", "2", "3", "4"]} />
  </div>
</div>


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
</style>