<script lang="ts">
  import Editor from "./components/Editor.svelte";
</script>

<div class="editor-container">
  <Editor registers={["1", "2"]} />
</div>

<style>
  .editor-container {
    width: 35rem;
  }
</style>


