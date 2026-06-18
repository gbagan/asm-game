<script lang="ts">
  import Button from "./Button.svelte";

  type Props = {
    objective: string;
    closeDialog: () => void;
  }

  let { objective, closeDialog }: Props = $props();

</script>
<div class="global-dialog-backdrop" role="presentation" onclick={closeDialog}>
  <dialog
    class="global-dialog objective-dialog"
    open
    onclick={(event) => event.stopPropagation()}
  >
    <header class="global-dialog-header header">
      <h2>Objectif du niveau</h2>

      <button
        class="global-dialog-close-button"
        type="button"
        onclick={closeDialog}
        aria-label="Fermer"
      >
        ×
      </button>
    </header>

    <div class="global-dialog-content">
      <p>{objective}</p>
    </div>

    <footer class="global-dialog-footer footer">
      <Button variant="green" onclick={closeDialog}>
        Compris !
      </Button>
    </footer>
  </dialog>
</div>

<style>
  .objective-dialog {
    width: 30rem;
    border: 3px solid var(--amber-500);
    background: linear-gradient(135deg, var(--orange-50), var(--amber-50));
    color: var(--amber-950);
  }

  .header {
    background: linear-gradient(135deg, var(--amber-100), var(--amber-200));
    border-bottom: 2px solid var(--amber-500);
  }

  .header h2 {
    color: var(--amber-900);
  }

  .footer {
    border-top: 1px solid rgb(245 158 11 / 0.25);
  }
</style>