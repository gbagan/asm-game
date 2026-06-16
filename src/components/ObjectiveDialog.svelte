<script lang="ts">
  import Button from "./Button.svelte";

  type Props = {
    objective: string;
    closeDialog: () => void;
  }

  let { objective, closeDialog }: Props = $props();

</script>
<div class="dialog-backdrop" role="presentation" onclick={closeDialog}>
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
    border: 3px solid #f59e0b;

    background: linear-gradient(135deg, #fff7ed, #fffbeb);
    color: #431407;

    box-shadow:
      0 20px 50px rgb(15 23 42 / 0.35),
      inset 0 -5px 0 rgb(0 0 0 / 0.08);
  }

  .header {
    background: linear-gradient(135deg, #fef3c7, #fde68a);
    border-bottom: 2px solid #f59e0b;
  }

  .header h2 {
    color: #78350f;
  }

  .footer {
    border-top: 1px solid rgb(245 158 11 / 0.25);
  }
</style>