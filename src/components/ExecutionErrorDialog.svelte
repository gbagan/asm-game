<script lang="ts">
  import Button from "./Button.svelte";

  type Props = {
    message: String;
    closeDialog: () => void;
  }

  let { message, closeDialog }: Props = $props();
</script>

<div
  class="global-dialog-backdrop"
  role="presentation"
  onclick={closeDialog}
>
  <dialog
    class="global-dialog execution-error-dialog"
    open
    onclick={event => event.stopPropagation()}
  >
    <header class="global-dialog-header execution-error-header">
      <h2 id="execution-error-title">
        Erreur d’exécution
      </h2>
    </header>

    <div class="execution-error-content">
      <p>{message}</p>
    </div>

    <footer class="global-dialog-footer execution-error-footer">
      <Button variant="red" onclick={closeDialog}>
        Compris
      </Button>
    </footer>
  </dialog>
</div>

<style>
.execution-error-dialog {
  width: 30rem;
  border: 3px solid var(--red-500);
  background: linear-gradient(135deg, var(--rose-50), var(--red-100));
  color: var(--red-950);
}

.execution-error-header {
  background: linear-gradient(135deg, var(--red-100), var(--red-300));
  border-bottom: 2px solid var(--red-500);
}

.execution-error-header h2 {
  margin: 0;
  font-size: 1.25rem;
  color: var(--red-900);
}

.execution-error-content {
  padding: 1.25rem;
  font-size: 1rem;
  line-height: 1.5;
}

.execution-error-content p {
  margin: 0;
  font-weight: 700;
}

.execution-error-footer {
  border-top: 1px solid color-mix(in srgb, var(--red-500) 25%, transparent);
}
</style>