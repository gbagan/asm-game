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

  border: 3px solid #ef4444;
  background: linear-gradient(135deg, #fff1f2, #fee2e2);
  color: #450a0a;

  box-shadow:
    0 20px 50px rgb(15 23 42 / 0.35),
    inset 0 -5px 0 rgb(0 0 0 / 0.08);
}

.execution-error-header {
  background: linear-gradient(135deg, #fee2e2, #fca5a5);
  border-bottom: 2px solid #ef4444;
}

.execution-error-header h2 {
  margin: 0;
  font-size: 1.25rem;
  color: #7f1d1d;
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
  border-top: 1px solid rgb(239 68 68 / 0.25);
}
</style>