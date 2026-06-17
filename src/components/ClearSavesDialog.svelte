<script lang="ts">
  type Props = {
    closeDialog: () => void;
    onOk: () => void;
  }

  let { closeDialog, onOk }: Props = $props();
</script>

<!-- svelte-ignore a11y_click_events_have_key_events -->
<!-- svelte-ignore a11y_no_static_element_interactions -->
<div class="global-dialog-backdrop" onclick={closeDialog}>
  <dialog
    class="global-dialog clear-saves-dialog"
    open
    onclick={event => event.stopPropagation()}
  >
    <h2 id="clear-saves-title">Effacer les sauvegardes ?</h2>

    <p>
      Cette action supprimera tous les niveaux terminés et tous les records.
      Elle est définitive.
    </p>

    <div class="dialog-actions">
      <button
        type="button"
        class="dialog-button cancel-button"
        onclick={closeDialog}
      >
        Annuler
      </button>

      <button
        type="button"
        class="dialog-button danger-button"
        onclick={onOk}
      >
        Effacer
      </button>
    </div>
  </dialog>
</div>

<style>
  .clear-saves-dialog {
    width: 40rem;
    padding: 1.5rem;

    border: 3px solid var(--red-300);
    background: linear-gradient(135deg, var(--orange-50), var(--red-100));
    color: var(--red-900);
    box-shadow: 0 18px 40px rgb(15 23 42 / 0.28);
  }

  .clear-saves-dialog h2 {
    margin: 0 0 0.75rem;

    font-size: 1.5rem;
    font-weight: 900;
  }

  .clear-saves-dialog p {
    line-height: 1.5;
    font-weight: 700;
  }

  .dialog-actions {
    margin-top: 1.4rem;
    display: flex;
    justify-content: flex-end;
    gap: 0.75rem;
  }

  .dialog-button {
    padding: 0.7rem 1rem;
    border-radius: 14px;
    border: none;
    font-size: 0.95rem;
    font-weight: 900;
    cursor: pointer;
  }

  .cancel-button {
    background: var(--slate-100);
    color: var(--slate-700);
    border: 2px solid var(--slate-300);
 }

  .danger-button {
    background: linear-gradient(135deg, var(--red-500), var(--red-600));
    color: white;

    box-shadow:
      inset 0 -3px 0 rgb(0 0 0 / 0.14),
      0 5px 12px rgb(220 38 38 / 0.22);
  }
</style>