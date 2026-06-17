<script lang="ts">
  import Button from "./Button.svelte";

  type Props = {
    instructionCount: number;
    stepCount: number;
    restartLevel: () => void;
    quitLevel: () => void;
  }

  let { instructionCount, stepCount, restartLevel, quitLevel }: Props = $props();
</script>

<div class="global-dialog-backdrop">
  <dialog class="global-dialog success-dialog" open>
    <header class="global-dialog-header success-dialog-header">
      <div class="success-icon">✓</div>

      <div>
        <h2 id="success-dialog-title">
          Niveau réussi !
        </h2>

        <p>Bravo, ton programme produit la bonne sortie.</p>
      </div>
    </header>

    <div class="success-dialog-content">
      <div class="success-stat">
        <span class="success-stat-label">
          Instructions utilisées
        </span>

        <strong class="success-stat-value">
          {instructionCount}
        </strong>
      </div>

      <div class="success-stat">
        <span class="success-stat-label">
          Étapes exécutées
        </span>

        <strong class="success-stat-value">
          {stepCount}
        </strong>
      </div>
    </div>

    <footer class="global-dialog-footer success-dialog-footer">
      <Button variant="blue" onclick={restartLevel}>
        ↻ Recommencer
      </Button>
      <Button variant="green" onclick={quitLevel}>
        🏠 Choisir un niveau
      </Button>
    </footer>
  </dialog>
</div>

<style>
.success-dialog {
  width: 28rem;

  border: 3px solid #22c55e;

  background: linear-gradient(135deg, #f0fdf4, #dcfce7);
  color: #14532d;

  box-shadow:
    0 20px 50px rgb(15 23 42 / 0.35),
    inset 0 -5px 0 rgb(0 0 0 / 0.08);
}

.success-dialog-header {
  background: linear-gradient(135deg, #dcfce7, #86efac);
  border-bottom: 2px solid #22c55e;
}

.success-icon {
  width: 3rem;
  height: 3rem;

  display: grid;
  place-items: center;

  border-radius: 999px;
  border: 3px solid #22c55e;

  background: white;
  color: #15803d;

  font-size: 1.8rem;
  font-weight: 900;

  box-shadow:
    inset 0 -3px 0 rgb(0 0 0 / 0.12),
    0 4px 10px rgb(34 197 94 / 0.25);
}

.success-dialog-header h2 {
  margin: 0;
  font-size: 1.5rem;
  color: #14532d;
}

.success-dialog-header p {
  margin: 0.25rem 0 0;
  color: #166534;
  font-weight: 600;
}

.success-dialog-content {
  padding: 1.25rem;

  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 0.9rem;
}

.success-stat {
  padding: 1rem;

  border-radius: 18px;
  border: 2px solid rgb(34 197 94 / 0.35);

  background: rgb(255 255 255 / 0.75);

  display: grid;
  gap: 0.4rem;
  justify-items: center;

  box-shadow:
    inset 0 -3px 0 rgb(0 0 0 / 0.06),
    0 4px 10px rgb(15 23 42 / 0.08);
}

.success-stat-label {
  font-size: 0.9rem;
  color: var(--green-800);
  font-weight: 700;
  text-align: center;
}

.success-stat-value {
  font-size: 2rem;
  line-height: 1;
  color: var(--green-900);
}

.success-dialog-footer {
  border-top: 1px solid rgb(34 197 94 / 0.25);
}
</style>