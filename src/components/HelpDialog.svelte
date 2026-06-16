<script lang="ts">
    import type { InstructionType } from "../lib/types";
    import Button from "./Button.svelte";

  type Props = {
    palette: InstructionType[];
    closeDialog: () => void;
  }

  let { palette, closeDialog }: Props = $props();

  function hasInstruction(type: InstructionType) {
    return palette.includes(type);
  }
</script>

<div class="global-dialog-backdrop" role="presentation" onclick={closeDialog}>
  <dialog
    class="global-dialog help-dialog"
    open
    onclick={(event) => event.stopPropagation()}
  >
    <header class="global-dialog-header help-header">
      <h2>Aide : les instructions</h2>

      <button
        class="global-dialog-close-button"
        onclick={closeDialog}
        aria-label="Fermer"
      >
        ×
      </button>
    </header>

    <div class="help-content">
      <div class="game-help-intro">
        <h3>Principe du jeu</h3>
        <p>Le but du jeu est de construire un programme qui réalise l’objectif du niveau.</p>
        <p>
          Pour créer ton programme, fais glisser les blocs d’instructions dans la zone
          d’édition, puis lance l’exécution pour vérifier le résultat. Tu peux déplacer
          les blocs, ajouter des sauts avec des flèches, et utiliser les registres pour
          mémoriser des valeurs.
        </p>
      </div>
      <section
        class="instruction-help io-help"
        class:disabled-help={!hasInstruction("input")}
      >
        <h3>Input</h3>
        <p>
          Prend le premier nombre de l’input et le place dans la valeur courante.
        </p>
      </section>

      <section
        class="instruction-help io-help"
        class:disabled-help={!hasInstruction("output")}
      >
        <h3>Output</h3>
        <p>
          Envoie la valeur courante vers l’output.
        </p>
      </section>

      <section
        class="instruction-help jump-help"
        class:disabled-help={!hasInstruction("jump")}
      >
        <h3>Jump</h3>
        <p>
          Saute directement vers la cible indiquée par la flèche.
        </p>
      </section>
      <section
        class="instruction-help jump-help"
        class:disabled-help={!hasInstruction("jump-if-zero")}
      >
        <h3>Jump If Zero</h3>
        <p>
          Saute directement vers la cible indiquée par la flèche si la valeur courante est égale à 0.
        </p>
      </section>
      <section
        class="instruction-help jump-help"
        class:disabled-help={!hasInstruction("jump-if-negative")}
      >
        <h3>Jump If Negative</h3>
        <p>
          Saute directement vers la cible indiquée par la flèche si la valeur courante est inférieure à 0.
        </p>
      </section>

      <section
        class="instruction-help memory-help"
        class:disabled-help={!hasInstruction("copy-from")}
      >
        <h3>Copy From <span class="instruction-argument">i</span></h3>
        <p>
          Copie la valeur du registre <strong>i</strong> dans la valeur courante.
        </p>
      </section>

      <section
        class="instruction-help memory-help"
        class:disabled-help={!hasInstruction("copy-to")}
      >
        <h3>Copy To <span class="instruction-argument">i</span></h3>
        <p>
          Copie la valeur courante dans le registre <strong>i</strong>.
        </p>
      </section>

      <section
        class="instruction-help arithmetic-help"
        class:disabled-help={!hasInstruction("add")}
      >
        <h3>Add <span class="instruction-argument">i</span></h3>
        <p>
          Additionne la valeur courante avec la valeur du registre <strong>i</strong>,
          puis place le résultat dans la valeur courante.
        </p>
      </section>
      <section
        class="instruction-help arithmetic-help"
        class:disabled-help={!hasInstruction("sub")}
      >
        <h3>Sub <span class="instruction-argument">i</span></h3>
        <p>
          Soustrait la valeur du registre <strong>i</strong> à la valeur courante,
          puis place le résultat dans la valeur courante.
        </p>
      </section>
      <section
        class="instruction-help arithmetic-help"
        class:disabled-help={!hasInstruction("inc")}
      >
        <h3>Inc <span class="instruction-argument">i</span></h3>
        <p>
          Ajoute 1 au registre <strong>i</strong>,
          puis fais-en une copie dans la valeur courante.
        </p>
      </section>
      <section
        class="instruction-help arithmetic-help"
        class:disabled-help={!hasInstruction("dec")}
      >
        <h3>Dec <span class="instruction-argument">i</span></h3>
        <p>
          Soustrais 1 au registre <strong>i</strong>,
          puis fais-en une copie dans la valeur courante.
        </p>
      </section>
      <section
        class="instruction-help"
        class:disabled-help={true}
      >
        <h3>Indirection</h3>
        <p>
          Quand activée, une instruction sur le registre <strong>i</strong> utilise plutôt le registre
          dont le numéro est contenu dans le registre <strong>i</strong>.
        </p>
      </section>
    </div>

    <footer class="global-dialog-footer">
      <Button variant="green" onclick={closeDialog}>
        Compris !
      </Button>
    </footer>
  </dialog>
</div>

<style>
  .help-dialog {
    width: 75rem;

    border: 3px solid #3b82f6;
    border-radius: 22px;

    background: linear-gradient(135deg, #eff6ff, #f8fafc);
    color: #0f172a;

    box-shadow:
      0 20px 50px rgb(15 23 42 / 0.35),
      inset 0 -5px 0 rgb(0 0 0 / 0.08);

    overflow: hidden;

    animation: dialog-pop 160ms ease-out;
  }

  .help-header {
    background: linear-gradient(135deg, #dbeafe, #93c5fd);
    border-bottom-color: #3b82f6;
  }

  .help-header h2 {
    color: #1e3a8a;
  }

  .help-content {
    padding: 1.25rem;
    display: grid;
    grid-template-columns: repeat(4, minmax(0, 1fr));
    gap: 0.85rem;
    overflow-y: auto;
  }

  .game-help-intro {
    grid-column: 1 / -1;
    padding: 1rem 1.1rem;
    border-radius: 1rem;
    border: 2px solid #3b82f6;
    background: linear-gradient(135deg, #eff6ff, #dbeafe);
    color: #1e3a8a;
    box-shadow:
      inset 0 -3px 0 rgb(0 0 0 / 0.06),
      0 4px 10px rgb(15 23 42 / 0.08);
  }

  .game-help-intro h3 {
    margin: 0 0 0.5rem;
    font-size: 1.1rem;
    font-weight: 900;
  }

  .game-help-intro p {
    line-height: 1.5;
    font-weight: 600;
  }

  .game-help-intro p + p {
    margin-top: 0.5rem;
  }

  .instruction-help {
    padding: 0.9rem 1rem;
    border-radius: 0.9rem;
    border: 2px solid #cbd5e1;
    background: white;

    box-shadow:
      inset 0 -3px 0 rgb(0 0 0 / 0.06),
      0 4px 10px rgb(15 23 42 / 0.08);
  }

  .instruction-help h3 {
    margin: 0 0 0.4rem;

    display: flex;
    align-items: center;
    gap: 0.4rem;

    font-size: 1.05rem;
    font-weight: 900;
  }

  .instruction-help p {
    margin: 0;
    line-height: 1.45;
    color: #334155;
  }

  .instruction-argument {
    min-width: 1.5rem;
    height: 1.5rem;
    padding: 0 0.35rem;

    display: inline-grid;
    place-items: center;

    border-radius: 999px;
    background: rgb(255 255 255 / 0.75);
    border: 2px solid rgb(0 0 0 / 0.12);

    font-size: 0.85rem;
    font-weight: 900;
  }

  .io-help {
    border-color: var(--io-border);
    background: var(--io-bg);
    color: var(--io-color);
  }

  .jump-help {
    border-color: var(--jump-border);
    background: var(--jump-bg);
    color: var(--jump-color);
  }

  .memory-help {
    border-color: #8b5cf6;
    background: linear-gradient(135deg, #ede9fe, #ddd6fe);
    color: #3b0764;
  }

  .arithmetic-help {
    border-color: var(--arith-border);
    background: var(--arith-bg);
    color: var(--arith-color);
  }

  .instruction-help.disabled-help {
    filter: grayscale(1);
    opacity: 0.42;
    background: #f1f5f9;
    border-color: #cbd5e1;
    color: #64748b;
  }

  .instruction-help.disabled-help h3,
  .instruction-help.disabled-help p {
    color: #64748b;
  }

  .instruction-help.disabled-help .instruction-argument {
    background: #e2e8f0;
    border-color: #cbd5e1;
    color: #64748b;
  }
</style>