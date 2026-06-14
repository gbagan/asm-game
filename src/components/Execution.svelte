<script lang="ts">
  import { count, filterMap, sleep } from "@gbagan/utils";
  import { tick } from "svelte";
  import type { LevelInfo, ProgramBlock } from "../lib/types";
  import { playFailureSound, playStepSound, playVictorySound } from "../lib/sound";
  import { fade } from "svelte/transition";
  import Button from "./Button.svelte";

  type TokenLocation =
    | { kind: "input"; index: number }
    | { kind: "current" }
    | { kind: "register"; index: number }
    | { kind: "output"; index: number }
    | { kind: "hidden" }
    | { kind: "calc"; side: "left" | "right" | "result" }

  type NumberToken = {
    id: string;
    value: number;
    location: TokenLocation;
  };

  type Point = {
    x: number;
    y: number;
  };

  type Props = {
    program: ProgramBlock[];
    initialInput: number[];
    registers: (number | null)[];
    registerNames: string[];
    expectedOutput: number[];
    setProgramCounter: (pc: number) => void;
    onQuitLevel: () => void;
    saveInfo: (fn: (info: LevelInfo) => LevelInfo) => void;
  };

  let { program, initialInput, registers, registerNames, expectedOutput,
    setProgramCounter, onQuitLevel, saveInfo }: Props = $props();

  let container = $state<HTMLDivElement | undefined>();
  let inputSlots: Array<HTMLDivElement | undefined> = $state([]);
  let outputSlots: Array<HTMLDivElement | undefined> = $state([]);
  let registerSlots: Array<HTMLDivElement | undefined> = $state([]);
  let currentSlot = $state<HTMLDivElement | undefined>();
  let calcLeftSlot = $state<HTMLDivElement | undefined>();
  let calcRightSlot = $state<HTMLDivElement | undefined>();
  let calcResultSlot = $state<HTMLDivElement | undefined>();

  let layoutVersion = $state(0);
  let showCalcArea = $state(false);
  let tokenPositions = $state<Record<string, Point>>({});

  let running: "stopped" | "running" | "pending" = $state("stopped");

  let initialTokens: NumberToken[] = $derived.by(() => {
    return [
      ...initialInput.map((value, index) => ({
        id: "i"+index,
        value,
        location: { kind: "input", index }
      })) as NumberToken[],
      ...filterMap(registers, (value, index) =>
        value === null 
        ? null 
        : {
          id: "r"+index,
          value,
          location: { kind: "register", index}
        }
      ) as NumberToken[]
    ]
  });

  let tokens: NumberToken[] = $derived.by(() => {
    layoutVersion;
    program;
    return initialTokens;
  });

  let pc = $derived.by(() => {
    program;
    initialInput;
    layoutVersion;
    return 0
  });

  let stepCount = $derived.by(() => {
    program;
    initialInput;
    layoutVersion;
    return 0;
  });

  let executionErrorMessage = $state<string | null>(null);
  let successDialog = $state(false);

  let instructionCount = $derived(count(program, b => b.kind === "instruction"));

  function closeExecutionErrorDialog() {
    executionErrorMessage = null;
    layoutVersion += 1;
    setProgramCounter(0);
  }

  function visibleTokens() {
    return tokens.filter((token) => token.location.kind !== "hidden");
  }

  function safeVisibleTokens() {
    return visibleTokens().filter(token => tokenPositions[token.id])
  }

  function inputTokens() {
    return tokens
      .filter(token => token.location.kind === "input")
      .sort((a, b) => (a.location as any).index - (b.location as any).index);
  }

  function outputTokens() {
    return tokens
      .filter((token) => token.location.kind === "output")
      .sort((a, b) => (a.location as any).index - (b.location as any).index);
  }

  function currentToken() {
    return tokens.find(token => token.location.kind === "current");
  }

  function registerToken(registerIndex: number) {
    return tokens.find(token =>
      token.location.kind === "register" &&
      token.location.index === registerIndex
    );
  }

  function getSlotForLocation(location: TokenLocation): HTMLElement | undefined {
    if (location.kind === "input") {
      return inputSlots[location.index];
    }

    if (location.kind === "current") {
      return currentSlot;
    }

    if (location.kind === "register") {
      return registerSlots[location.index];
    }

    if (location.kind === "output") {
      return outputSlots[location.index];
    }

    if (location.kind === "calc") {
      if (location.side === "left") return calcLeftSlot;
      if (location.side === "right") return calcRightSlot;
      return calcResultSlot;
    }

    return undefined;
  }

  function getCenterPosition(element: HTMLElement): Point {
    if (!container) return { x: 0, y: 0 };

    const containerRect = container.getBoundingClientRect();
    const rect = element.getBoundingClientRect();

    return {
      x: rect.left - containerRect.left + rect.width / 2,
      y: rect.top - containerRect.top + rect.height / 2
    };
  }

  async function updateTokenPositions() {
    await tick();

    const nextPositions: Record<string, Point> = {};

    for (const token of visibleTokens()) {
      const slot = getSlotForLocation(token.location);
      if (!slot) continue;

      nextPositions[token.id] = getCenterPosition(slot);
    }

    tokenPositions = nextPositions;
  }

  $effect(() => {
    tokens;
    container;
    updateTokenPositions();
  });

  function normalizeInputIndexes(nextTokens: NumberToken[]): NumberToken[] {
    const idsInInputOrder = nextTokens
      .filter(token => token.location.kind === "input")
      .sort((a, b) => (a.location as any).index - (b.location as any).index)
      .map(token => token.id);

    return nextTokens.map((token) => {
      if (token.location.kind !== "input") return token;

      return {
        ...token,
        location: {
          kind: "input",
          index: idsInInputOrder.indexOf(token.id)
        }
      };
    });
  }

  function hideTokenAt(locationKind: "current" | "register", index?: number) {
    tokens = tokens.map((token) => {
      if (locationKind === "current" && token.location.kind === "current") {
        return { ...token, location: { kind: "hidden" } };
      }

      if (
        locationKind === "register" &&
        token.location.kind === "register" &&
        token.location.index === index
      ) {
        return { ...token, location: { kind: "hidden" } };
      }

      return token;
    });
  }

  async function waitForAnimation() {
    await new Promise((resolve) => window.setTimeout(resolve, 700));
  }

  async function moveToken(tokenId: string, location: TokenLocation) {
    tokens = normalizeInputIndexes(
      tokens.map((token) =>
        token.id === tokenId
          ? { ...token, location }
          : token
      )
    );

    await updateTokenPositions();
    await waitForAnimation();
  }

  async function createCopyAndMove(
    value: number,
    from: TokenLocation,
    to: TokenLocation
  ) {

    const copyId = crypto.randomUUID();

    tokens = [
      ...tokens,
      {
        id: copyId,
        value,
        location: from
      }
    ];

    await updateTokenPositions();
    await tick();

    await moveToken(copyId, to);
  }

  async function executeOperation(name: string, registerId: number, op: (a: number, b: number) => number) {
    const current = currentToken();
    const register = registerToken(registerId);

    if (!current) throw new Error(`Tente de faire une ${name} alors que la valeur courante est vide`);
    if (!register) throw new Error(`Tente de faire une ${name} avec un registre vide`);

    showCalcArea = true;
    await tick();
    await updateTokenPositions();

    playStepSound();

    const registerCopyId = await createTokenAt(
      register.value,
      { kind: "register", index: registerId }
    );

    await Promise.all([
      moveToken(current.id, { kind: "calc", side: "left" }),
      moveToken(registerCopyId, { kind: "calc", side: "right" })
    ]);

    playStepSound();
    hideTokens([current.id, registerCopyId]);
    const result = op(current.value, register.value);

    const resultId = await createTokenAt(
      result,
      { kind: "calc", side: "result" }
    );

    await sleep(400);
    hideTokenAt("current");
    await moveToken(resultId, { kind: "current" });
    await sleep(250);
    showCalcArea = false;
    pc += 1;
  }

  async function executeInstruction(instruction: ProgramBlock) {
    if (instruction.kind === "jump-target") {
      pc += 1;
      // ne compte pas comme une étape, pour compenser l'incrément
      stepCount -= 1;
    } else if (instruction.type === "input") {
      const firstInputToken = inputTokens()[0];
      if (!firstInputToken) {
        throw new Error("Tente d'exécuter l'instruction Input alors que l'entrée est vide");
      }

      hideTokenAt("current");
      playStepSound();
      await moveToken(firstInputToken.id, { kind: "current" });
      await sleep(400);
      pc += 1;
    } else if (instruction.type === "output") {
      const token = currentToken();
      if (!token) {
        throw new Error("Tente d'exécuter l'instruction Output alors que la valeur courante est vide");
      }

      pushOutputTokensDown(token.id);
      playStepSound();
      await moveToken(token.id, {
        kind: "output",
        index: 0
      });
      await sleep(400);
      let output = outputTokens();
      const actual = output[0].value;
      const expected = expectedOutput[output.length - 1];
      if (actual !== expected) {
        throw new Error(`La valeur ${actual} n'était pas attendue dans l'Output`);
      }

      pc += 1;
    } else if (instruction.type === "copy-to") {
      const token = currentToken();
      if (!token) {
        throw new Error("Tente de copier la valeur courante alors qu'elle est vide");
      }
      hideTokenAt("register", instruction.register);
      playStepSound();
      await createCopyAndMove(
        token.value,
        { kind: "current" },
        { kind: "register", index: instruction.register! }
      );
      await sleep(250);
      pc += 1;
    } else if (instruction.type === "copy-from") {
      const token = registerToken(instruction.register!);
      if (!token) {
        throw new Error("Tente de copier un registre vide");
      }

      hideTokenAt("current");
      playStepSound();
      await createCopyAndMove(
        token.value,
        { kind: "register", index: instruction.register! },
        { kind: "current" }
      );
      await sleep(250);
      pc += 1;
    } else if (instruction.type === "add") {
      await executeOperation("addition", instruction.register!, (a, b) => a + b);
    } else if (instruction.type === "sub") {
      await executeOperation("soustraction", instruction.register!, (a, b) => a - b);
    } else if (instruction.type === "jump") {
      playStepSound();
      pc = program.findIndex(b => b.id === instruction.targetId);
    } else if (instruction.type === "jump-if-zero") {
      const token = currentToken();
      if (!token) {
        throw new Error("Effectue un Jump If Zero alors que la valeur courante est vide")
      }
      playStepSound();
      if (token.value === 0) {
        pc = program.findIndex(b => b.id === instruction.targetId);
      } else {
        pc += 1;  
      }
    }
  }

  async function createTokenAt(value: number, location: TokenLocation) {
    const id = crypto.randomUUID();
    tokens = [
      ...tokens,
      { id, value, location }
    ];
    await updateTokenPositions();
    await tick();
    return id;
  }

  function hideTokens(tokenIds: string[]) {
    tokens = tokens.map((token) =>
      tokenIds.includes(token.id)
        ? { ...token, location: { kind: "hidden" } }
        : token
    );
  }

  function pushOutputTokensDown(exceptTokenId?: string) {
    tokens = tokens.map((token) => {
      if (token.id === exceptTokenId) return token;
      if (token.location.kind !== "output") return token;
      return {
        ...token,
        location: {
          kind: "output",
          index: token.location.index + 1
        }
      };
    });
  }

  function isSuccess() {
    let input = inputTokens();
    let output = outputTokens();
    return input.length === 0 && output.length === expectedOutput.length;
  }

  async function step() {
    try {
      if (pc >= program.length) {
        throw new Error("Le programme s'est terminé avant d'avoir terminé sa tache");
      }
      await executeInstruction(program[pc]);
    } catch (e) {
      playFailureSound();
      executionErrorMessage = (e as Error).message;
      running = "stopped";
      return false;
    }
    setProgramCounter(pc);
    stepCount += 1;
    if (isSuccess()) {
      completeLevel();
      playVictorySound();
      successDialog = true; // todo
      running = "stopped";
      return false;
    }
    if (running === "pending") {
      running = "stopped";
      return false;
    }
    return true;
  }

  async function run() {
    setProgramCounter(0);
    running = "running";

    while (running === "running") {
      const cont = await step();
      if (!cont) return;
      await sleep(300);
    }
  }

  async function stop() {
    if (running === "running") {
      running = "pending";
    }
    while (running === "pending") {
      await sleep(100);
    }
    layoutVersion += 1;
  }

  function completeLevel() {
    saveInfo(info => {
      const ic = 
        info.instructionCount === 0 
        ? instructionCount 
        : Math.min(info.instructionCount, instructionCount);
      const sc = 
        info.stepCount === 0 
        ? stepCount
        : Math.min(info.stepCount, stepCount);
      return {...info, completed: true, program, instructionCount: ic, stepCount: sc};
    });
  }

  function restartLevel() { 
    successDialog = false;
    layoutVersion += 1;
  }
</script>

<div class="container">
  <div class="execution-view" bind:this={container}>
    <section class="panel input-panel">
      <h2>Input</h2>

      <div class="number-list">
        {#each inputTokens() as token, index (token.id)}
          <div class="slot" bind:this={inputSlots[index]}></div>
        {/each}
      </div>
    </section>

    <section class="center-panel">
      <div class="current-area">
        <h2>Valeur courante</h2>

        <div
          class="slot current-slot"
          bind:this={currentSlot}
        ></div>
      </div>

      <div class="calc-area" class:calc-area-visible={showCalcArea}>
        <div class="slot calc-slot" bind:this={calcLeftSlot}></div>
        <div class="operator">+</div>
        <div class="slot calc-slot" bind:this={calcRightSlot}></div>
        <div class="operator">=</div>
        <div class="slot calc-slot result-slot" bind:this={calcResultSlot}></div>
      </div>

      <div class="registers-area">
        <h2>Registres</h2>

        <div class="register-grid">
          {#each registerNames as name, index}
            <div class="register-cell">
              <div class="register-label">{name}</div>

              <div
                class="slot register-slot"
                bind:this={registerSlots[index]}
              ></div>
            </div>
          {/each}
        </div>
      </div>
    </section>

    <section class="panel output-panel">
      <h2>Output</h2>

      <div class="number-list">
        {#each outputTokens() as token, index (token.id)}
          <div class="slot" bind:this={outputSlots[index]}></div>
        {/each}
      </div>
    </section>

    {#each safeVisibleTokens() as token (token.id)}
        <div
          transition:fade
          class="number-token"
          style:left="{tokenPositions[token.id].x}px"
          style:top="{tokenPositions[token.id].y}px"
        >
          {token.value}
        </div>
    {/each}
  </div>
  <div class="controls">
    <Button variant="green" disabled={running !== "stopped"} onclick={run}>Lancer</Button>
    <Button variant="yellow" disabled={running === "stopped"} onclick={() => running = "pending"}>Pause</Button>
    <Button variant="red" disabled={running === "pending"} onclick={stop}>Arrêter</Button>
    <Button variant="blue" disabled={running !== "stopped"} onclick={step}>Pas à pas</Button>
  </div>
</div>
{#if executionErrorMessage !== null}
  <div
    class="execution-error-backdrop"
    role="presentation"
    onclick={closeExecutionErrorDialog}
  >
    <dialog
      class="dialog execution-error-dialog"
      open
      aria-labelledby="execution-error-title"
      onclick={event => event.stopPropagation()}
    >
      <header class="execution-error-header">
        <h2 id="execution-error-title">
          Erreur d’exécution
        </h2>

        <button
          class="execution-error-close"
          type="button"
          onclick={closeExecutionErrorDialog}
          aria-label="Fermer"
        >
          ×
        </button>
      </header>

      <div class="execution-error-content">
        <p>{executionErrorMessage}</p>
      </div>

      <footer class="execution-error-footer">
        <button
          class="execution-error-ok"
          type="button"
          onclick={closeExecutionErrorDialog}
        >
          Compris
        </button>
      </footer>
    </dialog>
  </div>
{/if}
{#if successDialog}
  <div
    class="success-dialog-backdrop"
    role="presentation"
  >
    <dialog
      class="dialog success-dialog"
      open
      aria-labelledby="success-dialog-title"
    >
      <header class="success-dialog-header">
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

      <footer class="success-dialog-footer">
        <Button variant="blue" onclick={restartLevel}>
          ↻ Recommencer
        </Button>
        <Button variant="green" onclick={onQuitLevel}>
          🏠 Choisir un niveau
        </Button>
      </footer>
    </dialog>
  </div>
{/if}

<style>
  .container {
    display: flex;
    flex-direction: column;
    gap: 1rem;
  }

.execution-view {
  position: relative;
  height: 45rem;
  margin: 0 auto;
  padding: 2rem;

  display: grid;
  grid-template-columns: 8rem 1fr 8rem;
  gap: 2rem;

  border-radius: 24px;
  background: rgb(255 255 255 / 0.7);
  overflow: hidden;
  box-sizing: border-box;
}

.panel,
.center-panel {
  border-radius: 1.25rem;
  padding: 1rem;
  background: rgb(255 255 255 / 0.75);
  box-shadow: 0 8px 24px rgb(15 23 42 / 0.12);
}

h2 {
  margin: 0 0 1rem;
  text-align: center;
  font-size: 1.2rem;
  color: #1e293b;
}

.number-list {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0.75rem;
}

.center-panel {
  width: 25rem;
  height: 42rem;
  display: grid;
  grid-template-rows: auto 7.5rem 1fr auto;
  gap: 1rem;
}

.current-area {
  display: grid;
  place-items: center;
}

.registers-area {
  display: grid;
  place-items: center;
}

.register-grid {
  display: grid;
  grid-template-columns: repeat(4, 5rem);
  gap: 1rem;
}

.register-cell {
  display: grid;
  justify-items: center;
  gap: 0.35rem;
}

.register-label {
  font-weight: 800;
  color: #334155;
}

.slot {
  width: 4rem;
  height: 4rem;
  border-radius: 16px;
  box-sizing: border-box;
}


.current-slot {
  width: 5rem;
  height: 5rem;
  border: 0.25rem dashed #fb7185;
  background: rgb(255 228 230 / 0.5);
}

.register-slot {
  width: 4.5rem;
  height: 4.5rem;
  border-radius: 1.1rem;
  background: linear-gradient(135deg, #ede9fe, #ddd6fe);
  border: 4px solid #8b5cf6;
}

  .number-token {
    position: absolute;
    z-index: 20;

    width: 3.5rem;
    height: 3.5rem;
    border-radius: 1rem;

    display: grid;
    place-items: center;

    transform: translate(-50%, -50%);
    transition:
      left 700ms ease-in-out,
      top 700ms ease-in-out;

    background: linear-gradient(135deg, #dbeafe, #93c5fd);
    border: 3px solid #2563eb;
    color: #1e3a8a;

    font-size: 1.55rem;
    font-weight: 900;

    box-shadow:
      inset 0 -4px 0 rgb(0 0 0 / 0.12),
      0 4px 10px rgb(15 23 42 / 0.14);

    pointer-events: none;
  }

  .calc-area {
    justify-self: center;
    align-self: center;

    min-height: 6rem;
    box-sizing: border-box;

    display: grid;
    grid-template-columns: 72px auto 72px auto 72px;
    align-items: center;
    gap: 0.75rem;

    padding: 0.75rem 1rem;
    border-radius: 1rem;

    background: rgb(255 255 255 / 0.25);
    border: 2px solid transparent;

    opacity: 0;

    transition:
      opacity 160ms ease,
      background 160ms ease,
      border-color 160ms ease,
      box-shadow 160ms ease,
      transform 160ms ease;
  }

  .calc-area-visible {
    opacity: 1;
    background: rgb(255 255 255 / 0.75);
    border-color: rgb(148 163 184 / 0.45);
    box-shadow: 0 8px 18px rgb(15 23 42 / 0.12);
    transform: scale(1.02);
  }

  .calc-slot {
    width: 4rem;
    height: 4rem;
    border: 3px dashed #cbd5e1;
    background: rgb(248 250 252 / 0.85);
  }

  .result-slot {
    border-color: #fb7185;
    background: rgb(255 228 230 / 0.55);
  }

  .operator {
    font-weight: 900;
    color: #334155;
    font-size: 1.8rem;
    line-height: 1;
  }


  .controls {
    display: flex;
    justify-content: center;
    align-items: center;
    gap: 0.75rem;
    flex-wrap: wrap;
  }

.execution-error-backdrop {
  position: fixed;
  inset: 0;
  z-index: 1000;

  display: grid;
  place-items: center;

  background: rgb(15 23 42 / 0.55);
  backdrop-filter: blur(3px);
}

.dialog {
  position: fixed;
  left: 50%;
  top: 50%;
  transform: translate(-50%, -50%);  
  overflow: hidden;
  animation: dialog-pop 160ms ease-out;
}

.execution-error-dialog {

  width: min(520px, calc(100vw - 2rem));

  border: 3px solid #ef4444;
  border-radius: 22px;

  background: linear-gradient(135deg, #fff1f2, #fee2e2);
  color: #450a0a;

  box-shadow:
    0 20px 50px rgb(15 23 42 / 0.35),
    inset 0 -5px 0 rgb(0 0 0 / 0.08);
}

.execution-error-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 1rem;

  padding: 1rem 1.25rem;

  background: linear-gradient(135deg, #fee2e2, #fca5a5);
  border-bottom: 2px solid #ef4444;
}

.execution-error-header h2 {
  margin: 0;
  font-size: 1.25rem;
  color: #7f1d1d;
}

.execution-error-close {
  width: 2rem;
  height: 2rem;

  border: none;
  border-radius: 999px;

  background: rgb(255 255 255 / 0.65);
  color: #7f1d1d;

  font-size: 1.4rem;
  font-weight: 900;
  line-height: 1;

  cursor: pointer;
}

.execution-error-close:hover {
  background: white;
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
  display: flex;
  justify-content: flex-end;

  padding: 1rem 1.25rem;
  border-top: 1px solid rgb(239 68 68 / 0.25);
}

.execution-error-ok {
  padding: 0.6rem 1rem;

  border: 2px solid #ef4444;
  border-radius: 999px;

  background: linear-gradient(135deg, #fee2e2, #fca5a5);
  color: #7f1d1d;

  font-weight: 800;
  cursor: pointer;

  box-shadow:
    inset 0 -3px 0 rgb(0 0 0 / 0.12),
    0 4px 10px rgb(239 68 68 / 0.22);
}

.execution-error-ok:hover {
  filter: brightness(1.03);
}

@keyframes dialog-pop {
  from {
    opacity: 0;
    transform: translate(-50%, calc(-50%-0.5rem)) scale(0.92);
  }

  to {
    opacity: 1;
    transform: translate(-50%, -50%) translateY(0);
  }
}

.success-dialog-backdrop {
  position: fixed;
  inset: 0;
  z-index: 1000;

  display: grid;
  place-items: center;

  background: rgb(15 23 42 / 0.55);
  backdrop-filter: blur(3px);
}

.success-dialog {
  width: 28rem;

  border: 3px solid #22c55e;
  border-radius: 24px;

  background: linear-gradient(135deg, #f0fdf4, #dcfce7);
  color: #14532d;

  box-shadow:
    0 20px 50px rgb(15 23 42 / 0.35),
    inset 0 -5px 0 rgb(0 0 0 / 0.08);
}

.success-dialog-header {
  display: flex;
  align-items: center;
  gap: 1rem;

  padding: 1.25rem;

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
  color: #166534;
  font-weight: 700;
  text-align: center;
}

.success-stat-value {
  font-size: 2rem;
  line-height: 1;
  color: #14532d;
}

.success-dialog-footer {
  display: flex;
  justify-content: flex-end;
  gap: 0.75rem;
  flex-wrap: wrap;

  padding: 1rem 1.25rem;
  border-top: 1px solid rgb(34 197 94 / 0.25);
}
</style>