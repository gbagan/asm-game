<script lang="ts">
    import { sleep } from "@gbagan/utils";
  import { tick } from "svelte";

  type Instruction =
    | { type: "input" }
    | { type: "output" }
    | { type: "copy-to"; register: number }
    | { type: "copy-from"; register: number }
    | { type: "add"; register: number };

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
    initialInput?: number[];
    maxRegisters?: number;
  };

  let {
    initialInput = [7, 3, 12, 5, 9],
    maxRegisters = 12
  }: Props = $props();

  let container = $state<HTMLDivElement | undefined>();

  let inputSlots: Array<HTMLDivElement | undefined> = [];
  let outputSlots: Array<HTMLDivElement | undefined> = [];
  let registerSlots: Array<HTMLDivElement | undefined> = [];
  let currentSlot = $state<HTMLDivElement | undefined>();
  let calcLeftSlot = $state<HTMLDivElement | undefined>();
  let calcRightSlot = $state<HTMLDivElement | undefined>();
  let calcResultSlot = $state<HTMLDivElement | undefined>();

  let showCalcArea = $state(false);

  let isMoving = $state(false);
  let tokenPositions = $state<Record<string, Point>>({});

  let tokens: NumberToken[] = $derived(
    initialInput.map((value, index) => ({
      id: crypto.randomUUID(),
      value,
      location: { kind: "input", index }
    }))
  );

  let registers = $derived(
    Array.from({ length: maxRegisters }, (_, index) => index)
  );

  function visibleTokens() {
    return tokens.filter((token) => token.location.kind !== "hidden");
  }

  function inputTokens() {
    return tokens
      .filter(token => token.location.kind === "input")
      .sort((a, b) => a.location.index - b.location.index);
  }

  function outputTokens() {
    return tokens
      .filter((token) => token.location.kind === "output")
      .sort((a, b) => a.location.index - b.location.index);
  }

  function currentToken() {
    return tokens.find((token) => token.location.kind === "current");
  }

  function registerToken(registerIndex: number) {
    return tokens.find(
      (token) =>
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
      .sort((a, b) => a.location.index - b.location.index)
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

  async function executeInstruction(instruction: Instruction) {
    if (isMoving) return;

    isMoving = true;

    if (instruction.type === "input") {
      const firstInputToken = inputTokens()[0];
      if (!firstInputToken) {
        isMoving = false;
        return;
      }

      hideTokenAt("current");
      await moveToken(firstInputToken.id, { kind: "current" });
    }

    if (instruction.type === "output") {
      const token = currentToken();
      if (!token) {
        isMoving = false;
        return;
      }

      pushOutputTokensDown(token.id);
      await moveToken(token.id, {
        kind: "output",
        index: 0
      });
    }

    if (instruction.type === "copy-to") {
      const token = currentToken();
      if (!token) {
        isMoving = false;
        return;
      }

      hideTokenAt("register", instruction.register);

      await createCopyAndMove(
        token.value,
        { kind: "current" },
        { kind: "register", index: instruction.register }
      );
    }

    if (instruction.type === "copy-from") {
      const token = registerToken(instruction.register);
      if (!token) {
        isMoving = false;
        return;
      }

      hideTokenAt("current");

      await createCopyAndMove(
        token.value,
        { kind: "register", index: instruction.register },
        { kind: "current" }
      );
    }

    if (instruction.type === "add") {
      const current = currentToken();
      const register = registerToken(instruction.register);

      if (!current || !register) {
        isMoving = false;
        return;
      }

      showCalcArea = true;
      await tick();
      await updateTokenPositions();

      const currentCopyId = await createTokenAt(
        current.value,
        { kind: "current" }
      );

      const registerCopyId = await createTokenAt(
        register.value,
        { kind: "register", index: instruction.register }
      );

      await Promise.all([
        moveToken(currentCopyId, { kind: "calc", side: "left" }),
        moveToken(registerCopyId, { kind: "calc", side: "right" })
      ]);

      await sleep(250);
      hideTokens([currentCopyId, registerCopyId]);
      const result = current.value + register.value;

      const resultId = await createTokenAt(
        result,
        { kind: "calc", side: "result" }
      );

      await sleep(250);
      hideTokenAt("current");
      await moveToken(resultId, { kind: "current" });
      await sleep(250);
      showCalcArea = false;
    }

    isMoving = false;
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
          {#each registers as registerIndex (registerIndex)}
            <div class="register-cell">
              <div class="register-label">R{registerIndex + 1}</div>

              <div
                class="slot register-slot"
                bind:this={registerSlots[registerIndex]}
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

    {#each visibleTokens() as token (token.id)}
      {#if tokenPositions[token.id]}
        <div
          class="number-token"
          style:left="{tokenPositions[token.id].x}px"
          style:top="{tokenPositions[token.id].y}px"
        >
          {token.value}
        </div>
      {/if}
    {/each}
  </div>
  <div class="controls">
    <button class="button start">Lancer</button>
    <button class="button pause">Pause</button>
    <button class="button fast">Accélerer</button>
  </div>
</div>

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

.button {
  --button-bg: #e2e8f0;
  --button-bg-2: #cbd5e1;
  --button-border: #94a3b8;
  --button-text: #0f172a;
  --button-shadow: rgb(15 23 42 / 0.18);

  min-width: 8rem;
  padding: 0.75rem 1.15rem;

  border: 2px solid var(--button-border);
  border-radius: 999px;

  background: linear-gradient(135deg, var(--button-bg), var(--button-bg-2));
  color: var(--button-text);

  font-size: 1rem;
  font-weight: 800;
  letter-spacing: 0.02em;

  cursor: pointer;
  user-select: none;

  box-shadow:
    inset 0 -4px 0 rgb(0 0 0 / 0.12),
    0 6px 14px var(--button-shadow);

  transition:
    transform 120ms ease,
    box-shadow 120ms ease,
    filter 120ms ease;
}

.button:hover:not(:disabled) {
  transform: translateY(-2px);
  filter: brightness(1.04);
  box-shadow:
    inset 0 -4px 0 rgb(0 0 0 / 0.12),
    0 10px 20px var(--button-shadow);
}

.button:active:not(:disabled) {
  transform: translateY(1px);
  box-shadow:
    inset 0 -2px 0 rgb(0 0 0 / 0.16),
    0 3px 8px var(--button-shadow);
}

.button:disabled {
  opacity: 0.45;
  cursor: default;
  filter: grayscale(0.25);
}

  /* Bouton lancer */
  .button.start {
    --button-bg: #dcfce7;
    --button-bg-2: #86efac;
    --button-border: #22c55e;
    --button-text: #14532d;
    --button-shadow: rgb(34 197 94 / 0.28);
  }

/* Bouton pause */
.button.pause {
  --button-bg: #fef3c7;
  --button-bg-2: #fcd34d;
  --button-border: #f59e0b;
  --button-text: #78350f;
  --button-shadow: rgb(245 158 11 / 0.28);
}

/* Bouton arrêter */
.button.fast {
  --button-bg: #fee2e2;
  --button-bg-2: #fca5a5;
  --button-border: #ef4444;
  --button-text: #7f1d1d;
  --button-shadow: rgb(239 68 68 / 0.28);
}

/* Bouton étape suivante */
.button.step {
  --button-bg: #dbeafe;
  --button-bg-2: #93c5fd;
  --button-border: #3b82f6;
  --button-text: #1e3a8a;
  --button-shadow: rgb(59 130 246 / 0.28);
}

</style>