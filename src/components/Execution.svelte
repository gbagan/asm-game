<script lang="ts">
  import { count, dec, filterMap, inc, sleep, update } from "@gbagan/utils";
  import { tick } from "svelte";
  import type { InstructionBlock, LevelInfo, ProgramBlock, RegisterBlock } from "../lib/types";
  import { playDiscardSound, playFailureSound, playStepSound, playVictorySound } from "../lib/sound";
  import Button from "./Button.svelte";
  import SuccessDialog from "./SuccessDialog.svelte";
  import ExecutionErrorDialog from "./ExecutionErrorDialog.svelte";

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
    discarding?: boolean;
    discardDx?: number;
    discardDy?: number;
    discardRotation?: number;
  };

  type Point = {
    x: number;
    y: number;
  };

  type Props = {
    program: ProgramBlock[];
    initialInput: number[];
    registers: (number | null)[];
    expectedOutput: number[];
    programCounter: number | null
    setProgramCounter: (pc: number | null) => void;
    onQuitLevel: () => void;
    saveInfo: (fn: (info: LevelInfo) => LevelInfo) => void;
    checkTests: () => boolean;
  };

  let { program, programCounter, initialInput, registers, expectedOutput,
    setProgramCounter, onQuitLevel, saveInfo, checkTests }: Props = $props();

  let container: HTMLDivElement;
  let inputSlots: (HTMLDivElement | undefined)[] = $state([]);
  let outputSlots: (HTMLDivElement | undefined)[] = $state([]);
  let registerSlots: (HTMLDivElement | undefined)[] = $state([]);
  let currentSlot: HTMLDivElement;
  let calcLeftSlot: HTMLDivElement;
  let calcRightSlot: HTMLDivElement;
  let calcResultSlot: HTMLDivElement;

  let version = $state.raw(0);
  let showCalcArea: "+" | "-" | null = $state.raw(null);
  let tokenPositions = $state.raw<Record<string, Point>>({});

  let running: "stopped" | "running" | "pending" = $state.raw("stopped");
  let fastMode = $derived.by(() => {
    version;
    program;
    return false;
  });

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
    version;
    program;
    return initialTokens;
  });

  let stepCount = $derived.by(() => {
    program;
    version;
    return 0;
  });

  let errorMessage: string | null = $state.raw(null);
  let successDialog = $state.raw(false);

  let modifiedRegister: [number, "inc" | "dec"] | null = $state.raw(null);

  let instructionCount = $derived(count(program, b => b.kind === "instruction"));

  async function delay(ms: number) {
    await sleep(fastMode ? ms / 10 : ms);
  }

  function playSound(sound: "step" | "discard" | "victory" | "failure") {
    if (sound === "step" && !fastMode) {
      playStepSound();
    } else if (sound === "discard" && !fastMode) {
      playDiscardSound();
    } else if (sound === "victory") {
      playVictorySound();
    } else if (sound === "failure") {
      playFailureSound();
    }
  }

  function incrementProgramCounter() {
    if (programCounter !== null) {
      setProgramCounter(programCounter + 1)
    }
  }

  function closeExecutionErrorDialog() {
    errorMessage = null;
    version += 1;
  }

  function visibleTokens() {
    return tokens.filter((token) => token.location.kind !== "hidden");
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

  function registerIndex(block: RegisterBlock) {
    if (!block.indirect) {
      return block.register;
    }
    const token = registerToken(block.register);
    if (token === undefined) {
      throw Error("Le registre est vide");
    }
    if (token.value < 0 && token.value > registers.length) {
      throw Error("Le registre ne contient pas une addresse valide")
    }
    return token.value; 
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
    updateTokenPositions();
  });

  $effect(() => {
    version;
    setProgramCounter(null);
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

  function modifyToken(id: string, f: (x: number) => number) {
    const idx = tokens.findIndex(token => token.id === id);
    if (idx === -1) return;
    tokens = update(tokens, [idx, "value"], f);
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

  async function moveToken(tokenId: string, location: TokenLocation) {
    tokens = normalizeInputIndexes(
      tokens.map((token) =>
        token.id === tokenId
          ? { ...token, location }
          : token
      )
    );

    await updateTokenPositions();
    await delay(700);
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

  async function discardToken(tokenId: string) {
    const direction = Math.random() < 0.5 ? -1 : 1;

    const discardDx = direction * (120 + Math.random() * 80);
    const discardDy = -90 - Math.random() * 60;
    const discardRotation = direction * (260 + Math.random() * 120);

    tokens = tokens.map(token =>
      token.id === tokenId
        ? {
          ...token,
          discarding: true,
          discardDx,
          discardDy,
          discardRotation
          }
        : token
    );

    await delay(520);

    tokens = tokens.map((token) =>
      token.id === tokenId
        ? {
          ...token,
          location: { kind: "hidden" },
          discarding: false,
          discardDx: undefined,
          discardDy: undefined,
          discardRotation: undefined
        }
        : token
    );
  }

  async function discardCurrentToken() {
    const token = currentToken();
    if (!token) return;
    playSound("discard");
    await discardToken(token.id);
  }

  async function discardRegisterToken(index: number) {
    const token = registerToken(index);
    if (!token) return;
    playSound("discard");
    await discardToken(token.id);
  }

  async function executeOperation(block: RegisterBlock, name: string, symbol: "+" | "-", op: (a: number, b: number) => number) {
    const current = currentToken();
    const index = registerIndex(block);
    const register = registerToken(index);

    if (!current) throw new Error(`Tente de faire une ${name} alors que la valeur courante est vide`);
    if (!register) throw new Error(`Tente de faire une ${name} avec un registre vide`);

    showCalcArea = symbol;
    await tick();
    await updateTokenPositions();

    playSound("step");

    const registerCopyId = await createToken(
      register.value,
      { kind: "register", index }
    );

    await Promise.all([
      moveToken(current.id, { kind: "calc", side: "left" }),
      moveToken(registerCopyId, { kind: "calc", side: "right" })
    ]);

    playSound("step");
    hideTokens([current.id, registerCopyId]);
    const result = op(current.value, register.value);

    const resultId = await createToken(
      result,
      { kind: "calc", side: "result" }
    );

    await delay(400);
    hideTokenAt("current");
    await moveToken(resultId, { kind: "current" });
    await delay(250);
    showCalcArea = null;
    incrementProgramCounter();
  }

  async function bumpRegister(instr: InstructionBlock, fn: (val: number) => number) {
    if (instr.type !== "dec" && instr.type !== "inc") {
      throw new Error("Unexpected error");
    }

    const index = registerIndex(instr);
    const token = registerToken(index);
   
    if (!token) {
      throw new Error("Le registre est vide")
    }
    playSound("step");
    modifyToken(token.id, fn);
    modifiedRegister = [index, instr.type];
    await delay(600);
    modifiedRegister = null;
    await discardCurrentToken();
    playSound("step");
    await createCopyAndMove(
      fn(token.value),
      { kind: "register", index },
      { kind: "current" }
    );
    await delay(250);
    incrementProgramCounter();
  }

  async function executeInstruction(instruction: ProgramBlock) {
    if (instruction.kind === "jump-target") {
      incrementProgramCounter();
      // ne compte pas comme une étape, pour compenser l'incrément
      stepCount -= 1;
    } else if (instruction.type === "input") {
      const firstInputToken = inputTokens()[0];
      if (!firstInputToken) {
        throw new Error("Tente d'exécuter l'instruction Input alors que l'entrée est vide");
      }

      await discardCurrentToken();

      playSound("step");
      await moveToken(firstInputToken.id, { kind: "current" });
      await delay(400);
      incrementProgramCounter();
    } else if (instruction.type === "output") {
      const token = currentToken();
      if (!token) {
        throw new Error("Tente d'exécuter l'instruction Output alors que la valeur courante est vide");
      }

      pushOutputTokensDown(token.id);
      playSound("step");
      await moveToken(token.id, {
        kind: "output",
        index: 0
      });
      await delay(400);
      let output = outputTokens();
      const actual = output[0].value;
      const expected = expectedOutput[output.length - 1];
      if (actual !== expected) {
        throw new Error(`La valeur ${actual} n'était pas attendue dans l'Output`);
      }

      incrementProgramCounter();
    } else if (instruction.type === "copy-to") {
      const token = currentToken();
      const index = registerIndex(instruction);
      if (!token) {
        throw new Error("Tente de copier la valeur courante alors qu'elle est vide");
      }
      await discardRegisterToken(index);
      playSound("step");
      await createCopyAndMove(
        token.value,
        { kind: "current" },
        { kind: "register", index }
      );
      await delay(250);
      incrementProgramCounter();
    } else if (instruction.type === "copy-from") {
      const index = registerIndex(instruction);
      const token = registerToken(index);
      if (!token) {
        throw new Error("Tente de récupérer un registre vide");
      }

      await discardCurrentToken();
      playSound("step");
      await createCopyAndMove(
        token.value,
        { kind: "register", index },
        { kind: "current" }
      );
      await delay(250);
      incrementProgramCounter();
    } else if (instruction.type === "add") {
      await executeOperation(instruction, "addition", "+", (a, b) => a + b);
    } else if (instruction.type === "sub") {
      await executeOperation(instruction, "soustraction", "-", (a, b) => a - b);
    } else if (instruction.type === "jump") {
      playSound("step");
      setProgramCounter(program.findIndex(b => b.id === instruction.targetId));
    } else if (instruction.type === "jump-if-zero") {
      const token = currentToken();
      if (!token) {
        throw new Error("Effectue un Jump If Zero alors que la valeur courante est vide")
      }
      playSound("step");
      if (token.value === 0) {
        setProgramCounter(program.findIndex(b => b.id === instruction.targetId));
      } else {
        incrementProgramCounter();
      }
    } else if (instruction.type === "jump-if-negative") {
      const token = currentToken();
      if (!token) {
        throw new Error("Effectue un Jump If Negative alors que la valeur courante est vide")
      }
      playSound("step");
      if (token.value < 0) {
        setProgramCounter(program.findIndex(b => b.id === instruction.targetId));
      } else {
        incrementProgramCounter();  
      }
    } else if (instruction.type === "inc") {
      await bumpRegister(instruction, inc);
    } else if (instruction.type === "dec") {
      await bumpRegister(instruction, dec);
    }
      
  }

  async function createToken(value: number, location: TokenLocation) {
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
    tokens = tokens.map(token => {
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
    return outputTokens().length === expectedOutput.length;
  }

  async function step() {
    if (programCounter === null) {
      setProgramCounter(0);
    }
    try {
      if (programCounter !== null && programCounter >= program.length) {
        throw new Error("Le programme s'est terminé avant d'avoir terminé sa tache");
      }
      await executeInstruction(program[programCounter ?? 0]);
    } catch (e) {
      playSound("failure");
      errorMessage = (e as Error).message;
      return false;
    }
    stepCount += 1;
    if (isSuccess()) {
      if (!checkTests()) {
        playSound("failure");
        errorMessage = "Ton programme est valide sur cette entrée mais ne donne pas la bonne réponse sur d'autres entrées";
        return false;
      }

      completeLevel();
      playSound("victory");
      successDialog = true;
      return false;
    }
    if (running === "pending") {
      return false;
    }
    return true;
  }

  async function oneStep() {
    running = "running";
    await step();
    running = "stopped"
  }

  async function run() {
    saveInfo(info => ({...info, program}));
    running = "running";

    while (running === "running") {
      const cont = await step();
      if (!cont) {
        running = "stopped";
        return;
      }
      await delay(300);
    }
    running = "stopped";
  }

  async function stop() {
    if (running === "running") {
      running = "pending";
    }
    while (running === "pending") {
      await delay(100);
    }
    version += 1;
    setProgramCounter(null);
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
    version += 1;
  }
</script>

<svelte:window onresize={updateTokenPositions} />

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

      <div class="calc-area" class:calc-area-visible={!!showCalcArea}>
        <div class="slot calc-slot" bind:this={calcLeftSlot}></div>
        <div class="operator">{showCalcArea}</div>
        <div class="slot calc-slot" bind:this={calcRightSlot}></div>
        <div class="operator">=</div>
        <div class="slot calc-slot result-slot" bind:this={calcResultSlot}></div>
      </div>

      {#if registers.length > 0}
        <div class="registers-area">
          <div class="register-grid">
            {#each registers as _, index}
              {@const isModified = modifiedRegister && modifiedRegister[0] === index}
              <div class="register-cell">
                <div class="register-label">{index}</div>
                <div
                  class="slot register-slot"
                  class:incremented={isModified && modifiedRegister![1] === "inc"}
                  class:decremented={isModified && modifiedRegister![1] === "dec"}
                  bind:this={registerSlots[index]}
                >
                  {#if isModified && modifiedRegister![1] === "inc"}
                    <span class="modify-bubble increment-bubble">+1</span>
                  {:else if isModified && modifiedRegister![1] === "dec"}
                    <span class="modify-bubble decrement-bubble">-1</span>
                  {/if}
                </div>
              </div>
            {/each}
          </div>
        </div>
      {/if}
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
      {@const pos = tokenPositions[token.id]}
      {#if pos}
        <div
          class="number-token"
          class:token-discarding={token.discarding}
          style:left="{pos.x}px"
          style:top="{pos.y}px"
          style:--discard-dx="{token.discardDx ?? 0}px"
          style:--discard-dy="{token.discardDy ?? 0}px"
          style:--discard-rotation="{token.discardRotation ?? 0}deg"
        >
          {token.value}
        </div>
      {/if}  
    {/each}
  </div>
  <div class="controls">
    <Button variant="green" disabled={running !== "stopped"} onclick={run}>Lancer</Button>
    <Button variant="yellow" disabled={running === "stopped"} onclick={() => running = "pending"}>Pause</Button>
    <Button variant="red" disabled={running === "pending"} onclick={stop}>Arrêter</Button>
    <Button variant="gray" disabled={running !== "running"} onclick={() => fastMode = !fastMode}>Accélerer</Button>
    <Button variant="blue" disabled={running !== "stopped"} onclick={oneStep}>Pas à pas</Button>
  </div>
</div>
{#if errorMessage !== null}
  <ExecutionErrorDialog message={errorMessage} closeDialog={closeExecutionErrorDialog} />
{/if}
{#if successDialog}
  <SuccessDialog {stepCount} {instructionCount} {restartLevel} quitLevel={onQuitLevel} />
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
  grid-template-columns: 7rem 1fr 7rem;
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
  width: 32rem;
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
  grid-template-columns: repeat(5, 5rem);
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
    border: 0.25rem dashed var(--rose-400);
    background: color-mix(in srgb, var(--rose-100) 50%, transparent);
  }

  .register-slot {
    position: relative;
    width: 4.5rem;
    height: 4.5rem;
    border-radius: 1.1rem;
    background: linear-gradient(135deg, var(--violet-100), var(--violet-200));
    border: 4px solid var(--violet-500);
  
    &.incremented {
      animation: register-increment-flash 600ms ease-out;
    }

    &.decremented {
      animation: register-decrement-flash 600ms ease-out;
    }
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

    background: linear-gradient(135deg, var(--blue-100), var(--blue-300));
    border: 3px solid var(--blue-600);
    color: var(--blue-900);

    font-size: 1.55rem;
    font-weight: 900;

    box-shadow:
      inset 0 -4px 0 rgb(0 0 0 / 0.12),
      0 4px 10px rgb(15 23 42 / 0.14);

    pointer-events: none;
  
    &.token-discarding {
      animation: throw-away 520ms cubic-bezier(0.25, 0.8, 0.3, 1) forwards;
    }
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

  @keyframes throw-away {
    0% {
      opacity: 1;
      transform:
        translate(-50%, -50%)
        translate(0, 0)
        rotate(0deg)
        scale(1);
      filter: none;
    }

    35% {
      opacity: 1;
      transform:
        translate(-50%, -50%)
        translate(calc(var(--discard-dx) * 0.45), calc(var(--discard-dy) * 1.15))
        rotate(calc(var(--discard-rotation) * 0.35))
        scale(1.04);
      filter: none;
    }

    100% {
      opacity: 0;
      transform:
        translate(-50%, -50%)
        translate(var(--discard-dx), calc(var(--discard-dy) + 170px))
        rotate(var(--discard-rotation))
        scale(0.55);
      filter: grayscale(1);
    }
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
    border: 3px dashed var(--slate-300);
    background: rgb(248 250 252 / 0.85);
  }

  .result-slot {
    border-color: var(--rose-400);
    background: color-mix(in srgb, var(--rose-100) 55%, transparent);
  }

  .operator {
    font-weight: 900;
    color: var(--slate-700);
    font-size: 1.8rem;
    line-height: 1;
  }

  .modify-bubble {
    position: absolute;
    top: -1.1rem;
    right: -0.8rem;
    z-index: 30;

    padding: 0.15rem 0.45rem;
    border-radius: 999px;

    font-size: 0.9rem;
    font-weight: 900;
    pointer-events: none;
  }

  .increment-bubble {
    background: var(--green-100);
    border: 2px solid var(--green-500);
    color: var(--green-800);
    animation: increment-bubble-pop 600ms ease-out forwards;
  }

  @keyframes register-increment-flash {
    0% {
      transform: scale(1);
      box-shadow: none;
      border-color: var(--slate-300);
      background: rgb(248 250 252 / 0.85);
    }

    25% {
      transform: scale(1.08);
      box-shadow:
        0 0 0 5px rgb(34 197 94 / 0.22),
        0 8px 18px rgb(34 197 94 / 0.22);
      border-color: var(--green-500);
      background: var(--green-100);
    }

    100% {
      transform: scale(1);
      box-shadow: none;
      border-color: var(--slate-300);
      background: rgb(248 250 252 / 0.85);
    }
  }

  .decrement-bubble {
    background: var(--red-100);
    border: 2px solid var(--red-500);
    color: var(--red-800);
    animation: modify-bubble-pop 600ms ease-out forwards;
  }

  @keyframes register-decrement-flash {
    0% {
      transform: scale(1);
      box-shadow: none;
      border-color: var(--slate-300);
      background: rgb(248 250 252 / 0.85);
    }

    25% {
      transform: scale(0.94);
      box-shadow:
        0 0 0 5px r0 8px 18px color-mix(in srgb, var(--red-500) 28%, transparent),
        0 8px 18px color-mix(in srgb, var(--red-500) 18%, transparent);
      border-color: var(--red-500);
      background: var(--red-100);
    }

    60% {
      transform: scale(1.04);
    }

    100% {
      transform: scale(1);
      box-shadow: none;
      border-color: var(--slate-300);
      background: rgb(248 250 252 / 0.85);
    }
  }

  @keyframes modify-bubble-pop {
    0% {
      opacity: 0;
      transform: translateY(0) scale(0.6);
    }

    20% {
      opacity: 1;
      transform: translateY(-4px) scale(1.12);
    }

    100% {
      opacity: 0;
      transform: translateY(22px) scale(0.85);
    }
  }

  .controls {
    display: flex;
    justify-content: center;
    align-items: center;
    gap: 0.75rem;
    flex-wrap: wrap;
  }
</style>