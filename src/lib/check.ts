import { type InstructionBlock, type ProgramBlock } from "./types";

export function checkProgramRun(program: ProgramBlock[], initRegisters: (number | null)[],
  input: number[], expectedOutput: number[], maxSteps: number)
{
  const registers = [...initRegisters];
  let inputIndex = 0;
  let outputIndex = 0;
  let programCounter = 0;
  let currentValue: number | null = null;
  let stepCount = 0;

  function registerIndex(block: InstructionBlock) {
    const index = block.register!;
    if (!block.indirect) {
      return index
    } else if (index < 0 || index >= registers.length) {
      throw new Error();
    }
    if (registers[index] === null) {
      throw new Error();
    } 
    return registers[index];
  }

  function target(block: InstructionBlock) {
    return program.findIndex(b => b.id === block.targetId);
  }

  try {
    while (true) {
      if (programCounter >= program.length) return false;
      if (stepCount > maxSteps) return false;
      // console.log(programCounter, registers, currentValue, input[inputIndex], outputIndex);
      const block = program[programCounter];
      if (block.kind === "jump-target") {
        programCounter += 1;
        stepCount -= 1;
      } else if (block.type === "input") {
        if (inputIndex >= input.length) {
          return false;
        }
        currentValue = input[inputIndex];
        inputIndex++;
        programCounter += 1;
      } else if (block.type === "output") {
        if (currentValue !== expectedOutput[outputIndex]) return false;
        outputIndex++;
        if (outputIndex === expectedOutput.length) {
          return true;
        }
        currentValue = null;
        programCounter += 1;
      } else if (block.type === "copy-from") {
        const index = registerIndex(block);
        if (registers[index] === null) {
          return false;
        }
        currentValue = registers[index];
        programCounter += 1;
      } else if (block.type === "copy-to") {
        if (currentValue === null) return false;
        const index = registerIndex(block);
        registers[index] = currentValue;
        programCounter += 1;
      } else if (block.type === "add") {
        if (currentValue === null) return false;
        const index = registerIndex(block);
        if (registers[index] === null) return false;
        currentValue += registers[index];
        programCounter += 1;
      } else if (block.type === "sub") {
        if (currentValue === null) return false;
        const index = registerIndex(block);
        if (registers[index] === null) return false;
        currentValue -= registers[index];
        programCounter += 1;
      } else if (block.type === "inc") {
        const index = registerIndex(block);
        if (registers[index] === null) return false;
        currentValue = ++registers[index];
        programCounter += 1;
      } else if (block.type === "dec") {
        const index = registerIndex(block);
        if (registers[index] === null) return false;
        currentValue = --registers[index];
        programCounter += 1;
      } else if (block.type === "jump") {
        programCounter = target(block);
      } else if (block.type === "jump-if-zero") {
        if (currentValue === null) return false;
        programCounter = currentValue === 0 ? target(block) : programCounter + 1;
      } else if (block.type === "jump-if-negative") {
        if (currentValue === null) return false;
        programCounter = currentValue < 0 ? target(block) : programCounter + 1;
      } else {
        throw new Error("unreachable");
      }
      stepCount += 1;
    }
  } catch (e) {
    return false;
  }
}