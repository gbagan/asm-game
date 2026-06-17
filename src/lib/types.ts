export type InstructionType =
  | "input"
  | "output"
  | "jump"
  | "jump-if-zero"
  | "jump-if-negative"
  | "copy-from"
  | "copy-to"
  | "add"
  | "sub"
  | "inc"
  | "dec";

export type ProgramBlock =
  | InstructionBlock
  | JumpTargetBlock;

export type InstructionBlock = {
  id: string;
  kind: "instruction";
  type: InstructionType;
  targetId?: string;
  register?: number;
  indirect?: boolean;
};

export type JumpTargetBlock = {
  id: string;
  kind: "jump-target";
  ownerJumpId: string;
};

export type PaletteBlock = {
  id: string;
  type: InstructionType;
  fromPalette: true;
};

export type DraggedBlock = ProgramBlock | PaletteBlock;

export function isPaletteBlock(block: DraggedBlock): block is PaletteBlock {
  return "fromPalette" in block;
}

export function isRegisterBlock(block: InstructionBlock | PaletteBlock) {
  return ["copy-from", "copy-to", "add", "sub", "inc", "dec"].includes(block.type)
}

export function isJumpBlock(block: InstructionBlock | PaletteBlock) {
  return ["jump", "jump-if-zero", "jump-if-negative"].includes(block.type)
}

export type Level = {
  id: string;
  title: string;  
  input: number[];
  registers: (number | null)[];
  expectedOutput: number[];
  palette: InstructionType[];
  objective: string;
  allowIndirect?: boolean;
  tests?: [number[], number[], number][];
}

export type LevelInfo = {
  completed: boolean;
  program: ProgramBlock[];
  instructionCount: number;
  stepCount: number;
}