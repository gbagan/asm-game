export type InstructionType =
  | "input"
  | "output"
  | "jump"
  | "jump-if-zero"
  | "copy-from"
  | "copy-to"
  | "add";

export type ProgramBlock =
  | InstructionBlock
  | JumpTargetBlock;

export type InstructionBlock = {
  id: string;
  kind: "instruction";
  type: InstructionType;
  targetId?: string;
  register?: number;
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

export function isRegisterBlock(block: InstructionBlock) {
  return ["copy-from", "copy-to", "add"].includes(block.type)
}

export type Level = {
  id: string;
  title: string;  
  input: number[];
  registers: (number | null)[];
  expectedOutput: number[];
  palette: InstructionType[];
  objective: string;
}

export type LevelInfo = {
  completed: boolean;
  program: ProgramBlock[];
  instructionCount: number;
  stepCount: number;
}