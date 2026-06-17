export type IOType =
  | "input"
  | "output"
export type JumpType =
  | "jump"
  | "jump-if-zero"
  | "jump-if-negative";
export type RegisterType =
  | "copy-from"
  | "copy-to"
  | "add"
  | "sub"
  | "inc"
  | "dec";

export type InstructionType = IOType | JumpType | RegisterType;

export type ProgramBlock =
  | InstructionBlock
  | JumpTargetBlock;

export type IOBlock = {
  id: string;
  kind: "instruction";
  type: IOType;
}

export type RegisterBlock = {
  id: string;
  kind: "instruction";
  type: RegisterType;
  register: number;
  indirect?: boolean;
}

export type JumpBlock = {
  id: string;
  kind: "instruction";
  type: JumpType;
  targetId: string;
};


export type InstructionBlock = IOBlock | RegisterBlock | JumpBlock

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

export function isRegisterType(type: InstructionType): type is RegisterType {
  return ["copy-from", "copy-to", "add", "sub", "inc", "dec"].includes(type);
}

export function isJumpType(type: InstructionType): type is JumpType {
  return ["jump", "jump-if-zero", "jump-if-negative"].includes(type)
}

export function isRegisterBlock(block: InstructionBlock): block is RegisterBlock;
export function isRegisterBlock(block: PaletteBlock): boolean;
export function isRegisterBlock(block: PaletteBlock | InstructionBlock): boolean {
  return isRegisterType(block.type)
}

export function isJumpBlock(block: InstructionBlock): block is JumpBlock;
export function isJumpBlock(block: PaletteBlock): boolean;
export function isJumpBlock(block: PaletteBlock | InstructionBlock): boolean  {
  return isJumpType(block.type);
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