export type InstructionType =
  | "input"
  | "output"
  | "jump"
  | "jump-if-zero"
  | "copy-from"
  | "copy-to";

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