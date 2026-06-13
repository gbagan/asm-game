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
  label: string;
  targetId?: string;
  register?: number;
};

export type JumpTargetBlock = {
  id: string;
  kind: "jump-target";
  label: string;

  // optionnel, mais pratique pour savoir quel jump l’a créé
  ownerJumpId: string;
};

export type PaletteBlock = {
  id: string;
  type: InstructionType;
  label: string;
  fromPalette: true;
};

export type DraggedBlock = ProgramBlock | PaletteBlock;