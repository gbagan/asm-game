import type { Level } from "./types"

export const LEVELS: Level[] = [ 
  {  
    id: "warmingup",
    title: "Echauffement",  
    input: [10, 5, 8],
    registers: [],
    expectedOutput: [10, 5, 8],
    palette: ["input", "output" ],
    objective: "Place chaque élémént de l'INBOX dans l'OUTBOX."
  },
  {  
    id: "loop",
    title: "Boucle",  
    input: [10, 5, 8, -3, 6, 2, 9, 1],
    registers: [],
    expectedOutput: [10, 5, 8, -3, 6, 2, 9, 1],
    palette: ["input", "output", "jump" ],
    objective: "Place chaque élémént de l'INBOX dans l'OUTBOX. Essaie de le faire en utilisant 3 instructions."
  },
  { 
    id: "permutations",
    title: "Permutations",
    input: [9, 6, 7, 12, 0, 15],
    registers: [null, null, null],
    expectedOutput: [6, 9, 12, 7, 15, 0],
    palette: ["input", "output", "copy-from", "copy-to", "jump"],
    objective: "Prends les deux premiers valeurs de l'INBOX et place les dans l'OUTBOX dans le sens inverse. Repétez jusqu'à ce que l'INBOX soit vide."
  },
  {
    id: "additions",
    title: "Additions",
    registers: [null, null, null],
    input: [10, 5, 8, 14, 5, 3],
    expectedOutput: [15, 22, 8],
    palette: ["input", "output", "copy-from", "copy-to", "add", "jump" ],
    objective: "Pour chaque paire d'éléments de l'INBOX, ajoute les et place le résultat dans l'OUTBOX."
  },
  {
    id: "tripler",
    title: "Tripler",
    registers: [null, null, null],
    input: [3, -7, 5, 0],
    expectedOutput: [9, -21, 15, 0],
    palette: ["input", "output", "copy-from", "copy-to", "add", "jump" ],
    objective: "Pour chaque élément de l'INBOX, triple le et place le résultat dans l'OUTBOX."
  },
  {
    id: "octupler",
    title: "Octupler",
    registers: [null, null, null],
    input: [4, -6, 0, 8],
    expectedOutput: [32, -48, 0, 64],
    palette: ["input", "output", "copy-from", "copy-to", "add", "jump" ],
    objective: "Pour chaque élément de l'INBOX, multiplie le par 8 et place le résultat dans l'OUTBOX."
  },
  {
    id: "zero-filter",
    title: "Filtrage des zéros",
    registers: [null, null, null, null, null, null, null, null],
    input: [4, 0, 8, -3, 0, 0, 5, 0],
    expectedOutput: [4, 8, -3, 5],
    palette: ["input", "output", "copy-from", "copy-to", "add", "jump", "jump-if-zero" ],
    objective: "Pour chaque élément de l'INBOX, place le dans l'OUTBOX si il est différent de ZERO."
  },
  {
    id: "nonzero-filter",
    title: "Filtrage des non-zéros",
    registers: [null, null, null, null, null, null, null, null],
    input: [4, 0, 8, -3, 0, 0, 5, 0],
    expectedOutput: [0, 0, 0, 0],
    palette: ["input", "output", "copy-from", "copy-to", "add", "jump", "jump-if-zero" ],
    objective: "Pour chaque ZERO de l'INBOX, place le dans l'OUTBOX."
  },
  {
    id: "subtract",
    title: "Soustraction",
    registers: [null, null, null, null, null, null, null, null],
    input: [5, 4, 8, -3, 2, 7, 6, 2],
    expectedOutput: [1, -1, 11, -11, -5, 5, 4, -4],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "jump", "jump-if-zero" ],
    objective: "Pour chaque paire d'éléments de l'INBOX, envoie dans l'OUTBOX le premier auquel est soustrait le second puis le second auquel est soustrait le premier",
  },
  {
    id: "equality",
    title: "Egalité",
    registers: [null, null, null, null],
    input: [5, 4, 6, 6, 2, 7, -9, 9, 4, 4],
    expectedOutput: [6, 4],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "jump", "jump-if-zero" ],
    objective: "Pour chaque paire d'éléments de l'INBOX, envoie dans l'OUTBOX un de ces éléments si ils sont égaux et rejette les deux sinon."
  }
]