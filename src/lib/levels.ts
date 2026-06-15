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
    objective: "Place chaque élémént de l'INBOX dans l'OUTBOX. Essaie de le faire en utilisant juste 3 instructions."
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
    id: "zero-filter",
    title: "Filtrage des zéros",
    registers: [],
    input: [4, 0, 8, -3, 0, 0, 5, 0],
    expectedOutput: [4, 8, -3, 5],
    palette: ["input", "output", "jump", "jump-if-zero" ],
    objective: "Pour chaque élément de l'INBOX, place le dans l'OUTBOX si il est différent de ZERO."
  },
  {
    id: "nonzero-filter",
    title: "Filtrage des non-zéros",
    registers: [],
    input: [4, 0, 8, -3, 0, 0, 5, 0],
    expectedOutput: [0, 0, 0, 0],
    palette: ["input", "output", "jump", "jump-if-zero" ],
    objective: "Pour chaque ZERO de l'INBOX, place le dans l'OUTBOX."
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
    title: "Triples",
    registers: [null, null, null],
    input: [3, -7, 5, 0],
    expectedOutput: [9, -21, 15, 0],
    palette: ["input", "output", "copy-from", "copy-to", "add", "jump" ],
    objective: "Pour chaque élément de l'INBOX, triple le et place le résultat dans l'OUTBOX."
  },
  {
    id: "octupler",
    title: "Octuples",
    registers: [null, null, null],
    input: [4, -6, 0, 8],
    expectedOutput: [32, -48, 0, 64],
    palette: ["input", "output", "copy-from", "copy-to", "add", "jump" ],
    objective: "Pour chaque élément de l'INBOX, multiplie le par 8 et place le résultat dans l'OUTBOX."
  },
  {
    id: "mult40",
    title: "Multiplication par 40",
    registers: [null, null, null],
    input: [4, -6, 0, 8],
    expectedOutput: [32, -48, 0, 64],
    palette: ["input", "output", "copy-from", "copy-to", "add", "jump" ],
    objective: "Pour chaque élément de l'INBOX, multiplie le par 40 et place le résultat dans l'OUTBOX."
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
  },
  {
    id: "absolute",
    title: "Valeur absolue",
    registers: [null, null, null, null],
    input: [5, -4, 0, -91, 63],
    expectedOutput: [5, 4, 0, 91, 63],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Pour chaque élément de l'INBOX, envoie dans l'OUTBOX sa valeur absolue. La valeur absolue d'un nombre est ce nombre auquel on retire son signe."
  }, 
  {
    id: "min-max",
    title: "Minimum et maximum",
    registers: [null, null, null, null],
    input: [5, 3, 0, 8, -3, 7, 8, -5],
    expectedOutput: [3, 5, 0, 8, -3, 7, -5, 8],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Pour chaque paire d'élément de l'INBOX, envoie dans l'OUTBOX le plus petit d'entre eux puis le plus grand."
  },
  {
    id: "countdown",
    title: "Compte à rebours",
    registers: [null, null, null, null],
    input: [5, -4, 0, 3, -1],
    expectedOutput: [5, 4, 3, 2, 1, 0, -4, -3, -2, -1, 0, 0, 3, 2, 1, 0, -1, 0],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "inc", "dec", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Pour chaque élément de l’INBOX, produis la suite qui le ramène progressivement à ZERO et place tous les nombres obtenus dans l’OUTBOX. Si le nombre est positif, compte à rebours jusqu’à ZERO. S’il est négatif, compte dans l’autre sens jusqu’à ZERO."
  },
  {
    id: "multiplication",
    title: "Multiplication",
    registers: [null, null, null, null],
    input: [2, 4, 7, 3, 5, 17],
    expectedOutput: [8, 21, 85],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "inc", "dec", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Pour chaque paire d'élément de l’INBOX, produis la multiplication des deux éléments et place la dans l'OUTBOX. Les éléments sont toujours positifs ou nuls."
  },
  {
    id: "parity",
    title: "Parité",
    registers: [null, null, null, null],
    input: [2, 3, -12, 7, 8, 13, 0, 10],
    expectedOutput: [2, 8, 0, 16],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "inc", "dec", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Pour chaque élément de l’INBOX, place le dans l'OUTBOX si il positif et pair (0 compris)."
  },
  {
    id: "euclidean",
    title: "Division euclidienne",
    registers: [null, null, null, null],
    input: [17, 3, 8, 2, 19, 4],
    expectedOutput: [5, 2, 4, 0, 4, 3],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "inc", "dec", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Pour chaque paire d'élément de l’INBOX, produis la division entière du premier par le second et place la dans l'OUTBOX. Place ensuite le reste de la division dans l'OUTBOX. Les éléments sont toujours positifs."
  }
]