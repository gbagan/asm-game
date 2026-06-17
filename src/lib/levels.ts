import { arrayOf, range, times } from "@gbagan/utils"
import type { Level } from "./types"

export const LEVELS: Level[] = [ 
  {  
    id: "warmingup",
    title: "Echauffement",  
    input: [10, 5, 8],
    registers: [],
    expectedOutput: [10, 5, 8],
    palette: ["input", "output" ],
    objective: "Place chaque valeur de l'INPUT dans l'OUTPUT."
  },
  {  
    id: "loop",
    title: "Boucle",  
    input: [10, 5, 8, -3, 6, 2, 9, 1],
    registers: [],
    expectedOutput: [10, 5, 8, -3, 6, 2, 9, 1],
    palette: ["input", "output", "jump" ],
    objective: "Place chaque valeur de l'INPUT dans l'OUTPUT. Essaie de le faire en utilisant juste 3 instructions.",
    tests: [
      [range(0, 40), range(0, 40), 200]
    ]
  },
  { 
    id: "permutations",
    title: "Permutations",
    input: [9, 6, 7, 12, 0, 15, -9, 3],
    registers: [null, null, null],
    expectedOutput: [6, 9, 12, 7, 15, 0, 3, -9],
    palette: ["input", "output", "copy-from", "copy-to", "jump"],
    objective: "Prends les deux premières valeurs de l'INPUT et place les dans l'OUTPUT dans le sens inverse. Répète jusqu'à ce que l'INPUT soit vide.",
    tests: [
      [range(0, 30), times(30, i => i ^ 1), 150] 
    ]
  },
  {
    id: "zero-filter",
    title: "Filtrage des zéros",
    registers: [],
    input: [4, 0, 8, -3, 0, 0, 5, 0],
    expectedOutput: [4, 8, -3, 5],
    palette: ["input", "output", "jump", "jump-if-zero" ],
    objective: "Pour chaque valeur de l'INPUT, place la dans l'OUTPUT si elle est différente de ZERO. Sinon rejette la.",
    tests: [
      [[0, 3, 0, 4, 6, 0], [3, 4, 6], 50],
    ]
  },
  {
    id: "nonzero-filter",
    title: "Filtrage des positifs",
    registers: [],
    input: [4, 6, -8, -3, 0, 4, -5, 8],
    expectedOutput: [-8, -3, 0, -5],
    palette: ["input", "output", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Pour chaque valeur de l'INPUT, place la dans l'OUTPUT s'elle est négative ou nulle. Sinon rejette la.",
      tests: [
      [[-5, 3, 0, 4, -8, -9], [-5, 0, -8, -9], 50],
    ]
  },
  {
    id: "additions",
    title: "Additions",
    registers: [null, null, null],
    input: [10, 5, -8, 14, 5, 3, 6, -18],
    expectedOutput: [15, 6, 8, -12],
    palette: ["input", "output", "copy-from", "copy-to", "add", "jump" ],
    objective: "Pour chaque paire de valeurs de l'INPUT, ajoute les et place le résultat dans l'OUTPUT."
  },
  {
    id: "tripler",
    title: "Triples",
    registers: [null, null, null],
    input: [3, -7, 5, 0],
    expectedOutput: [9, -21, 15, 0],
    palette: ["input", "output", "copy-from", "copy-to", "add", "jump" ],
    objective: "Pour chaque valeur de l'INPUT, triple la et place la résultat dans l'OUTPUT."
  },
  {
    id: "octupler",
    title: "Octuples",
    registers: [null, null, null],
    input: [4, -6, 0, 8],
    expectedOutput: [32, -48, 0, 64],
    palette: ["input", "output", "copy-from", "copy-to", "add", "jump" ],
    objective: "Pour chaque valeur de l'INPUT, multiplie la par 8 et place le résultat dans l'OUTPUT."
  },
  {
    id: "mult40",
    title: "Multiplication par 40",
    registers: [null, null, null],
    input: [4, -6, 0, 8],
    expectedOutput: [32, -48, 0, 64],
    palette: ["input", "output", "copy-from", "copy-to", "add", "jump" ],
    objective: "Pour chaque valeur de l'INPUT, multiplie la par 40 et place le résultat dans l'OUTPUT."
  },
  {
    id: "subtract",
    title: "Soustraction",
    registers: [null, null, null, null],
    input: [5, 4, 8, -3, 2, 7, 6, 2],
    expectedOutput: [1, -1, 11, -11, -5, 5, 4, -4],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "jump", "jump-if-zero" ],
    objective: "Pour chaque paire de valeurs de l'INPUT, envoie dans l'OUTPUT la première auquel est soustrait la seconde puis le second auquel est soustrait le premier",
  },
  {
    id: "equality",
    title: "Egalité",
    registers: [null, null, null, null],
    input: [5, 4, 6, 6, 2, 7, -9, 9, 4, 4],
    expectedOutput: [6, 4],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "jump", "jump-if-zero" ],
    objective: "Pour chaque paire de valeurs de l'INPUT, envoie dans l'OUTPUT un de ces éléments si ils sont égaux et rejette les deux sinon."
  },
  {
    id: "absolute",
    title: "Valeur absolue",
    registers: [null, null, null, null],
    input: [5, -4, 0, -91, 63],
    expectedOutput: [5, 4, 0, 91, 63],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Pour chaque élément de l'INPUT, envoie dans l'OUTPUT sa valeur absolue. La valeur absolue d'un nombre est ce nombre auquel on retire son signe."
  }, 
  {
    id: "min-max",
    title: "Minimum et maximum",
    registers: [null, null, null, null],
    input: [5, 3, 0, 8, -3, 7, 8, -5],
    expectedOutput: [3, 5, 0, 8, -3, 7, -5, 8],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Pour chaque paire d'élément de l'INPUT, envoie dans l'OUTPUT le plus petit d'entre eux puis le plus grand."
  },
  {
    id: "countdown",
    title: "Compte à rebours",
    registers: [null, null, null, null, null, null, null, null, null, null],
    input: [5, -4, 0, 3, -1],
    expectedOutput: [5, 4, 3, 2, 1, 0, -4, -3, -2, -1, 0, 0, 3, 2, 1, 0, -1, 0],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "inc", "dec", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Pour chaque élément de l’INPUT, produis la suite qui le ramène progressivement à ZERO et place tous les nombres obtenus dans l’OUTPUT. Si le nombre est positif, compte à rebours jusqu’à ZERO. S’il est négatif, compte dans l’autre sens jusqu’à ZERO."
  },
  {
    id: "min-max2",
    title: "Minimum et maximum II",
    registers: [null, null, null, null, null, null, null, null, null, null],
    input: [6, -4, 0, 6, -7, 8, 1, 10, -13, 6, 18],
    expectedOutput: [-7, 8],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "inc", "dec", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Le premier élément de l'INPUT indique le nombre n. Lis ensuite les n éléments suivants, trouve parmi eux le minimum et le maximum et place les dans l'OUTPUT. Ignore les éléments restants de l'INBOX."
  },
  {
    id: "parity",
    title: "Parité",
    registers: [null, null, null, null, null, null, null, null, null, 0],
    input: [2, 3, -12, 7, 8, 13, 0, 10],
    expectedOutput: [2, 8, 0, 10],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "inc", "dec", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Pour chaque valeur de l’INPUT, place la dans l'OUTPUT si il positif et pair (0 compris). Sinone rejette la.",
    tests: [
      [[-3, 0, 2, 5, 7, 8], [0, 2, 8], 250],
    ]
  },
  {
    id: "fibonacci",
    title: "Fibonacci",
    registers: [null, null, null, null, null, null, null, null, null, 0 ],
    input: [3, 5, 10, 1, 12, 0],
    expectedOutput: [2, 5, 55, 1, 144, 0],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "inc", "dec", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "La suite de Fibonacci commence par 0 et 1. Chaque terme suivant est obtenu en additionnant les deux termes précédents. Pour chaque valeur n de l’INPUT, écris dans l’OUTPUT le n-ième terme de la suite de Fibonacci, les termes étant numérotés à partir de 0.",
    tests: [
      [[4, 0, 1, 6, 15], [3, 0, 1, 8, 610], 400]
    ]
  },
  {
    id: "multiplication",
    title: "Multiplication",
    registers: [null, null, null, null, null, null, null, null, null, 0 ],
    input: [2, 4, 7, 3, 5, 17],
    expectedOutput: [8, 21, 85],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "inc", "dec", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Pour chaque paire d'élément de l’INPUT, produis la multiplication des deux éléments et place la dans l'OUTPUT. Les éléments sont toujours positifs ou nuls."
  },
  {
    id: "euclidean",
    title: "Division euclidienne",
    registers: [null, null, null, null, null, null, null, null, null, 0],
    input: [17, 3, 8, 2, 19, 4],
    expectedOutput: [5, 2, 4, 0, 4, 3],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "inc", "dec", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Pour chaque paire d'élément de l’INPUT, produis la division entière du premier par le second et place la dans l'OUTPUT. Place ensuite le reste de la division dans l'OUTPUT. Les éléments sont toujours positifs."
  },
  {
    id: "primality",
    title: "Primalité",
    registers: [null, null, null, null, null, null, null, null, null, 0],
    input: [5, 4, 6, 7, -2, 13, 1, 9, 17],
    expectedOutput: [5, 7, 13, 17],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "inc", "dec", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Pour chaque valeur de l’INPUT, place la dans l'OUTPUT seulement si c'est un nombre premier. Sinon rejette la. Un nombre est premier si il est supérieur ou égal à 2 et ses seuls diviseurs sont 1 et lui même."
  },
  {
    id: "prime-factors",
    title: "Facteurs premiers",
    registers: [null, null, null, null, null, null, null, null, null, 0],
    input: [9, 12, 7, 35],
    expectedOutput: [3, 3, 2, 2, 3, 7, 5, 7],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "inc", "dec", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Pour chaque nombre de l’INPUT, décompose-le en facteurs premiers, puis place ces facteurs dans l’OUTPUT dans l’ordre croissant."
  },
  {
    id: "sequence-inversion",
    title: "Inversion de séquence",
    registers: arrayOf<number | null>(15, null).with(14, 0),
    input: [8, 3, 17, -35, 4, 8, 72, -5, 0, 10, 23],
    expectedOutput: [-5, 72, 8, 4, -35, 17, 3, 8],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "inc", "dec", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Récupère les éléments de l’INPUT jusqu’au premier 0, sans inclure ce 0. Ignore le reste, puis place les éléments récupérés dans l’OUTPUT en ordre inverse. Il y a au plus 10 éléments avant le 0",
    allowIndirect: true,
    tests: [
      [range(10, -1, -1), range(1, 11), 100]
    ]
  },
  {
    id: "sequence-sorting",
    title: "Tri de séquence",
    registers: arrayOf<number | null>(15, null).with(14, 0),
    input: [8, 3, 17, -35, 4, 8, 72, -5, 0, 10, 23],
    expectedOutput: [-35, -5, 3, 4, 8, 8, 17, 72],
    palette: ["input", "output", "copy-from", "copy-to", "add", "sub", "inc", "dec", "jump", "jump-if-zero", "jump-if-negative" ],
    objective: "Récupère les éléments de l’INPUT jusqu’au premier 0, sans inclure ce 0. Ignore le reste, puis place les éléments récupérés dans l’OUTPUT en ordre croissant. Il y a au plus 10 éléments avant le 0",
    allowIndirect: true
  }
]