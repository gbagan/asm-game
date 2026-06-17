<script lang="ts">
  import { tick } from "svelte";
  import { isJumpBlock, type ProgramBlock } from "../lib/types";

  const COLORS = [
    "var(--blue-500)",
    "var(--red-500)",
    "var(--green-500)",
    "var(--orange-500)",
    "var(--amber-500)" ,
    "var(--teal-500)",
    "var(--violet-500)",
    "var(--pink-500)",
  ];

  type Props = {
    program: ProgramBlock[];
    container?: HTMLDivElement;
    hidden: boolean;
    layoutVersion: number;
  };

  let { program, container, hidden, layoutVersion }: Props = $props();

  type Arrow = {
    fromX: number;
    fromY: number;
    toX: number;
    toY: number;
  };

  let arrows = $state.raw<Arrow[]>([]);
  let width = $state.raw(0);
  let height = $state.raw(0);

  async function updateArrows() {
    await tick();

    if (!container) return;

    const containerRect = container.getBoundingClientRect();

    width = container.clientWidth;
    height = container.scrollHeight;

    const nextArrows: Arrow[] = [];

    for (const block of program) {
      if (block.kind !== "instruction" || !isJumpBlock(block) || !block.targetId) continue;

      const fromEl = container.querySelector<HTMLElement>(
        `[data-block-id="${block.id}"]`
      );

      const toEl = container.querySelector<HTMLElement>(
        `[data-block-id="${block.targetId}"]`
      );

      if (!fromEl || !toEl) continue;

      const fromRect = fromEl.getBoundingClientRect();
      const toRect = toEl.getBoundingClientRect();

      const fromX =
        fromRect.right -
        containerRect.left +
        container.scrollLeft;

      const fromY =
        fromRect.top -
        containerRect.top +
        container.scrollTop +
        fromRect.height / 2;

      const toX =
        toRect.right -
        containerRect.left +
        container.scrollLeft;

      const toY =
        toRect.top -
        containerRect.top +
        container.scrollTop +
        toRect.height / 2;

      nextArrows.push({
        fromX,
        fromY,
        toX,
        toY
      });
    }

    arrows = nextArrows;
  }

  $effect(() => {
    program;
    layoutVersion;
    updateArrows();
  });

  function pathFor(arrow: Arrow) {
    const x1 = arrow.fromX;
    const y1 = arrow.fromY;
    const x2 = arrow.toX;
    const y2 = arrow.toY;

    const distanceY = Math.abs(y2 - y1);

    const out = Math.max(10, Math.min(70, distanceY / 4));

    const sideX = Math.max(x1, x2) + out;

    const sweep = y2 > y1 ? 1 : 0;

    return `
      M ${x1} ${y1}
      L ${sideX} ${y1}
      A ${out} ${distanceY / 2} 0 0 ${sweep} ${sideX} ${y2}
      L ${x2} ${y2}
    `;
  }
</script>

<svelte:window onresize={updateArrows} />

<svg
  class="arrows"
  class:hidden
  {width}
  {height}
  viewBox={`0 0 ${width} ${height}`}
>
  <defs>
    {#each COLORS as color, i}
      <marker
        id="arrow-head-{i}"
        markerWidth="10"
        markerHeight="10"
        refX="8"
        refY="3"
        orient="auto"
        markerUnits="strokeWidth"
      >
        <path d="M0,0 L0,6 L9,3 z" fill={color}/>
      </marker>
    {/each}
  </defs>

  {#each arrows as arrow, i}
    <path
      d={pathFor(arrow)}
      fill="none"
      stroke={COLORS[i % COLORS.length]}
      stroke-width="4"
      marker-end="url(#arrow-head-{i % COLORS.length})"
    />
  {/each}
</svg>

<style>
  .arrows {
    position: absolute;
    top: 0;
    left: 0;
    z-index: 1;
    pointer-events: none;
    overflow: visible;
    transition: opacity 120ms ease;
  }

  .hidden {
    opacity: 0;
  }
</style>