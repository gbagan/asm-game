<script lang="ts">
  import { tick } from "svelte";

  type Props = {
    container: HTMLDivElement;
    blockId: string | null;
    layoutVersion: number;
  };

  let { container, blockId, layoutVersion }: Props = $props();

  let y = $state(0);
  let visible = $state(false);

  async function updatePosition() {
    await tick();

    requestAnimationFrame(() => {
      if (!blockId) {
        visible = false;
        return;
      }

      const blockEl = container.querySelector<HTMLElement>(
        `[data-block-id="${blockId}"]`
      );

      if (!blockEl) {
        visible = false;
        return;
      }

      const containerRect = container.getBoundingClientRect();
      const blockRect = blockEl.getBoundingClientRect();

      y =
        blockRect.top -
        containerRect.top +
        container.scrollTop +
        blockRect.height / 2;

      visible = true;
    });
  }

  $effect(() => {
    blockId;
    layoutVersion;
    updatePosition();
  });
</script>

<svelte:window onresize={updatePosition} />

{#if visible}
  <div
    class="execution-pointer"
    style:top={`${y}px`}
  >
    <span class="pointer-line"></span>
    <span class="pointer-head"></span>
  </div>
{/if}

<style>
  .execution-pointer {
    position: absolute;
    left: 0.5rem;
    z-index: 5;
    transform: translateY(-50%);
    width: 2rem;
    height: 1rem;
    pointer-events: none;
    transition: top 140ms ease;
  }

  .pointer-line {
    position: absolute;
    left: 0;
    top: 50%;
    width: 1.4rem;
    height: 3px;
    background: #dc2626;
    transform: translateY(-50%);
    border-radius: 999px;
  }

  .pointer-head {
    position: absolute;
    right: 0;
    top: 50%;
    width: 0;
    height: 0;
    border-top: 0.4rem solid transparent;
    border-bottom: 0.4rem solid transparent;
    border-left: 0.6rem solid #dc2626;
    transform: translateY(-50%);
  }
</style>