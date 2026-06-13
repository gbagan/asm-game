<script lang="ts">
</script>

<div class="bg-canvas" aria-hidden="true">
  <div class="bg-punchcard"></div>

  <svg
    class="bg-machines"
    viewBox="0 0 1200 800"
      preserveAspectRatio="xMidYMid slice"
      xmlns="http://www.w3.org/2000/svg"
  >
    <defs>
      <g id="gear-teeth">
        <rect x="-6" y="-46" width="12" height="14" rx="2" />
        <rect x="-6" y="32" width="12" height="14" rx="2" />
        <rect x="-46" y="-6" width="14" height="12" rx="2" />
        <rect x="32" y="-6" width="14" height="12" rx="2" />
        <g transform="rotate(45)">
          <rect x="-6" y="-46" width="12" height="14" rx="2" />
          <rect x="-6" y="32" width="12" height="14" rx="2" />
          <rect x="-46" y="-6" width="14" height="12" rx="2" />
          <rect x="32" y="-6" width="14" height="12" rx="2" />
        </g>
      </g>

      <g id="gear">
        <circle r="34" class="theme-shape" />
        <use href="#gear-teeth" class="theme-shape" />
        <circle r="12" class="gear-hole" />
      </g>

      <g id="tube">
        <rect x="-22" y="-50" width="44" height="80" rx="22" class="tube-glass" />
        <rect x="-14" y="20" width="28" height="14" class="tube-base" />
        <rect x="-2" y="34" width="4" height="14" class="tube-pin" />
        <circle cx="0" cy="-12" r="9" class="tube-filament" />
      </g>

      <g id="floppy">
        <path d="M-40,-40 H30 L40,-30 V40 H-40 Z" class="floppy-body" />
        <rect x="-26" y="-40" width="36" height="18" class="floppy-shutter" />
        <rect x="-26" y="6" width="52" height="26" class="floppy-label" />
      </g>
	</defs>

    <g transform="translate(160,140)">
      <g class="machine spin-cw" fill="none" stroke-width="2">
        <use href="#gear" />
      </g>
    </g>
    <g transform="translate(255,215) scale(0.6)">
      <g class="machine spin-ccw" fill="none" stroke-width="2">
        <use href="#gear" />
      </g>
    </g>
    <g transform="translate(1020,540) scale(0.85)">
      <g class="machine spin-cw-slow" fill="none" stroke-width="2">
        <use href="#gear" />
      </g>
	</g>

    <!-- Lampes à vide -->
    <g transform="translate(940,130)">
	  <g class="machine float-a" stroke-width="2">
	    <use href="#tube" />
	  </g>
    </g>
    <g transform="translate(380,600) scale(0.75)">
	  <g class="machine float-b" stroke-width="2">
        <use href="#tube" />
      </g>
	</g>

    <!-- Disquettes -->
    <g transform="translate(720,600) rotate(-4)">
      <g class="machine float-rot-a" stroke-width="2">
        <use href="#floppy" />
	  </g>
	</g>
    <g transform="translate(180,360) scale(0.7) rotate(5)">
      <g class="machine float-rot-b" stroke-width="2">
		<use href="#floppy" />
      </g>
    </g>

    <!-- Chiffres binaires flottants -->
    <g transform="translate(240,120)"><text class="bits float-a">01001</text></g>
    <g transform="translate(980,640)"><text class="bits float-b">1010</text></g>
    <g transform="translate(560,700)"><text class="bits float-a">0110</text></g>
    <g transform="translate(120,520)"><text class="bits float-b">1101</text></g>
    <g transform="translate(850,180)"><text class="bits float-a">0011</text></g>

    <!-- Circuits avec impulsions -->
    <path id="circuitA" class="circuit" d="M80,500 H320 V380 H520 V240 H760" fill="none" />
    <circle r="6" class="pulse pulse-copper">
      <animateMotion dur="9s" repeatCount="indefinite">
        <mpath href="#circuitA" />
      </animateMotion>
    </circle>

    <path id="circuitB" class="circuit" d="M1100,160 H900 V300 H700 V460 H440" fill="none" />
    <circle r="5" class="pulse pulse-teal">
      <animateMotion dur="12s" repeatCount="indefinite" begin="-3s">
	    <mpath href="#circuitB" />
	  </animateMotion>
	</circle>
  </svg>
</div>

<style>
  .bg-canvas {
    position: fixed;
    inset: 0;
    z-index: -1;
    overflow: hidden;
    background: linear-gradient(135deg, #faf6ec 0%, #f4eddd 55%, #f1e8d4 100%);
  }

  /* Trame "carte perforée" */
  .bg-punchcard {
	position: absolute;
	inset: 0;
	background-image:
      repeating-linear-gradient(0deg, rgba(120, 95, 60, 0.08) 0 3px, transparent 3px 22px),
	  repeating-linear-gradient(90deg, rgba(120, 95, 60, 0.08) 0 6px, transparent 6px 28px);
  }

  .bg-machines {
    position: absolute;
    inset: 0;
    width: 100%;
    height: 100%;
  }

  /* Teintes communes */
  .theme-shape {
    fill: #e4d7be;
    stroke: #b7a687;
  }
  .gear-hole {
    fill: #faf6ec;
    stroke: #b7a687;
  }
  .tube-glass {
    fill: #f3ead6;
    stroke: #c9bba0;
  }
  .tube-base {
    fill: #c9bba0;
    stroke: none;
  }
  .tube-pin {
    fill: #9c8e73;
    stroke: none;
  }
  .tube-filament {
    fill: #e8a33d;
    stroke: none;
    animation: glow 3.4s ease-in-out infinite;
  }
  .floppy-body {
    fill: #d8c9ae;
    stroke: #b7a687;
  }
  .floppy-shutter {
    fill: #9fb8c9;
    stroke: #8aa3b5;
  }
  .floppy-label {
    fill: #f3ead6;
    stroke: #c9bba0;
  }
  .bits {
    font-family: 'Courier New', monospace;
    font-size: 28px;
    fill: rgba(120, 95, 60, 0.22);
  }
  .circuit {
    stroke: #c9a877;
    stroke-width: 2;
    stroke-dasharray: 4 9;
    opacity: 0.4;
  }
  .pulse-copper {
    fill: #c9824b;
    opacity: 0.7;
  }
  .pulse-teal {
    fill: #6fae9c;
    opacity: 0.6;
  }

  /* Opacité d'ensemble des éléments décoratifs */
  .machine {
    opacity: 0.45;
  }

  /* Toutes les animations ci-dessous ne touchent que `transform`, en
    restant centrées sur l'origine locale (0,0) du groupe positionné
    par le <g transform="translate(...)"> parent.
  */
  .spin-cw {
    transform-origin: 0px 0px;
    animation: spin 36s linear infinite;
  }
  .spin-ccw {
    transform-origin: 0px 0px;
    animation: spin-rev 24s linear infinite;
  }
  .spin-cw-slow {
    transform-origin: 0px 0px;
    animation: spin 30s linear infinite;
  }

  .float-a {
    animation: float 19s ease-in-out infinite alternate;
    --dy: -18px;
  }
  .float-b {
    animation: float 16s ease-in-out infinite alternate-reverse;
    animation-delay: -6s;
    --dy: -14px;
  }
  .float-rot-a {
    animation: float-rot 22s ease-in-out infinite alternate;
    --dy: -18px;
    --rot: -6deg;
  }
  .float-rot-b {
    animation: float-rot 19s ease-in-out infinite alternate;
    animation-delay: -8s;
    --dy: -14px;
    --rot: 8deg;
  }

	@keyframes spin {
		from {
			transform: rotate(0deg);
		}
		to {
			transform: rotate(360deg);
		}
	}
	@keyframes spin-rev {
		from {
			transform: rotate(0deg);
		}
		to {
			transform: rotate(-360deg);
		}
	}
	@keyframes float {
		0% {
			transform: translate(0, 0);
		}
		100% {
			transform: translate(0, var(--dy, -20px));
		}
	}
	@keyframes float-rot {
		0% {
			transform: translate(0, 0) rotate(0deg);
		}
		100% {
			transform: translate(0, var(--dy, -20px)) rotate(var(--rot, 6deg));
		}
	}
	@keyframes glow {
		0%,
		100% {
			opacity: 0.55;
			r: 7px;
		}
		50% {
			opacity: 1;
			r: 10px;
		}
	}

	/* Respect des préférences de réduction de mouvement */
	@media (prefers-reduced-motion: reduce) {
		.machine,
		.bits,
		.tube-filament {
			animation: none;
		}
		.pulse {
			animation: none;
		}
		circle animateMotion {
			display: none;
		}
	}
</style>