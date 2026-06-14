<script lang="ts">
</script>

<div class="bg-canvas" aria-hidden="true">
  <div class="bg-punchcard"></div>

  <svg
    class="bg-scene"
    viewBox="0 0 1000 520"
    preserveAspectRatio="xMidYMid slice"
    xmlns="http://www.w3.org/2000/svg"
  >

    <!-- ═══ TUYAUX ══════════════════════════════════════ -->
    <g fill="none" stroke="#C4A87A" stroke-width="9" stroke-linecap="round" opacity=".38">
      <path d="M0,210 H200 Q220,210 220,230 V330 Q220,350 240,350 H480"/>
      <path d="M480,350 H640 Q660,350 660,330 V180 Q660,160 680,160 H1000"/>
      <path d="M0,400 H90 Q110,400 110,380 V310 Q110,290 130,290 H300"/>
      <path d="M700,520 V440 Q700,420 720,420 H1000"/>
      <path d="M300,0 V80 Q300,100 320,100 H480 Q500,100 500,120 V350"/>
    </g>
    <!-- Brides de tuyaux -->
    <g fill="none" stroke="#B09060" stroke-width="3" opacity=".4">
      <rect x="190" y="202" width="20" height="16" rx="2"/>
      <rect x="470" y="342" width="20" height="16" rx="2"/>
      <rect x="652" y="152" width="20" height="16" rx="2"/>
      <rect x="100" y="282" width="20" height="16" rx="2"/>
      <rect x="290" y="92"  width="20" height="16" rx="2"/>
      <rect x="490" y="92"  width="20" height="16" rx="2"/>
    </g>
    <!-- Impulsions -->
    <path class="pipe-pulse"
      fill="none" stroke="#D47E40" stroke-width="4" opacity=".55"
      d="M0,210 H200 Q220,210 220,230 V330 Q220,350 240,350 H480"/>
    <path class="pipe-pulse pipe-pulse--2"
      fill="none" stroke="#6FAE9C" stroke-width="3.5" opacity=".5"
      d="M480,350 H640 Q660,350 660,330 V180 Q660,160 680,160 H1000"/>
    <path class="pipe-pulse pipe-pulse--3"
      fill="none" stroke="#D47E40" stroke-width="3" opacity=".45"
      d="M0,400 H90 Q110,400 110,380 V310 Q110,290 130,290 H300"/>

    <!-- ═══ ENGRENAGES ══════════════════════════════════ -->
    <!-- Engrenage 1 – grand, haut gauche -->
    <g class="gear gear--cw" style="transform-origin:110px 108px; animation-duration:34s;" opacity=".42"
       fill="#E8D8B8" stroke="#C0A070" stroke-width="2">
      <circle cx="110" cy="108" r="52"/>
      <rect x="104" y="48"  width="12" height="18" rx="2" transform="rotate(0,110,108)"/>
      <rect x="104" y="48"  width="12" height="18" rx="2" transform="rotate(45,110,108)"/>
      <rect x="104" y="48"  width="12" height="18" rx="2" transform="rotate(90,110,108)"/>
      <rect x="104" y="48"  width="12" height="18" rx="2" transform="rotate(135,110,108)"/>
      <rect x="104" y="48"  width="12" height="18" rx="2" transform="rotate(180,110,108)"/>
      <rect x="104" y="48"  width="12" height="18" rx="2" transform="rotate(225,110,108)"/>
      <rect x="104" y="48"  width="12" height="18" rx="2" transform="rotate(270,110,108)"/>
      <rect x="104" y="48"  width="12" height="18" rx="2" transform="rotate(315,110,108)"/>
      <circle cx="110" cy="108" r="18" fill="#F5EDD8" stroke="#C0A070"/>
    </g>
    <!-- Engrenage 2 – petit, engrène avec 1 -->
    <g class="gear gear--ccw" style="transform-origin:186px 130px; animation-duration:20s;" opacity=".40"
       fill="#E0D0B0" stroke="#B89860" stroke-width="1.5">
      <circle cx="186" cy="130" r="30"/>
      <rect x="181" y="93"  width="10" height="12" rx="2" transform="rotate(0,186,130)"/>
      <rect x="181" y="93"  width="10" height="12" rx="2" transform="rotate(60,186,130)"/>
      <rect x="181" y="93"  width="10" height="12" rx="2" transform="rotate(120,186,130)"/>
      <rect x="181" y="93"  width="10" height="12" rx="2" transform="rotate(180,186,130)"/>
      <rect x="181" y="93"  width="10" height="12" rx="2" transform="rotate(240,186,130)"/>
      <rect x="181" y="93"  width="10" height="12" rx="2" transform="rotate(300,186,130)"/>
      <circle cx="186" cy="130" r="10" fill="#F5EDD8" stroke="#B89860"/>
    </g>
    <!-- Engrenage 3 – grand, bas droite -->
    <g class="gear gear--cw" style="transform-origin:895px 390px; animation-duration:28s;" opacity=".38"
       fill="#E8D8B8" stroke="#C0A070" stroke-width="2">
      <circle cx="895" cy="390" r="60"/>
      <rect x="889" y="322" width="12" height="20" rx="2" transform="rotate(0,895,390)"/>
      <rect x="889" y="322" width="12" height="20" rx="2" transform="rotate(45,895,390)"/>
      <rect x="889" y="322" width="12" height="20" rx="2" transform="rotate(90,895,390)"/>
      <rect x="889" y="322" width="12" height="20" rx="2" transform="rotate(135,895,390)"/>
      <rect x="889" y="322" width="12" height="20" rx="2" transform="rotate(180,895,390)"/>
      <rect x="889" y="322" width="12" height="20" rx="2" transform="rotate(225,895,390)"/>
      <rect x="889" y="322" width="12" height="20" rx="2" transform="rotate(270,895,390)"/>
      <rect x="889" y="322" width="12" height="20" rx="2" transform="rotate(315,895,390)"/>
      <circle cx="895" cy="390" r="22" fill="#F5EDD8" stroke="#C0A070"/>
    </g>
    <!-- Engrenage 4 – moyen, engrène avec 3 -->
    <g class="gear gear--ccw" style="transform-origin:950px 450px; animation-duration:17s;" opacity=".36"
       fill="#E0D0B0" stroke="#B89860" stroke-width="1.5">
      <circle cx="950" cy="450" r="36"/>
      <rect x="945" y="406" width="10" height="14" rx="2" transform="rotate(0,950,450)"/>
      <rect x="945" y="406" width="10" height="14" rx="2" transform="rotate(51,950,450)"/>
      <rect x="945" y="406" width="10" height="14" rx="2" transform="rotate(102,950,450)"/>
      <rect x="945" y="406" width="10" height="14" rx="2" transform="rotate(153,950,450)"/>
      <rect x="945" y="406" width="10" height="14" rx="2" transform="rotate(204,950,450)"/>
      <rect x="945" y="406" width="10" height="14" rx="2" transform="rotate(255,950,450)"/>
      <rect x="945" y="406" width="10" height="14" rx="2" transform="rotate(306,950,450)"/>
      <circle cx="950" cy="450" r="12" fill="#F5EDD8" stroke="#B89860"/>
    </g>
    <!-- Engrenage 5 – bas gauche -->
    <g class="gear gear--ccw" style="transform-origin:110px 390px; animation-duration:40s;" opacity=".32"
       fill="#E8D8B8" stroke="#C0A070" stroke-width="2">
      <circle cx="110" cy="390" r="44"/>
      <rect x="105" y="338" width="10" height="16" rx="2" transform="rotate(0,110,390)"/>
      <rect x="105" y="338" width="10" height="16" rx="2" transform="rotate(45,110,390)"/>
      <rect x="105" y="338" width="10" height="16" rx="2" transform="rotate(90,110,390)"/>
      <rect x="105" y="338" width="10" height="16" rx="2" transform="rotate(135,110,390)"/>
      <rect x="105" y="338" width="10" height="16" rx="2" transform="rotate(180,110,390)"/>
      <rect x="105" y="338" width="10" height="16" rx="2" transform="rotate(225,110,390)"/>
      <rect x="105" y="338" width="10" height="16" rx="2" transform="rotate(270,110,390)"/>
      <rect x="105" y="338" width="10" height="16" rx="2" transform="rotate(315,110,390)"/>
      <circle cx="110" cy="390" r="16" fill="#F5EDD8" stroke="#C0A070"/>
    </g>

    <!-- ═══ LAMPES À VIDE ══════════════════════════════ -->
    <!-- Lampe 1, haut droite -->
    <g opacity=".62" transform="translate(820,50)">
      <rect x="-24" y="-54" width="48" height="88" rx="24" fill="#F2E8D0" stroke="#C8B890" stroke-width="2"/>
      <rect x="-14" y="28"  width="28" height="16" rx="3" fill="#C8B890"/>
      <rect x="-3"  y="44"  width="6"  height="18" fill="#A09060"/>
      <rect x="5"   y="44"  width="6"  height="18" fill="#A09060"/>
      <circle class="filament" cx="0" cy="-10" r="11" fill="#EFA030" opacity=".7"/>
      <circle cx="0" cy="-10" r="5" fill="#FDD060" opacity=".9"/>
      <line x1="0" y1="62" x2="0" y2="90" stroke="#9A8858" stroke-width="2"/>
    </g>
    <!-- Lampe 2, milieu -->
    <g opacity=".52" transform="translate(360,80) scale(.78)">
      <rect x="-24" y="-54" width="48" height="88" rx="24" fill="#F2E8D0" stroke="#C8B890" stroke-width="2"/>
      <rect x="-14" y="28"  width="28" height="16" rx="3" fill="#C8B890"/>
      <rect x="-3"  y="44"  width="6"  height="18" fill="#A09060"/>
      <rect x="5"   y="44"  width="6"  height="18" fill="#A09060"/>
      <circle class="filament filament--2" cx="0" cy="-10" r="11" fill="#EFA030" opacity=".65"/>
      <circle cx="0" cy="-10" r="5" fill="#FDD060" opacity=".85"/>
    </g>

    <!-- ═══ CADRANS ════════════════════════════════════ -->
    <!-- Cadran 1 – milieu haut -->
    <g transform="translate(500,78)" opacity=".55">
      <circle cx="0" cy="0" r="46" fill="#F5EDD8" stroke="#C0A070" stroke-width="3"/>
      <circle cx="0" cy="0" r="40" fill="none" stroke="#D4B87A" stroke-width="1" stroke-dasharray="2 4"/>
      <g stroke="#B09060" stroke-width="1.5">
        <line x1="0" y1="-35" x2="0" y2="-42" transform="rotate(-90)"/>
        <line x1="0" y1="-35" x2="0" y2="-42" transform="rotate(-60)"/>
        <line x1="0" y1="-35" x2="0" y2="-42" transform="rotate(-30)"/>
        <line x1="0" y1="-35" x2="0" y2="-42" transform="rotate(0)"/>
        <line x1="0" y1="-35" x2="0" y2="-42" transform="rotate(30)"/>
        <line x1="0" y1="-35" x2="0" y2="-42" transform="rotate(60)"/>
        <line x1="0" y1="-35" x2="0" y2="-42" transform="rotate(90)"/>
      </g>
      <line class="needle-l" x1="0" y1="8" x2="0" y2="-32"
        stroke="#8B5E3C" stroke-width="2.5" stroke-linecap="round"/>
      <circle cx="0" cy="0" r="5" fill="#C0906A" stroke="#8B5E3C" stroke-width="1.5"/>
      <text text-anchor="middle" y="58"
        font-family="'Courier New',monospace" font-size="10" fill="rgba(100,72,30,.55)">PRESSION</text>
    </g>
    <!-- Cadran 2 – bas gauche -->
    <g transform="translate(270,440)" opacity=".45">
      <circle cx="0" cy="0" r="36" fill="#F5EDD8" stroke="#C0A070" stroke-width="2.5"/>
      <circle cx="0" cy="0" r="30" fill="none" stroke="#D4B87A" stroke-width="1" stroke-dasharray="2 3"/>
      <g stroke="#B09060" stroke-width="1.5">
        <line x1="0" y1="-25" x2="0" y2="-32" transform="rotate(-60)"/>
        <line x1="0" y1="-25" x2="0" y2="-32" transform="rotate(-30)"/>
        <line x1="0" y1="-25" x2="0" y2="-32" transform="rotate(0)"/>
        <line x1="0" y1="-25" x2="0" y2="-32" transform="rotate(30)"/>
        <line x1="0" y1="-25" x2="0" y2="-32" transform="rotate(60)"/>
      </g>
      <line class="needle-r" x1="0" y1="6" x2="0" y2="-24"
        stroke="#8B5E3C" stroke-width="2" stroke-linecap="round"/>
      <circle cx="0" cy="0" r="4" fill="#C0906A" stroke="#8B5E3C" stroke-width="1.5"/>
      <text text-anchor="middle" y="46"
        font-family="'Courier New',monospace" font-size="9" fill="rgba(100,72,30,.5)">VITESSE</text>
    </g>

    <!-- ═══ CARTES PERFORÉES FLOTTANTES ════════════════ -->
    <g class="card card--a" transform="translate(680,280)" opacity=".38">
      <rect x="0" y="0" width="110" height="70" rx="4" fill="#EDE3C4" stroke="#C4A870" stroke-width="1.5"/>
      <rect x="0" y="0" width="110" height="8" fill="#C4A870" opacity=".6"/>
      <g fill="#C8B890">
        <circle cx="14" cy="24" r="3"/><circle cx="14" cy="38" r="3"/><circle cx="14" cy="52" r="3"/>
        <circle cx="28" cy="24" r="3"/><circle cx="28" cy="52" r="3"/>
        <circle cx="42" cy="38" r="3"/><circle cx="42" cy="52" r="3"/>
        <circle cx="56" cy="24" r="3"/><circle cx="56" cy="38" r="3"/>
        <circle cx="70" cy="52" r="3"/>
        <circle cx="84" cy="24" r="3"/><circle cx="84" cy="38" r="3"/>
        <circle cx="98" cy="24" r="3"/><circle cx="98" cy="52" r="3"/>
      </g>
    </g>
    <g class="card card--b" transform="translate(590,390)" opacity=".3">
      <rect x="0" y="0" width="90" height="56" rx="4" fill="#EDE3C4" stroke="#C4A870" stroke-width="1.5"/>
      <rect x="0" y="0" width="90" height="7" fill="#C4A870" opacity=".6"/>
      <g fill="#C8B890">
        <circle cx="12" cy="20" r="2.5"/><circle cx="12" cy="32" r="2.5"/><circle cx="12" cy="44" r="2.5"/>
        <circle cx="24" cy="20" r="2.5"/><circle cx="24" cy="44" r="2.5"/>
        <circle cx="36" cy="32" r="2.5"/>
        <circle cx="48" cy="20" r="2.5"/><circle cx="48" cy="44" r="2.5"/>
        <circle cx="60" cy="32" r="2.5"/><circle cx="60" cy="44" r="2.5"/>
        <circle cx="72" cy="20" r="2.5"/><circle cx="72" cy="32" r="2.5"/>
      </g>
    </g>

    <!-- ═══ BINAIRE EN FILIGRANE ══════════════════════ -->
    <text class="bits bits--a" x="440" y="190">01001101</text>
    <text class="bits bits--b" x="750" y="310">1011</text>
    <text class="bits bits--a" x="120" y="270">01100</text>
    <text class="bits bits--b" x="630" y="130">110100</text>

  </svg>
</div>

<style>
  .bg-canvas {
    position: fixed;
    inset: 0;
    z-index: -1;
    overflow: hidden;
    background: linear-gradient(160deg, #FAF5E8 0%, #F2E8D0 55%, #EDE3C8 100%);
  }

  /* Trame "carte perforée" */
  .bg-punchcard {
    position: absolute;
    inset: 0;
    background-image:
      repeating-linear-gradient(0deg, rgba(110,82,42,0.07) 0 3px, transparent 3px 24px),
      repeating-linear-gradient(90deg, rgba(110,82,42,0.07) 0 6px, transparent 6px 30px);
  }

  .bg-scene {
    position: absolute;
    inset: 0;
    width: 100%;
    height: 100%;
  }

  /* Engrenages */
  @keyframes spin-cw  { to { transform: rotate(360deg);  } }
  @keyframes spin-ccw { to { transform: rotate(-360deg); } }
  .gear--cw  { animation: spin-cw  linear infinite; }
  .gear--ccw { animation: spin-ccw linear infinite; }

  /* Impulsions dans les tuyaux */
  @keyframes pipe-pulse { to { stroke-dashoffset: -28; } }
  .pipe-pulse          { stroke-dasharray: 6 14; animation: pipe-pulse 3s linear infinite; }
  .pipe-pulse--2       { animation-delay: -1.4s; }
  .pipe-pulse--3       { animation-delay: -0.8s; }

  /* Lampes à vide */
  @keyframes glow {
    0%, 100% { opacity: 0.55; }
    50%       { opacity: 1;    }
  }
  .filament   { animation: glow 2.8s ease-in-out infinite; }
  .filament--2 { animation: glow 3.6s ease-in-out infinite 0.6s; }

  /* Aiguilles de cadrans */
  @keyframes needle-l { 0%,100%{transform:rotate(-18deg)} 50%{transform:rotate(14deg)} }
  @keyframes needle-r { 0%,100%{transform:rotate(10deg)}  50%{transform:rotate(-22deg)} }
  .needle-l { transform-origin:0 0; animation: needle-l 5s   ease-in-out infinite; }
  .needle-r { transform-origin:0 0; animation: needle-r 6.5s ease-in-out infinite 0.8s; }

  /* Cartes perforées flottantes */
  @keyframes float-a { 0%,100%{transform:translateY(0) rotate(-3deg)} 50%{transform:translateY(-14px) rotate(-1deg)} }
  @keyframes float-b { 0%,100%{transform:translateY(0) rotate(4deg)}  50%{transform:translateY(-10px) rotate(2deg)}  }
  .card--a { animation: float-a 9s  ease-in-out infinite; }
  .card--b { animation: float-b 11s ease-in-out infinite 1.5s; }

  /* Bits flottants */
  @keyframes drift-a { 0%,100%{transform:translateY(0)} 50%{transform:translateY(-18px)} }
  @keyframes drift-b { 0%,100%{transform:translateY(0)} 50%{transform:translateY(-12px)} }
  .bits {
    font-family: 'Courier New', monospace;
    fill: rgba(100, 72, 30, 0.18);
    font-size: 22px;
  }
  .bits--a { animation: drift-a 13s ease-in-out infinite; }
  .bits--b { animation: drift-b 16s ease-in-out infinite 2s; fill-opacity: 0.15; }

  /* Accessibilité : pas de mouvement */
  @media (prefers-reduced-motion: reduce) {
    .gear--cw, .gear--ccw,
    .pipe-pulse,
    .filament, .filament--2,
    .needle-l, .needle-r,
    .card--a, .card--b,
    .bits--a, .bits--b { animation: none; }
  }
</style>