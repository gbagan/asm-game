let victoryAudio: HTMLAudioElement = new Audio("./victory.opus");
let stepAudio: HTMLAudioElement = new Audio("./step.opus");
let failureAudio: HTMLAudioElement = new Audio("./failure.opus");
let discardAudio: HTMLAudioElement = new Audio("./discard2.opus");
let dragAudio: HTMLAudioElement = new Audio("./drag.opus");
let dropAudio: HTMLAudioElement = new Audio("./drop.opus");

export function playVictorySound() {
  victoryAudio.volume = 0.6;
  victoryAudio.currentTime = 0;
  victoryAudio.play();
}

export function playStepSound() {
  stepAudio.volume = 1;
  stepAudio.currentTime = 0;
  stepAudio.play();
}

export function playFailureSound() {
  failureAudio.volume = 1;
  failureAudio.currentTime = 0;
  failureAudio.play();
}

export function playDiscardSound() {
  discardAudio.volume = 1;
  discardAudio.currentTime = 0;
  discardAudio.play();
}

export function playDragSound() {
  dragAudio.volume = 0.3;
  dragAudio.currentTime = 0;
  dragAudio.play();
}

export function playDropSound() {
  dropAudio.volume = 0.5;
  dropAudio.currentTime = 0;
  dropAudio.play();
}