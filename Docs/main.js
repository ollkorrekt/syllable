// main.js
// Main app logic translated from Haskell to JavaScript

import { randomSyllable } from './syllable.js';

// Test the proportion of 'El' nuclei in n random samples
function testNucProp(n) {
  let countL = 0;
  for (let i = 0; i < n; i++) {
    const nuc = randomNucleus();
    if (isL(nuc)) countL++;
  }
  const lProp = countL / n;
  const trueProp = 1 / 3;
  console.log("proportion more Els than expected: " + (lProp - trueProp));
}

// Render a random syllable to the page
function renderRandomSyllable() {
  const syl = randomSyllable();
  // Format the syllable as a string
  const text = `${syl.onset}${syl.nucleus.value}${syl.coda}`;
  const el = document.getElementById('syllable');
  if (el) el.textContent = text;
}

window.addEventListener('DOMContentLoaded', () => {
  const btn = document.getElementById('generate');
  if (btn) btn.addEventListener('click', renderRandomSyllable);
});
