# Grouped factor settings prototype

Branch: `codex/grouped-factor-settings`

Screenshots: `grouped-factor-settings-desktop.png` and `grouped-factor-settings-narrow.png` in this directory.

## Demonstrated workflow

The prototype was exercised in a live Shiny end-to-end test and in the desktop screenshot:

1. Select two treatment factors and receive `facA` / `facA1, facA2` and `facB` / `facB1, facB2`.
2. Rename the factors to Diet and Time, with `Control, Supplement` and `Week 1, Week 2` levels.
3. Increase Diet to three levels. The field becomes `Control, Supplement, facA3`; Time is unchanged.
4. Add the internal `facA : facB` interaction. The selector and model preview display `Diet × Time` while the statistical identifier remains unchanged.
5. Switch to t-tests. Diet, Time, and Diet × Time appear as effect choices; Time is available as a conditioning factor.
6. Run a CRD analysis and verify custom labels in expected-response rows, omnibus/contrast results, and the export translation layer.
7. Switch to split-plot and verify separately headed Whole-plot factors and Subplot factors editors, with custom names translated from stable `*.main` / `*.sub` identifiers.

The end-to-end test also adds and removes a third factor, confirming that Diet and Time retain their settings. Pure and server tests cover generated defaults, append-on-grow behavior, explicit shrink warnings with retained customized names, unique factor/level validation, split-plot separation, and internal/display identifier separation.

## UX tradeoffs

Strengths:

- The three related settings are visible in one scan and consume one compact row per factor.
- Live `X of Y level names entered` feedback makes comma-separated completeness visible.
- Existing custom names are retained on level-count reduction and accompanied by an explicit action-oriented warning.
- The same row pattern scales well for experiments with several factors and stacks on phone-width screens.

Weaknesses:

- Users must understand comma-separated entry, and commas cannot be part of a level name.
- Long level-name lists remain dense and may require horizontal text-field scrolling.
- Correcting one invalid item in a long comma-separated list is less guided than editing separate level inputs.

## Maintenance risks

- The grouped inputs keep the legacy hidden `level_numbers*` vectors synchronized for the existing pwr4exp pipeline. Future changes should treat the visible numeric inputs as the source of truth and preserve that compatibility contract until the calculation layer is refactored.
- Dynamic Shiny input rebinding is timing-sensitive in automated tests. The end-to-end checks wait for both factor feedback rows to report ready before entering custom values.
