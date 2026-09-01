# Known correlation-structure issues

Status: open; documented on 2026-09-01  
Verified with `pwr4exp` 1.0.1 and `nlme` 3.1-170.

## Scope

General Design accepts residual correlation structures created by `nlme` and
passes them to `pwr4exp::mkdesign()`. The issues below concern validation,
coercion, and the mapping of user input to those structures. They are not
evidence that `pwr4exp` computes incorrect power when it receives a valid,
correctly ordered correlation structure.

Responsibility is shared across three layers:

- `nlme` defines the mathematical and data-shape requirements for each
  correlation class.
- `pwr4exp` initializes the supplied `nlme` object and currently performs some
  permissive coercions before doing so.
- This app is responsible for presenting valid type-specific controls and for
  preserving the meaning and ordering of values entered by the user.

## Tracked issues

### COR-001: `corSymm` index and matrix ordering

Severity: high.

`nlme::corSymm` requires an integer-valued index whose unique values form a
consecutive sequence. `pwr4exp` attempts to convert a non-integer index with
`as.integer()`. This behavior has two consequences:

1. Numeric indices with gaps or unsuitable fractional values can fail during
   `nlme::Initialize()`.
2. Factor indices are converted according to factor-level order, while the
   app labels the manual correlation matrix in first-appearance order. If
   those orders differ, the entered correlations can be assigned to the wrong
   pairs of levels without an obvious error.

For example, data appearing in the order `post, pre, mid` can have factor
levels ordered as `mid, post, pre`. A matrix labeled in appearance order is
then interpreted in factor-level order.

Temporary workaround:

- Supply an explicit integer index `1, ..., m` within each group.
- Ensure the integer order exactly matches the row and column order displayed
  in the correlation matrix editor.

Planned fix:

- Build a private index with `match(index, displayed_levels)` and pass that
  index to `corSymm`.
- Use one shared `displayed_levels` value for the table labels, matrix values,
  and model index.
- Reject duplicate index values within a group and incomplete level mappings.

### COR-002: `corCAR1` uses the wrong UI range for rho

Severity: medium.

`corCAR1` requires `0 < rho < 1`. The app currently shares a rho slider with
`corAR1` and `corCompSymm`, allowing values from `-0.99` through `0.99`.
Negative values and zero therefore reach the constructor and fail.

Temporary workaround: use a strictly positive rho smaller than one.

Planned fix: render a `corCAR1`-specific slider and validate the open interval
in `build_correlation_spec()`.

### COR-003: `corCompSymm` has a group-size-dependent lower bound

Severity: medium.

For a group containing `m` correlated observations, compound symmetry
requires:

```text
-1 / (m - 1) < rho < 1
```

The common slider permits values down to `-0.99`, which can be invalid. For
example, with three observations per group, rho must be greater than `-0.5`.
In addition, `corCompSymm` normally only needs a grouping formula such as
`~ 1 | subject`; requiring a time variable in the app is unnecessary.

Temporary workaround: calculate the lower bound using the largest group and
choose rho above that bound.

Planned fix:

- Derive a safe lower bound from the uploaded data.
- Remove the required time selector and use `~ 1 | group`.
- Validate the resulting structure before power calculation.

### COR-004: AR1 and ARMA time values can be silently truncated

Severity: medium.

`corAR1` and `corARMA` use integer-valued time/order covariates. `pwr4exp`
currently applies `as.integer()` to other storage types. Numeric values may
therefore be truncated, and distinct times can collapse to the same integer.
The app converts nonnumeric values but does not reject non-integer numeric
values.

For ARMA, the app also converts `p` and `q` with `as.integer()` before fully
checking them, so fractional orders can be silently truncated. Individual
coefficients inside `(-1, 1)` are not sufficient to guarantee a stationary
and invertible ARMA structure; `nlme` may reject some combinations later.

Temporary workaround:

- Use explicit, distinct integer time values.
- Enter nonnegative integer values for `p` and `q`.
- Treat an initialization error as an invalid coefficient combination.

Planned fix: validate integer-valued time, `p`, and `q` before coercion and
surface `nlme` stationarity/invertibility errors next to the inputs.

### COR-005: spatial coordinates are not required to be numeric

Severity: high.

The spatial structures `corExp`, `corGaus`, `corLin`, `corRatio`, and
`corSpher` require meaningful numeric coordinates. The app currently checks
only that selected columns exist. Categorical coordinates can pass far enough
to be converted into artificial dummy-variable distances, and `pwr4exp` can
emit coercion warnings while ordering the data. A calculation based on those
distances would not represent the uploaded spatial coordinates.

Temporary workaround: select only finite numeric coordinate columns and use
consistent units across dimensions.

Planned fix:

- Restrict coordinate selectors to numeric columns.
- Reject missing or non-finite coordinates.
- Reject duplicate coordinate tuples within a group when the selected
  structure cannot support them.

### COR-006: small ranges for `corLin` and `corSpher` may be adjusted

Severity: low.

When the initial range is smaller than the minimum observed distance, `nlme`
can replace it with approximately `1.1` times that minimum and emit a warning.
The app does not currently show that adjustment clearly, so the initialized
structure may not use the exact value entered by the user.

Temporary workaround: choose a range larger than the minimum nonzero
within-group distance.

Planned fix: calculate and display the minimum distance, validate the entered
range, and turn any automatic adjustment warning into explicit feedback.

## Confirmed behavior

The following checks were reproduced through `pwr4exp::mkdesign()` or its
internal correlation initialization path:

- `corSymm` works with correctly ordered consecutive integer indices.
- `corSymm` fails with integer gaps such as `1, 3, 5`.
- A factor-backed `corSymm` can initialize but can permute the intended matrix
  when factor-level order differs from the app's displayed order.
- `corCAR1` rejects negative rho, zero, and one.
- `corCompSymm` rejects rho `-0.9` for groups of size three.
- A categorical spatial coordinate produces coercion warnings and does not
  represent a meaningful numeric distance.

## Implementation locations

- Correlation UI and validation: `app.R`, around `cor_params_ui` and
  `cor_validation_ui`.
- Correlation construction: `R/app_helpers.R`, in
  `build_correlation_spec()`.
- App-to-package integration: `app.R`, in `build_correlation()` and the
  General Design calculation path.

## Tests to add with future fixes

- A `corSymm` factor whose first-appearance order differs from its factor-level
  order.
- `corSymm` indices with gaps, duplicates, missing levels, and unequal group
  ordering.
- Boundary and out-of-range rho values for AR1, CAR1, and compound symmetry.
- Fractional ARMA orders and nonstationary/invertible coefficient vectors.
- Factor, character, missing, infinite, and multidimensional spatial
  coordinates.
- `corLin` and `corSpher` ranges below and above the minimum observed distance.

