## Table of contents

-   [Overview](#overview)
-   [Workflow diagram](#workflow-diagram)
-   [Input files](#inputs-files)
-   [Output files](#outputs-files)
-   [Procedure](#procedure)
    -   [Block 0 · Starting
        point](#block-0--starting-point-construir_spec)
    -   [Block 1 - Log-level
        transformation](#block-1---log-level-transformation)
        -   [Block 1 - R documentation](#block-1---r-documentation)
        -   [Block 1 - Our application](#block-1---our-application)
-   [Block 2 · Set Arima Model Identification in Pre-Processing
    Specification](#block-2---arima-model-identification)
    -   [Block 2 - R documentation](#block-2---r-documentation)
    -   [Block 2 - Our application](#block-2---our-application)
        identification\](#block-2–set-arima-model-identification-in-pre-processing-specification)
-   [Block 3 - Outlier](#block-3---outlier-detection)
    -   [Block 3 - R documentation](#block-3---r-documentation)
    -   [Block 3 - Our application](#block-3---our-application)
-   [Block 4 - Calendar effects](#block-4---calendar-effects)
    -   [Block 4 - R documentation](#block-4---r-documentation)
    -   [Block 4 - Our application](#block-4---our-application)

------------------------------------------------------------------------

## Overview

This script automates the **reading and construction of seasonal
adjustment specifications** for multiple time series from a template
organised by indicator (Excel sheet) and Spanish autonomous community
(CCAA). Each specification follows the TRAMO-SEATS method as implemented
in the `rjd3tramoseats` package.

The main workflow is:

1.  Read an Excel workbook with one sheet per indicator
2.  For each row (= CCAA × indicator combination), build a parameterised
    `tramoseats_spec`
3.  Store the specifications in a named list (`specs_lista`) for later
    use in seasonal adjustment

This template could be change for different purposes

## Workflow diagram

    Input time series to be seasonally adjusted. Excel workbook
        │
        ▼
    Excel workbook with the specification
        │
        ├── "Instructions" sheet   → explains the instructions
        ├── "SUMMARY" sheet        → summarizes the models and outliers
        └── Indicator sheets (AFI, IPI, …)
                │
                └── For each row (Autonomous Community / CCAA)
                        │
                        ├── [1] Transformation    → set_transform()
                        ├── [2] ARIMA model       → set_automodel() / set_arima()
                        ├── [3] Outliers          → set_outlier() + add_outlier()
                        ├── [4] Calendar effects  → set_tradingdays() + set_easter()
                        │
                        └── tramoseats_spec → specs_lista[["CCAA.INDICATOR"]]
        │
        ▼
    Seasonal adjustment
        │
        ▼
    Results displayed in a Shiny app

[Back to table of contents](#table-of-contents)

------------------------------------------------------------------------

## Input files

**Data series**

The data that will be seasonally adjusted are stored
in`Datos_entrada/Datos_entrada_indicadores_tot_sa.xlsx`

**Adjustment specifications**

The specification options are provided in the input file
`Datos/entrada/Spec_template.xlsx`. This Excel workbook (`ruta_modelos`)
contains:

-   An **“Instrucciones”** sheet and a **“RESUMEN”** sheet, both
    excluded from processing
-   One sheet per indicator (e.g. `AFI`, `IPI`, `EPA`…), each with one
    row per CCAA
-   The first row of each sheet is a group header and is skipped with
    `skip = 1`

The relevant columns in each sheet are:

<table>
<thead>
<tr>
<th>Column</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>CCAA</code></td>
<td>Autonomous community code</td>
</tr>
<tr>
<td><code>transform</code></td>
<td>Series transformation (<code>None</code>, <code>Log</code>,
<code>Auto</code>)</td>
</tr>
<tr>
<td><code>model.auto</code></td>
<td>Automatic ARIMA model identification?</td>
</tr>
<tr>
<td><code>p</code>, <code>d</code>, <code>q</code>, <code>bp</code>,
<code>bd</code>, <code>bq</code></td>
<td>Orders for the manual ARIMA model</td>
</tr>
<tr>
<td><code>outlier.auto</code></td>
<td>Automatic outlier detection?</td>
</tr>
<tr>
<td><code>auto.cv</code></td>
<td>Critical value for automatic detection</td>
</tr>
<tr>
<td><code>outlier.user</code></td>
<td>Are manual outliers specified?</td>
</tr>
<tr>
<td><code>out_type_k</code>, <code>out_date_k</code></td>
<td>Type and date of the k-th outlier</td>
</tr>
<tr>
<td><code>calendar.auto</code></td>
<td>Automatic calendar effect?</td>
</tr>
<tr>
<td><code>td.type</code>, <code>td.test</code>,
<code>td.leapyear</code></td>
<td>Trading days configuration</td>
</tr>
<tr>
<td><code>easter.type</code>, <code>easter.test</code></td>
<td>Easter effect configuration</td>
</tr>
</tbody>
</table>

[Back to table of contents](#table-of-contents)

------------------------------------------------------------------------

## Output files

We created two output summary excel and RData file to use in the Shiny
app and the seasonally adjustes series. All the ouutput files are in the
file Datos\_salida.

1.  resumen\_sa\_CCAA.xlsx: summary by region
2.  resumen\_sa\_indicador.xlsx: summary by indicator
3.  series\_SA\_CCAA.xlsx: finalseasonally adjusted series
4.  Salida\_des.RData. Outputs from rjd3 used in the Shiny app.

[Back to table of contents](#table-of-contents)

------------------------------------------------------------------------

## Procedure

The main program is **SA\_JDEM3.R** this program read the series to be
seasonality adjusted, read the specification for the template and
performs the seasonal adjustment using rjd3tramoseats for all the series
in the input file.

The auxiliary functions are located inside the file Funciones\_R and
are:

1.  **leer\_modelos.R**: read the specifications from the template.
2.  **SA\_procedure**: reads the series and performs the seasonal
    adjustment using the specifications.
3.  **Analisis\_SA\_function.R**: reates all the outputs from the
    adjustment to analyze and display in the Shiny app.

The **app** is a Shiny application used to display the results of the
seasonal adjustment.

### **Block 0. Starting point:** `construir_spec()`

This function takes one Excel row (as a named list) and the indicator
name, and returns a fully configured `tramoseats_spec` object. It is
structured in four blocks.

    construir_spec <- function(fila, indicador) {
      ccaa <- as.character(fila[["CCAA"]])
      spec <- tramoseats_spec()   # empty base spec
      # ... blocks 1–4
    }

The table below summarises the available predefined specifications in
JDemetra+. Each identifier can be passed directly to `tramoseats_spec()`
as a starting point.

<table>
<colgroup>
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr>
<th>Identifier</th>
<th>Log/level detection</th>
<th>Outliers detection</th>
<th>Calendar effects</th>
<th>ARIMA model</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>RSA0</code> / <code>TR0</code></td>
<td>NA</td>
<td>NA</td>
<td>NA</td>
<td>Airline (+mean)</td>
</tr>
<tr>
<td><code>RSA1</code> / <code>TR1</code></td>
<td>automatic</td>
<td>AO / LS / TC</td>
<td>NA</td>
<td>Airline (+mean)</td>
</tr>
<tr>
<td><code>RSA2</code> / <code>TR2</code></td>
<td>automatic</td>
<td>AO / LS / TC</td>
<td>2 td vars + Easter</td>
<td>Airline (+mean)</td>
</tr>
<tr>
<td><code>RSA3</code> / <code>TR3</code></td>
<td>automatic</td>
<td>AO / LS / TC</td>
<td>NA</td>
<td>automatic</td>
</tr>
<tr>
<td><code>RSA4</code> / <code>TR4</code></td>
<td>automatic</td>
<td>AO / LS / TC</td>
<td>2 td vars + Easter</td>
<td>automatic</td>
</tr>
<tr>
<td><code>RSA5</code> / <code>TR5</code></td>
<td>automatic</td>
<td>AO / LS / TC</td>
<td>7 td vars + Easter</td>
<td>automatic</td>
</tr>
<tr>
<td><code>RSAfull</code> / <code>TRfull</code></td>
<td>automatic</td>
<td>AO / LS / TC</td>
<td>automatic</td>
<td>automatic</td>
</tr>
</tbody>
</table>

The process starts with the **RSA5** specification and changes the
automatic options to those defined by the user through the template

[Back to table of contents](#table-of-contents)

------------------------------------------------------------------------

### Block 1 · Set Log-level transformation

#### Block 1 - R documentation

    set_transform(
      x,
      fun = c(NA, "Auto", "Log", "None"),
      adjust = c(NA, "None", "LeapYear", "LengthOfPeriod"),
      outliers = NA,
      aicdiff = NA,
      fct = NA
    )

##### **`x`**

The specification to customise. Must be a `"SPEC"` class object (see
details).

##### **`fun`**

The transformation applied to the input series: - `"None"` — no
transformation of the series - `"Log"` — takes the log of the series -
`"Auto"` — the program tests for the log-level specification

##### **`adjust`**

Pre-adjustment of the input series for the length of period or leap year
effects: - `"None"` — no adjustment - `"LeapYear"` — leap year effect -
`"LengthOfPeriod"` — length of period

Modifications of this argument are taken into account only when
`fun = "Log"`.

**We don´t understand what is this**

##### **`outliers`**

Boolean indicating whether a pre-correction for large outliers (AO and
LS only) should be applied in the test for the log-level specification
(`fun = "Auto"`). Defaults to `FALSE`.

##### **`aicdiff`**

*(REGARIMA / X-13 specific)* A numeric value defining the difference in
AICC needed to accept no transformation when automatic transformation
selection is chosen. Considered only when `fun = "Auto"`. Default: `-2`.

##### **`fct`**

*(TRAMO specific)* A `numeric` value controlling the bias in the
log/level pre-test: - `fct > 1` favours levels - `fct < 1` favours logs

Considered only when `fun = "Auto"`. Default value is `0.95`.

------------------------------------------------------------------------

#### Block 1 - Our application

The `transform` column is read and applied via `set_transform()`.
Accepted values are `"None"`, `"Log"`, and `"Auto"` (the default
fallback for any unrecognised value).

    transform_val <- as_str(fila[["transform"]])
    if (!is.null(transform_val)) {
      fn <- switch(transform_val,
        "None" = "None",
        "Log"  = "Log",
        "Auto" = "Auto",
        "Auto"           # fallback
      )
      spec <- set_transform(spec, fun = fn)
    }

[Back to table of contents](#table-of-contents)

------------------------------------------------------------------------

### Block 2 · Set Arima Model Identification in Pre-Processing Specification

#### Block 2 - R documentation

    set_automodel(
      x,
      enabled       = NA,
      acceptdefault = NA,
      cancel        = NA,
      ub1           = NA,
      ub2           = NA,
      reducecv      = NA,
      ljungboxlimit = NA,
      tsig          = NA,
      ubfinal       = NA,
      checkmu       = NA,
      mixed         = NA,
      balanced      = NA,
      amicompare    = NA
    )

##### **`x`**

The specification to customise. Must be a `"SPEC"` class object.

##### **`enabled`**

Logical. If `TRUE`, automatic modelling of the ARIMA model is enabled.
If `FALSE`, the ARIMA parameters can be specified manually.

##### **`acceptdefault`**

Logical. If `TRUE`, the default model ARIMA(0,1,1)(0,1,1) will be chosen
in the first step of automatic identification if the Ljung-Box Q
statistics for the residuals are acceptable. No further attempt will be
made to identify a better model. Default: `FALSE`.

##### **`cancel`**

Numeric. Cancellation limit for AR and MA roots to be assumed equal.
Used in the automatic identification of the differencing order. If the
difference in moduli of an AR and an MA root is smaller than this limit,
the two roots cancel out. Default: `0.1`.

##### **`ub1`**

Numeric. First unit root limit. Threshold for the initial unit root test
in the automatic differencing procedure. When a root in the estimation
of the ARIMA(2,0,0)(1,0,0) plus mean model is larger than this value in
modulus, it is set to unity. Default: `1.030928`.

##### **`ub2`**

Numeric. Second unit root limit. When a root in the estimation of the
ARIMA(1,0,1)(1,0,1) plus mean model is larger than this value in
modulus, a common AR/MA factor cancellation is checked. If no
cancellation exists, the AR root is set to unity. Default: `1.136364`.

##### **`reducecv`**

Numeric. Percentage by which the outlier critical value is reduced when
the identified model has an unacceptable Ljung-Box confidence
coefficient. Must be between 0 and 1. The reduced value is set to
`(1 - ReduceCV) × CV`. Only active when automatic outlier identification
is enabled. Default: `0.14268`.

##### **`ljungboxlimit`**

Numeric. Acceptance criterion for the Ljung-Box Q-statistic confidence
interval. If the Q statistic of the final model exceeds this limit, the
model is rejected, the outlier critical value is reduced, and
identification is redone. Default: `0.95`.

##### **`tsig`**

Numeric. Threshold for t-statistics of ARMA coefficients and the
constant term in the final parsimony test. If the highest-order ARMA
coefficient has a t-value smaller than this in magnitude, the model
order is reduced. Default: `1`.

##### **`ubfinal`**

*(REGARIMA / X-13 specific)* Numeric. Final unit root limit. If the
magnitude of an AR root in the final model is smaller than this value, a
unit root is assumed and the differencing order is increased. Must be
greater than 1. Default: `1.05`.

##### **`checkmu`**

*(REGARIMA / X-13 specific)* Logical. Whether the automatic model
selection checks the significance of the constant term.

##### **`mixed`**

*(REGARIMA / X-13 specific)* Logical. Controls whether ARIMA models with
non-seasonal AR and MA terms or seasonal AR and MA terms are considered.
If `FALSE`, models with AR and MA terms in both parts are acceptable
provided there are no terms in either part alone.

##### **`balanced`**

*(REGARIMA / X-13 specific)* Logical. If `TRUE`, the automatic
identification procedure prefers balanced models (order of the combined
AR and differencing operators equals order of the combined MA
operators). Default: `FALSE`.

##### **`amicompare`**

*(TRAMO specific)* Logical. If `TRUE`, the program compares the
automatically identified model to the default ARIMA(0,1,1)(0,1,1) and
selects the best fit based on residual diagnostics, model structure, and
number of outliers.

------------------------------------------------------------------------

#### Block 2 - Our application

If `model.auto = TRUE` (the default), automatic model identification is
enabled via `set_automodel()`. Otherwise, the six ARIMA orders are read
and set with `set_arima()`.

Default orders when cells are empty are
`p=0, d=1, q=1, bp=0, bd=1, bq=1` — the classic airline model
ARIMA(0,1,1)(0,1,1).

    model_auto <- as_bool(fila[["model.auto"]], default = TRUE)
    spec <- set_automodel(spec, enabled = model_auto)

    if (!model_auto) {
      p  <- as_int(fila[["p"]],  default = 0L)
      d  <- as_int(fila[["d"]],  default = 1L)
      q  <- as_int(fila[["q"]],  default = 1L)
      bp <- as_int(fila[["bp"]], default = 0L)
      bd <- as_int(fila[["bd"]], default = 1L)
      bq <- as_int(fila[["bq"]], default = 1L)

      spec <- set_arima(spec,
                        p = p, d = d, q = q,
                        bp = bp, bd = bd, bq = bq,
                        coef.type = "Undefined")
    }

[Back to table of contents](#table-of-contents)

------------------------------------------------------------------------

### Block 3 · Outlier

#### Block 3 - R documentation

    set_outlier(
      x,
      span.type      = c(NA, "All", "From", "To", "Between", "Last", "First", "Excluding"),
      d0             = NULL,
      d1             = NULL,
      n0             = 0,
      n1             = 0,
      outliers.type  = NA,
      critical.value = NA,
      tc.rate        = NA,
      method         = c(NA, "AddOne", "AddAll"),
      maxiter        = NA,
      lsrun          = NA,
      eml.est        = NA
    )

##### **`x`**

The specification to customise. Must be a `"SPEC"` class object.

##### **`span.type`, `d0`, `d1`, `n0`, `n1`**

Parameters to specify the sub-span on which outliers will be detected. -
`d0` and `d1`: characters in `"YYYY-MM-DD"` format specifying the
first/last date of the span when `span.type` is `"From"`, `"To"`, or
`"Between"`. - `n0` and `n1`: numerics specifying the number of periods
at the beginning/end of the series to use for the span (`"From"`,
`"To"`) or to exclude (`"Excluding"`).

##### **`outliers.type`**

Character vector of the outlier types to be automatically detected: -
`"AO"` — additive outliers - `"TC"` — transitory changes - `"LS"` —
level shifts - `"SO"` — seasonal outliers

For example, `outliers.type = c("AO", "LS")` enables detection of
additive outliers and level shifts. If `NULL` or `character()`,
automatic outlier detection is disabled. Default: `c("AO", "LS", "TC")`.

##### **`critical.value`**

Numeric. Critical value for the outlier detection procedure. If `0`, the
critical value is automatically determined by the number of observations
in the detection span. Default: `4` for REGARIMA/X-13, `3.5` for TRAMO.

##### **`tc.rate`**

Rate of decay for the transitory change outlier. Default: `0.7`.

##### **`method`**

*(REGARIMA / X-13 specific)* Determines how detected outliers are
successively added to the model. Currently only `"AddOne"` is supported.

##### **`maxiter`**

*(REGARIMA / X-13 specific)* Maximum number of iterations. Default:
`30`.

##### **`lsrun`**

*(REGARIMA / X-13 specific)* Number of successive level shifts to test
for cancellation. Default: `0`.

##### **`eml.est`**

*(TRAMO specific)* Logical. Controls the estimation method in
intermediate steps. If `TRUE`, exact likelihood estimation is used. If
`FALSE`, the fast Hannan-Rissanen method is applied.

------------------------------------------------------------------------

#### Block 3 - Our application

This block has two independent sub-parts: automatic detection and manual
outlier registration.

#### **1. Automatic detection**

If `outlier.auto = TRUE`, automatic detection is enabled for types `AO`
(additive outlier), `TC` (transitory change), and `LS` (level shift),
with an optional critical value `cv`. If `outlier.auto = FALSE`, all
detection is disabled (`outliers.type = NULL`).

    outlier_auto <- as_bool(fila[["outlier.auto"]], default = FALSE)
    auto_cv      <- suppressWarnings(as.numeric(as_str(fila[["auto.cv"]])))

    if (outlier_auto) {
      spec <- set_outlier(spec,
                          outliers.type = c("AO", "TC", "LS"),
                          cv = if (!is.na(auto_cv)) auto_cv else NULL)
    } else {
      spec <- set_outlier(spec, outliers.type = NULL)
    }

#### **2. Manual outliers**

If `outlier.user = TRUE`, all columns prefixed `out_type_k` and
`out_date_k` are read. For each valid pair (recognised type + valid
date), `add_outlier()` is called. A **duplicate-prevention mechanism**
is implemented via a `pares_vistos` character vector that prevents the
same outlier from being registered twice.

Valid outlier types are: `"AO"`, `"TC"`, `"LS"`, `"SO"` (seasonal
outlier).

    if (as_bool(fila[["outlier.user"]], default = FALSE)) {
      cols_type <- grep("^out_type_", names(fila), value = TRUE)
      cols_date <- grep("^out_date_", names(fila), value = TRUE)
      pares_vistos <- character(0)

      for (k in seq_along(cols_type)) {
        tipo  <- as_str(fila[[cols_type[k]]])
        fecha <- as_fecha(fila[[cols_date[k]]])

        if (is.null(tipo) || !(toupper(tipo) %in% TIPOS_VALIDOS)) next
        if (is.null(fecha)) next

        par_id <- paste0(toupper(tipo), "@", fecha)
        if (par_id %in% pares_vistos) {
          message("    [", ccaa, ".", indicador, "] Duplicate outlier ignored: ", par_id)
          next
        }
        pares_vistos <- c(pares_vistos, par_id)
        spec <- add_outlier(spec, type = toupper(tipo), date = fecha)
      }
    }

[Back to table of contents](#table-of-contents)

------------------------------------------------------------------------

### Block 4 · Calendar effects

Trading days and Easter are configured separately.

#### Block 4 - R documentation

    set_tradingdays(
      x,
      option             = c(NA, "TradingDays", "WorkingDays", "TD2c", "TD3",
                             "TD3c", "TD4", "None", "UserDefined"),
      calendar.name      = NA,
      uservariable       = NA,
      stocktd            = NA,
      test               = c(NA, "None", "Remove", "Add", "Separate_T", "Joint_F"),
      coef               = NA,
      coef.type          = c(NA, "Fixed", "Estimated"),
      automatic          = c(NA, "Unused", "FTest", "WaldTest", "Aic", "Bic"),
      pftd               = NA,
      autoadjust         = NA,
      leapyear           = c(NA, "LeapYear", "LengthOfPeriod", "None"),
      leapyear.coef      = NA,
      leapyear.coef.type = c(NA, "Fixed", "Estimated")
    )

##### **`x`**

The specification to customise. Must be a `"SPEC"` class object.

##### **`option`**

Set of trading days regression variables: - `"TradingDays"` — six
contrast variables, each day (Monday–Saturday) vs Sundays -
`"WorkingDays"` — one working (weekdays) vs non-working (weekends)
contrast variable - `"TD2c"` — one working (Monday–Saturday) vs
non-working (Sunday) contrast variable - `"TD3"` — two contrast
variables: weekdays vs Sundays and Saturdays vs Sundays - `"TD3c"` — two
contrast variables: weekdays (Monday–Thursday) vs Sundays and
Friday+Saturday vs Sundays - `"TD4"` — three contrast variables:
weekdays (Monday–Thursday) vs Sundays, Fridays vs Sundays, Saturdays vs
Sundays - `"None"` — no correction for trading days - `"UserDefined"` —
user-defined trading day regressors

##### **`calendar.name`**

Name (string) of the user-defined calendar to use when generating
built-in regressors set in `option` (if not `"UserDefined"`).

##### **`uservariable`**

Character vector specifying the names of user-defined calendar
regressors. When provided, automatically sets `option = "UserDefined"`.
Names must match those in `modelling_context`.

##### **`stocktd`**

Numeric indicating the day of the month when inventories and stocks are
reported (use `31` for the last day of the month). When specified,
automatically sets `option = "None"`.

##### **`test`**

Pre-test for the significance of trading day regression variables based
on AICC: - `"None"` — trading day variables are not pre-tested and are
included in the model - *(REGARIMA / X-13 specific)* - `"Add"` —
variables are not in the initial model but can be added after the test -
`"Remove"` — variables are in the initial model but can be removed after
the test - *(TRAMO specific)* - `"Separate_T"` — t-test applied to each
variable separately; included if at least one |t| &gt; 2.6 or two |t|
&gt; 2.0 - `"Joint_F"` — joint F-test; effect is significant if F &gt;
0.95

##### **`coef`**

Vector of coefficients for the trading day regressors.

##### **`coef.type`, `leapyear.coef.type`**

Vector defining whether the coefficients are `"Fixed"` or `"Estimated"`.

##### **`automatic`**

Whether calendar effects are added manually (`"Unused"`) or
automatically. Automatic selection can be based on `"FTest"` *(TRAMO
specific)*, `"WaldTest"`, `"Aic"`, or `"Bic"`. The model with the higher
F-value is chosen, provided it exceeds `pftd`.

##### **`pftd`**

*(TRAMO specific)* Numeric. P-value used to assess the significance of
the pre-tested calendar effects.

##### **`autoadjust`**

Logical. If `TRUE`, the program automatically corrects the raw series
for the leap year effect when the leap year regressor is significant.
Only applied when the data is log-transformed.

##### **`leapyear`**

Whether to include the leap year effect in the model: - `"LeapYear"` —
leap year effect - `"LengthOfPeriod"` — length of period *(REGARIMA /
X-13 specific)* - `"None"` — no effect included

Default: a leap year effect regressor is included with any built-in set
of trading day regressors.

##### **`leapyear.coef`**

Coefficient for the leap year regressor.

##### **`leapyear.coef.type`**

Whether the leap year coefficient is `"Fixed"` or `"Estimated"`.

------------------------------------------------------------------------

    set_easter(
      x,
      enabled   = NA,
      julian    = NA,
      duration  = NA,
      test      = c(NA, "Add", "Remove", "None"),
      coef      = NA,
      coef.type = c(NA, "Estimated", "Fixed"),
      type      = c(NA, "Unused", "Standard", "IncludeEaster", "IncludeEasterMonday")
    )

##### **`x`**

The specification to customise. Must be a `"SPEC"` class object.

##### **`enabled`**

Logical. Whether the program considers the Easter effect in the
pre-processing model. Default: `TRUE`.

##### **`julian`**

Logical. Whether the program uses the Julian Easter (expressed in the
Gregorian calendar).

##### **`duration`**

Numeric. Duration of the Easter effect in days (between 1 and 20).
Default: `8` for REGARIMA/X-13, `6` for TRAMO.

##### **`test`**

Pre-test for the significance of the Easter effect based on the
t-statistic (significant if |t| &gt; 1.96): - `"Add"` — the Easter
variable is not included in the initial model but can be added after the
test - `"Remove"` — the Easter variable is in the initial model but can
be removed after the test - `"None"` — the Easter variable is not
pre-tested and is always included in the model

##### **`coef`**

Sets the coefficient of the Easter regressor. The `test` parameter must
be set to `"None"`.

##### **`coef.type`**

Defines the estimation procedure for the Easter regressor coefficient: -
`"Estimated"` — coefficient is estimated - `"Fixed"` — coefficient is
fixed

Default: `"Estimated"`.

##### **`type`**

*(TRAMO specific)* Specifies the presence and length of the Easter
effect: - `"Unused"` — Easter effect is not considered - `"Standard"` —
influences the period of `n` days strictly before Easter Sunday -
`"IncludeEaster"` — influences the entire period (`n`) up to and
including Easter Sunday - `"IncludeEasterMonday"` — influences the
entire period (`n`) up to and including Easter Monday

------------------------------------------------------------------------

#### Block 4 - Our application

#### **1. Automatic mode** (`calendar.auto = TRUE`)

Trading days are set to `"WorkingDays"` with an individual significance
test (`"Separate_T"`), and a standard Easter effect with testing
enabled.

    if (cal_auto) {
      spec <- set_tradingdays(spec, option = "WorkingDays", test = "Separate_T")
      spec <- set_easter(spec, type = "Standard", test = TRUE)
    }

#### **2. Manual mode** (`calendar.auto = FALSE`)

Trading days and Easter are configured separately:

**Trading days:**

-   `td.type`: effect type (e.g. `"WorkingDays"`, `"TradingDays"`,
    `"None"`)
-   `td.test`: whether to apply a significance test (`"Separate_T"` vs
    `"None"`)
-   `td.leapyear`: whether to include a leap-year correction

<!-- -->

    td_type      <- as_str(fila[["td.type"]])
    td_test      <- as_bool(fila[["td.test"]],     default = FALSE)
    td_leapyear  <- as_bool(fila[["td.leapyear"]], default = FALSE)
    leapyear_str <- if (isTRUE(td_leapyear)) "LeapYear" else "None"

    if (!is.null(td_type) && td_type != "None") {
      spec <- set_tradingdays(spec,
                              option   = td_type,
                              leapyear = leapyear_str,
                              test     = if (td_test) "Separate_T" else "None")
    } else {
      spec <- set_tradingdays(spec, option = "None")
    }

**Easter effect:**

-   `easter.type`: effect type (e.g. `"Standard"`, `"Unused"`)
-   `easter.test`: whether to apply a significance test

<!-- -->

    easter_test <- as_bool(fila[["easter.test"]], default = FALSE)
    easter_type <- as_str(fila[["easter.type"]])

    if (!is.null(easter_type) && easter_type != "Unused") {
      spec <- set_easter(spec, type = easter_type, test = easter_test)
    } else {
      spec <- set_easter(spec)
    }

[Back to table of contents](#table-of-contents)

------------------------------------------------------------------------
