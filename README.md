# dsh-r-workbench

A DeepSeek Harness bundle that turns the local R toolchain into six model-facing tools: acquisition, cleaning, a data-value gate, statistical analysis, reporting, and an R console.

The gate is the product: when the cleaned data cannot support a meaningful analysis, `r_assess` returns `STOP` and the analysis/report stages refuse to run. See [docs/DETAIL.md](docs/DETAIL.md) for the user-facing behavior and rules.

## Install

```sh
dsh plugin --profile web add github:HougeLangley/dsh-r-workbench
```

Then call `r_env` first. The plugin needs a local R installation; `r_env` reports missing packages and prints the exact `Rscript r/install-deps.R` command. RStudio and Quarto are optional and detected when present.

## Layout

```text
index.js              Host plugin: six tools, run-directory lifecycle, R subprocess runner
r/common.R            Shared R helpers (JSON I/O, hashing, HTML/Markdown, fonts, stage wrapper)
r/env.R               Toolchain inspection
r/ingest.R            Readers + cleaning
r/assess.R            Six-dimension value gate
r/analyze.R           Tests, effect sizes, BH correction, tables, charts
r/report.R            HTML/Markdown/Quarto/DOCX composition
r/run.R               Ad-hoc R console
r/install-deps.R      User-library installer for required/optional packages
cordis.patch.yml      Bundle patch (inserts the `r-workbench` row)
locale/{en,zh}.json   Plugin Manager metadata
assets/icon-180.png   Raster icon for storefronts
```
## Stage contract

`index.js` invokes `Rscript --vanilla r/<stage>.R <config.json>` with the run directory as cwd. The config carries `runDir`, `runId`, `pluginVersion`, and stage-specific options. Each stage writes `logs/<stage>-result.json`:

```json
{ "ok": true, "stage": "assess", "data": { "...": "..." }, "elapsedSec": 0.4 }
```

The Node tool reads only `data`; `ok: false` carries `error` and is surfaced as a tool failure. Artifacts live in the run directory, never only in the tool result, so a later turn can inspect them.

## Gate enforcement

- `r_assess` writes `assessment.json` with the SHA-256 of `data/clean.csv`.
- `r_analyze` and `r_report` call `rw_gate()`, which rejects a missing/stale assessment, rejects `STOP`, and marks `CONDITIONAL` runs as exploratory.
- `r_report` additionally requires `analysis.json` for the same data hash.

## Install

From a source checkout of this directory:

```js
plugin_manager({ action: "install_bundle", target: "/absolute/path/to/r-workbench" })
```

The bundle has no runtime npm dependencies; it uses Node built-ins and the local `Rscript`/`R` installation. R packages are installed into the user library by `Rscript r/install-deps.R`.

## Testing

`tests/run-tests.sh` exercises the R stages directly against generated fixtures (valuable CSV, GB18030 text with preamble, Excel with dates, JSON, ZIP, WPS-compatible `.et`, a 20-row CONDITIONAL sample, and a deliberately low-value workbook). It verifies:

- ingest/merge row counts, encoding detection, leading-zero preservation, and multi-sheet skip disclosure;
- `PROCEED`, `CONDITIONAL`, and `STOP` decisions, including exploratory labeling;
- `STOP` refusal by both `r_analyze` and `r_report`;
- tampered `clean.rds` rejection (the gate is bound to both data artifacts);
- calendar-month slope on a dataset with missing months;
- HTML report content and DOCX `word/document.xml` structure when pandoc is available.

The real acceptance check is calling the installed tools from the Harness, not this script.
