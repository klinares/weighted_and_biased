# RAG Time

```
  ♪ ♫  ─────────────────────────────────────────────────────────  ♫ ♪
   ___    _    ___    _____ _
  | _ \  /_\  / __|  |_   _(_)_ __  ___
  |   / / _ \| (_ |    | | | | '  \/ -_)     an AI Survey Methodologist
  |_|_\/_/ \_\\___|    |_| |_|_|_|_\___|     in syncopated time
  ─────────────────────────────────────────────────────────────────────
   \ud834\udd1e  retrieve · augment · generate  —  left hand keeps the design,
                                         right hand plays the answer
  ─────────────────────────────────────────────────────────────────────
```

*Ragtime is two hands doing different jobs: a steady bass (the stride) and a
syncopated melody on top. RAG Time works the same way. The **left hand** is
steady and never improvises — your JPSM coursework, searched the same way every
time. The **right hand** is the model, free to phrase the answer, but only over
the chords the left hand plays.*

---

## The score  (what's in the folder)

```
rag_time/
├── shiny_app.R          ♩  THE app. Run this, deploy this.
├── R/                   ♩  everything the app needs (sourced by shiny_app.R)
│   ├── config.R            endpoints, models, settings  ← edit model names here
│   ├── llm.R               keys (per session), chat + embedding calls
│   ├── retrieve.R          vector + keyword search in DuckDB, rank fusion
│   ├── reflexes.R          the methodologist's habits, prohibitions, answer audit
│   ├── render.R            markdown + LaTeX (KaTeX → MathML), sources
│   ├── help.R, ui_start.R  tab 1: Start here (docs + your key)
│   ├── ui_chat.R           tab 2: Methodologist (the chat)
│   └── llm_openrouter.R    optional provider override — delete where not used
├── store/jpsm.duckdb    ♩  the index (~70 MB, fits GitHub)
├── build_store.R        \ud834\udd3d  rebuilds the store from the coursework (not deployed)
├── parse_textbook.R     \ud834\udd3d  textbook PDF → one markdown file per chapter
├── build/extract.R      \ud834\udd3d  PDF/Quarto text extraction for the build
├── eval/eval_rag.R      \ud834\udd3d  accuracy check (not deployed)
├── deploy.R             \ud834\udd3d  publish to Posit Connect
└── _archive/            \ud834\udd3d  earlier attempts, kept only for reference — not used
```

♩ = played on stage (deployed) · \ud834\udd3d = rest (stays on your machine)

## Tempo marking  (how to play it)

Every model call goes through **ellmer** to an OpenAI endpoint (`R/llm.R`,
settings in `R/config.R`). Embeddings go to the same endpoint's `/embeddings`.

1. `.Renviron`:
   ```
   OPENAI_API_KEY=...
   SVYMETH_CORPUS=<path to UMD_JPSM_coursework>
   SVYMETH_CACHE=<a scratch folder>        # optional; speeds up rebuilds
   ```
2. In `R/config.R`, set `chat` and `worker` to the model names your endpoint
   serves (or set `SVYMETH_CHAT_MODEL` / `SVYMETH_WORKER_MODEL`).
3. `source("build_store.R")` — embeddings only (~100 calls, no chat model).
   Resumes if interrupted.
4. `shiny::runApp("shiny_app.R")` to check, then `source("deploy.R")`.
   On Connect set `SVYMETH_KEY_SOURCE=analyst`.

Tab **1. Start here** has the docs and a box for your key. Tab **2.
Methodologist** is the chat. Every answer cites `[S1]`, `[S2]`… with links to
the files, renders formulas from LaTeX, and ends with an audit line that flags
any formula or citation not found in the retrieved sources.

**Other providers.** `R/llm_openrouter.R` is an optional override that points
the same code at OpenRouter. Where that is not available, delete the file.

**The textbook.** `parse_textbook.R` splits the Practical Tools PDF into one
markdown file per chapter under `UMD_JPSM_coursework/practical_tools_textbook/`
(`SVYMETH_BOOK` = the PDF). The build then indexes those files like any other
course file. The textbook is copyrighted: keep that repository private.

## The rests  (what the build leaves out on purpose)

So the build never stalls on the wrong files:
rendered PDFs in `assignments/` (their `.qmd` is used), any PDF with a
`.qmd`/`.Rmd` twin, `readings/`, `data/`, `*_files/` figure folders, syllabi and
admin PDFs, data/binary files. Image-only PDFs are listed and skipped.

## Models  (who plays which part)

| Part | Model | Set in |
|---|---|---|
| Chat (talks to analysts) | your endpoint's main model | `R/config.R` |
| Worker (evaluation) | a smaller, cheaper model | `R/config.R` |
| Embeddings | text-embedding-3-small, 512 dims | `R/config.R` (must match the store) |

The embedding model must match the store; the app refuses to search otherwise.

## Last recital  (accuracy check, 108 questions)

Right source file in top 5: **88%** · top 10: **92%** · right course in top 5: **93%**.
Judged answer score (40 questions): 0.86 with a frontier chat model, 0.68 with
a small open model. Weakest spot: R-code questions, where models substitute a package
for the course's own code.

## Instruments

shiny, bslib, shinychat, ellmer, httr2, duckdb, DBI, dplyr, purrr, stringr,
tibble, katex, commonmark, htmltools — plus ragnar, pdftools, stringi, fs,
tidyr, rlang to build.

```
  ♪  fin.  "Don't play this piece fast. It is never right to play ragtime fast."
                                                    — Scott Joplin
```

