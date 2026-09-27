# help.R -- the text of the Start here tab.

HELP_MD <- '
## What this is

An **AI Survey Methodologist** that answers from your own JPSM coursework:
lecture slides and notes, the *Practical Tools for Designing and Weighting
Survey Samples* textbook, and the Quarto/R code from assignments, labs and exams.
For every question it retrieves the most relevant passages, answers from them,
and cites each one as **[S1]**, **[S2]** ... with a link to the file.

It is built to behave like a design-based survey statistician: it assumes a
complex design, copies formulas in the notation of the course that states them,
and writes tidyverse R. Those habits are written down (see *Reflexes* below)
rather than left to the model.

## How to use it

1. Paste your API key below and press **Use this key**.
2. Open **2. Methodologist** and ask. Good questions name the thing you want:
   *"formula for the design effect due to interviewers"*,
   *"how do I rake to age and region margins in R"*,
   *"why subset the design rather than the data for a domain estimate"*.
3. Follow-ups keep the thread. **New conversation** clears it.

## What to trust, and how to check

- **Formulas** are rendered from LaTeX. Under each answer an **audit** line says
  whether every displayed formula was found in a retrieved source. A formula
  flagged there came from the model, not your notes -- check it.
- **Citations** are checked too: a label that was not retrieved is flagged.
- **Sources** open the file in the coursework repository (PDFs at the page).
- If the materials do not cover a question the assistant should say so. It is
  not a general-purpose chatbot and is told not to fill gaps silently.

## Your key

The key is held for this browser session only. It is never written to disk,
never placed in the server environment, and is discarded when you close the
tab. Every question makes one embedding call (to find sources) and one chat call.

## Limits

- Scanned or image-only PDFs, and the rendered PDFs of assignments (whose Quarto
  source is indexed instead), are not in the store.
- The store is a snapshot; it is rebuilt with `build_store.R` when the
  coursework changes.
'

help_html <- function() shiny::HTML(commonmark::markdown_html(HELP_MD))

reflex_table_html <- function() {
  rows <- purrr::map_chr(SVY_REFLEXES, function(r)
    sprintf("<tr><td><code>%s</code></td><td>%s</td><td>%s</td></tr>",
            r$id, htmltools::htmlEscape(r$say),
            htmltools::htmlEscape(if (is.na(r$never)) "" else r$never)))
  shiny::HTML(paste0(
    "<table class='table table-sm small'><thead><tr><th>Reflex</th><th>Does</th>",
    "<th>Must not</th></tr></thead><tbody>", paste(rows, collapse = ""),
    "</tbody></table><p class='small text-muted'>Prohibitions: ",
    htmltools::htmlEscape(paste(SVY_PROHIBITIONS, collapse = " ")), "</p>"))
}
