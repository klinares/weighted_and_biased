# Presenter Script — "Garbage In, Inference Out?"
**Audience:** public opinion analysts · **Length:** ~30 minutes (34 slides) · **Cut freely** — each deep dive (slides 14–19) stands alone, so dropping any of them doesn't break the arc.

Timing plan: dividers ~10 sec each; content slides 50–70 sec; total ≈ 29–31 min. The same notes are embedded in the PowerPoint's speaker-notes pane.

---

**1 · Title — 0:30**
Good morning. Two years ago I started a master's at the University of Maryland's Joint Program in Survey Methodology, and this talk is what I'm bringing back. The title is the thesis: the quality of any inference we publish is bounded by the quality of what went in — and survey methodology gives us a complete, named vocabulary for "what went in." That vocabulary is Total Survey Error, and it extends past surveys to big data and AI.

**2 · Roadmap — 0:40**
Quick map. First, why this matters for us as public opinion analysts. Then the two years in one slide, the Total Survey Error foundation, and six deep dives — coverage, response rates, weighting, measurement, analytic error, and what survives in a tracking series. After that, the AI landscape in survey research, my capstone, where I trained, and two concrete commitments I'd like us to adopt.

**3 · Two years at a glance — 1:00**
The degree in one slide, organized by domain rather than course number. Sampling design and data collection on the representation side; measurement and questionnaires on the measurement side; total survey error tying them together; then complex-sample analysis, statistical inference, machine learning and big data, and a computing-and-visualization backbone. The color accents preview the deck's logic — blue for representation, amber for measurement, green for new methods. The point: this wasn't a statistics degree or a data-science degree. It was both, anchored to how data is actually made.

**4 · What I'm bringing back — 0:50**
Four ideas. One: the data our analyses ride on has quality we must defend, not assume. Two: JPSM gave me a vendor-neutral framework for reasoning about that quality, whether the data is a designed survey or something found. Three: that framework is Total Survey Error, and it anchors everything in this deck. Four: AI doesn't get a pass — new tools are evaluated through this foundation, not around it.

**5 · Divider: Foundation — 0:10**
Let's build the foundation.

**6 · A map of where error enters — 1:00**
TSE's core move: treat a statistic's accuracy as the accumulation of distinct, named error sources rather than one number. Two arms. Representation — the "who": coverage, sampling, nonresponse, adjustment. Measurement — the "what": validity, measurement, processing. Two honest caveats: errors attach to specific estimates, not to "the survey" — and the framework has known limits: it centers on accuracy, can imply error sources are independent when they're often correlated, and many bias components are rarely measured.

**7 · Where error enters (Groves) — 1:00**
The same idea as a flowchart. Across the top, the measurement arm: construct, measurement, response, edited response — validity, measurement error, processing error entering between steps. Across the bottom, the representation arm: target population, frame, sample, respondents, adjustments — coverage, sampling, nonresponse, adjustment error. Both converge on the survey statistic. Two things to notice: every poll we cite traveled this whole path, and as analysts we usually receive the data at the far right — after most of the error has already happened.

**8 · Accuracy is the inverse of MSE — 1:20**
The framework's core number. Mean squared error equals bias squared plus variance; accuracy is its inverse — the smaller the MSE, the more accurate the estimate. MSE is never zero; even a census carries measurement and processing error. Now the expanded decomposition, with each component colored by its arm — and notice something important: bias and variance do *not* split cleanly across the arms. The representation arm contributes the most familiar variance of all — sampling — *and* the biggest biases, coverage and nonresponse. The measurement arm contributes bias, like social desirability, *and* variance, like interviewer effects. Both arms feed both terms. The margin of error you see published covers only one slice of this.

**9 · The Total Error Framework — 1:10**
TSE was built for designed surveys, but in 2020 Amaya, Biemer, and Kinyon extended it to big data — they call it the Total Error Framework, eight error components. The survey process and the big-data process run in parallel: where we construct a frame, big data generates and identifies sources; where we draw a sample, design a questionnaire, field, and process, big data runs extract-transform-load. Both paths converge on shared modeling, estimation, and inference — where modeling and analytic error live for everyone. The payoff: one vocabulary to compare a designed poll and a scraped dataset error-for-error.

**10 · Bringing in big data — with caveats — 1:10**
So can social media measure public opinion? It can *supplement* — cheaper, faster, timelier — but it carries its own total error, and the TEF is the lens. Four caveats. Coverage: the platform is not the target population. Content: people curate a best self, with no expectation of privacy, which inflates measurement error. Query and interpretation: keyword scraping and coding decisions add error of their own. And validity can drift — Google Flu Trends was accurate until algorithms and search habits changed. Quantity is not validity. Bottom line: found data complements probability samples, calibrated against them; it doesn't replace them.

**11 · Fitness for use — 0:50**
Accuracy is only one dimension of quality. Brackstone's six: relevance, accuracy, timeliness, accessibility, interpretability, coherence. A perfectly accurate estimate delivered after the decision is low quality. And good design weighs cost against error — the levers interact, so pushing one, like the response rate, can raise total error elsewhere.

**12 · The analyst's vantage point — 0:50**
West, Heeringa, and Berglund's *Applied Survey Data Analysis* makes the analyst's position explicit: we usually receive data only after it's designed, collected, and processed, so analytic quality is bounded by every upstream decision. The book's six-step process — define the question, understand the design, prepare weights, run descriptive checks, fit models, interpret with design-based inference — is the discipline. The new edition adds differential privacy and disclosure risk, which increasingly affect the public files we analyze.

**13 · Divider: Deep dives — 0:10**
Now six deep dives into the error sources polling lives with.

**14 · Coverage — 1:00**
Coverage error: failure to include all elements of the target population in the sampling frame. The everyday version: "the poll was online-only" is a coverage problem, not a sampling problem — the offline population never had a *chance* of selection, and no sample size fixes that. The bias formula has two levers: how large the uncovered share is, and how different the uncovered are on the outcome. The trap is that the uncovered are invisible in your own data — you need external benchmarks like the ACS, or studies that measure frame membership. Repairs: redefine the population honestly, combine frames, link missing units, benchmark.

**15 · Response rates — 1:20**
The number everyone asks about. AAPOR gives us standard definitions — RR1 the most conservative, counting only completes, through RR4 the most liberal, adding partials and estimated eligibility. Useful, comparable — and not a quality grade. Groves' 2006 meta-analysis of thirty studies: nonresponse bias averaged about nine percent in absolute relative terms, and the correlation between the response rate and the bias was only about 0.33. Why? Bias lives in the relationship between *who responds* and *what you're measuring* — so it differs estimate by estimate within one survey. The takeaway from coursework, verbatim: a balanced forty percent can beat a skewed sixty. Pushing the rate up helps only if it brings in different people.

**16 · Weighting — 1:10**
Weighting is how we repair nonresponse and coverage, and the folklore says it always trades bias for variance. Little and Vartivarian showed that's an oversimplification — it depends on the adjustment variables. This two-by-two: a variable related only to nonresponse buys you variance and removes no bias. A variable strongly related to *both* the outcome and response — bottom right — reduces both. That's the target for every adjustment. The toolbox is standard: weighting classes, propensity weights, poststratification, raking. The cautionary class example: upweight low-responding men four-fold in a GPA study when gender doesn't predict GPA, and you've added noise for nothing. And for machine-learning weighting: the algorithm matters less than the variables.

**17 · Measurement — 1:20**
For public opinion work this is the most visceral slide: the question is an instrument, and instruments can be miscalibrated. Wording effects are not small — alcohol response scales changed reports of heavy drinking, and gambling-expenditure wording produced five-fold differences in reported spending. Respondents satisfice: shortcutting rises with task difficulty and falls with ability and motivation — straightlining is the symptom. Memory and self-presentation distort: telescoping pulls events into the reference period, and social desirability edits sensitive answers — in one validation, tobacco self-reports agreed with saliva 87.5 percent of the time. And interviewers add *variance*: the design effect grows with the intraclass correlation and the workload. The defenses are standard tools: cognitive testing, behavior coding, record checks, split-ballot experiments.

**18 · Analytic error — 1:00**
The last mile, and the one entirely within our control: the failure of the data user to employ appropriate estimation methods. The headline: in a metadata review, roughly half of published secondary analyses did not use the survey weights — and prevalence hadn't improved over time. What design-based analysis means in practice: weights make point estimates speak for the population; strata and clusters are needed for honest standard errors — skip them and you typically understate uncertainty and overstate significance. Unlike coverage or nonresponse, this needs no budget to fix. That's what the six-step process is for.

**19 · Levels vs. trends — 1:10**
The most reassuring finding in the deck, from Curtin, Presser, and Singer's study of the Index of Consumer Sentiment. Who's hard to reach is systematic: respondents requiring many calls skewed younger, more affluent, more optimistic; refusal conversions skewed older, less educated, less optimistic. So response propensity correlates with the very thing being measured — and *levels* are vulnerable. But estimates of *change over time* stayed remarkably robust, because harder and easier respondents change at similar rates. For trackers: keep the design consistent and disclose changes — a level shift right after a design change may be method, not opinion.

**20 · Divider: AI landscape — 0:10**
Now the part everyone's asking about, through Buskirk's 2026 keynote.

**21 · What do we mean by AI — 0:50**
Definitions first, because "AI" gets used for everything. A nested simplification: large language models are one kind of generative AI, built on deep learning, inside machine learning, inside AI. The property that matters for us: an LLM learns statistical patterns in language and is not deterministic — it generates probable, sometimes variable, text. That's exactly why we treat its output as a new error source rather than an oracle.

**22 · Where does AI belong — 0:50**
Buskirk's central question, and a litmus test you can apply to any pitch: is the AI *operating* the survey, or making an *inferential* claim? Use AI to operate — detect problems earlier, adapt in the field, reduce burden. Keep inference, transparency, reproducibility, and validity human-led; AI informs those decisions, it doesn't make them. Probability surveys have always been human-in-the-loop, and the near-term future is intentional augmentation — and notice the reverse direction: probability samples can help align and correct AI, not only the other way around.

**23 · Promise & precaution — 1:10**
The balance sheet across the survey process. Pre-collection: models can draft and diversify items and support readability — but they stumble on negatively-keyed items, and open-source readability tools are still more reliable. Collection: conversational probing can raise open-ended detail, and "silicon" respondents are being explored for hard-to-reach groups — but synthetic answers are less variable than real ones, prompt- and model-dependent, can under-represent groups, and autonomous AI can evade detection. Post-collection: coding and labeling at scale, often dramatically cheaper — but smaller and older models trail human coders, and performance varies sharply by task. Every row says the same thing: promising, conditional on evaluation.

**24 · Divider: Capstone — 0:10**
Here's where I tried to practice what I'm preaching.

**25 · L2L: the problem — 0:50**
Surveys carry missing item values. The standard fixes — multiple imputation, tree-based methods — treat items as generic tabular features, ignoring that an instrument has measurement structure. LLMs are fluent but opaque, and can impose world knowledge that doesn't match the surveyed population. So the question my capstone asks: can we guide an LLM to impute missing items in a way that *respects* the instrument's measurement structure?

**26 · L2L: the approach — 1:00**
The pipeline: observed items go into a psychometric model — IRT, CFA, or latent class. Then the key move: don't feed the LLM factor loadings; *translate the model's conclusions into language* — a percentile, a confidence label, an adjustment direction, correlated-item context. The LLM then anchors on the model's prediction and adjusts, rather than guessing from scratch. Prompt tiers isolate the value of each layer of psychometric context: demographics and items alone, then latent scores, then the modal prediction with confidence.

**27 · L2L: design & guards — 0:50**
What the design guards against. Prompt tiers isolate whether psychometric context actually helps. Silicon sampling is constrained to real population segments. Evaluation is out-of-sample, on both per-case accuracy and distributional fidelity — watching for mode collapse, where imputations cluster on the modal answer. And one honest TSE point: translating model output into text is itself a processing step that can introduce error. I present this as a framework and an evaluation design, not an outperformance claim.

**28 · JPSM & the Michigan SRC — 0:50**
Where this training comes from: JPSM is run jointly by Maryland and Michigan with Westat, anchored by Michigan's Survey Research Center — home to long-running national studies and the Summer Institute. My training: an MS in Survey and Data Science alongside an MA in Psychology — design-based sampling, measurement and latent-variable modeling, multilevel and Bayesian methods, responsible ML and LLM integration. The throughline is the part I'd most like to import: reproducible, auditable pipelines in R and Quarto — everything in code, rendered from source.

**29 · Divider: Bringing it back — 0:10**
So what do we do with all this?

**30 · Two commitments — 1:00**
Two concrete commitments. One: monitor survey data quality continuously — treat quality as multi-dimensional, TSE plus fitness-for-use, not a response rate; track indicators and paradata over time; investigate departures; keep design-based inference honest. Two: experiment with survey-AI responsibly — pilot LLMs where they plausibly help, item drafting, coding, probing, imputation — each pilot with a fitness-for-use evaluation and a human in the loop; insist on transparency and disclosure; prefer enterprise deployments for sensitive data; and watch for the new error sources each tool introduces. The pair matters: monitoring tells us whether quality is holding; disciplined experimentation tells us whether a tool helps before we depend on it.

**31 · Continuous quality monitoring — 0:50**
What commitment one looks like — numbers here are illustrative; the discipline is the point. A quality indicator tracked weekly, a center line, control limits. In weeks eighteen and nineteen the indicator breaches the lower limit — that's a signal to investigate *during* fielding, not an end-of-field surprise. Borrowed straight from statistical process control, and it works for response indicators, coding agreement, item missingness — anything we can measure repeatedly.

**32 · Why this matters — and next steps — 0:50**
Our products ride on data we must be able to defend and reproduce. The same discipline that protects a public survey protects an analytic product: know the error structure, document the pipeline, disclose any tool use. For public opinion analysis, AI is a force multiplier inside a sound methodology — not a substitute for it. Three next steps: stand up lightweight quality monitoring; run small, well-evaluated survey-AI pilots; adopt a standard for disclosing AI use.

**33 · References — 0:15**
Selected references — note the distinction between Amaya, Biemer & Kinyon's 2020 Total Error Framework and Biemer's earlier row-column-cell framework; and the polling deep dives trace to Groves 2006, Curtin, Presser & Singer 2000, Little & Vartivarian 2005, and West, Sakshaug & Aurelien 2016.

**34 · Thank you — 0:30**
Two years compressed into thirty minutes: name where error enters, spend effort where it matters, and evaluate every new tool — including AI — through that same lens. Thank you; happy to take questions.

---
**Running total ≈ 29:30.** If you need to cut to ~20 minutes, drop slides 14, 16, and 19 first (coverage, weighting, levels-vs-trends) and compress 21–23 into two; the spine — 6–10, 15, 17–18, 25–27, 30–32 — carries the argument.
