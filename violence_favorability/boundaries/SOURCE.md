# Boundary source

**In use:** OCHA administrative boundaries for Mali (COD-AB), levels 0 to 3.

- Producer: Direction Nationale des Collectivités Territoriales (DNCT), Mali
- Publisher: United Nations Office for the Coordination of Humanitarian Affairs (OCHA), Humanitarian Data Exchange (HDX)
- Files: `mli_admbnda_adm0..3_1m_gov_20211110b` (release of 10 November 2021), folder `mli_adm_1m_dnct_2021_shp`
- Downloaded: 1 October 2026
- Contents: 10 regions (including Ménaka), 53 cercles, 701 communes, with official P-codes (for example `ML090101`) and the official commune > cercle > region hierarchy

Suggested citation:

> OCHA. *Mali: Subnational administrative boundaries (COD-AB), levels 0 to 3.* Source: Direction Nationale des Collectivités Territoriales (DNCT), 2021. Humanitarian Data Exchange. Accessed 1 October 2026.

Record the exact HDX page you downloaded from, and check the license stated there before publishing.

**Notes**
- OCHA's 2025 Mali release (Institut Géographique du Mali) covers levels 0 to 2 only (no communes), so the 2021 DNCT release is the current OCHA commune layer.
- Compared with the 2017 boundaries used earlier, the 2021 file has the same 701 communes; differences are official spelling updates (for example "Kayes Commune" became "Kayes", "Dougoutene 1" became "Dougoutene I").
- The survey uses 8 regions, which predate Ménaka's separation from Gao. For name matching only, Ménaka's communes are searched under Gao (`region_alias` in `R/boundaries.R`); maps show OCHA's region names.
