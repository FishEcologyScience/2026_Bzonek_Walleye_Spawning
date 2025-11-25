# WalleyeReport Project Instructions

## Session Startup Protocol
At the beginning of each session, Claude should:
1. Automatically read through the repository structure and key scripts
2. Announce: "I have read the repository"
3. Provide a 1-sentence description of what the repository is about
4. Provide a 1-sentence summary of the most recent changes

This should happen automatically without Paul needing to request it.

## Automatic Project Familiarization
**IMPORTANT**: At the start of each session, Claude should automatically familiarize itself with this walleye research repository by:

1. **Reading key scripts** in `02 - Scripts/` to understand the analysis workflow
2. **Reviewing the user interface script** (`Script0-0_UserInterface.R`) to understand dependencies
3. **Identifying project strengths** such as:
   - Effective data integration approaches
   - Well-implemented statistical methods  
   - Strong analytical workflow organization
   - Sophisticated behavioral consistency analyses

4. **Identifying project weaknesses** such as:
   - Commented-out analyses that need completion
   - Missing methodological components
   - Workflow inefficiencies or gaps
   - Documentation or reproducibility issues

5. **Providing actionable next steps** prioritized by impact and feasibility

This familiarization should happen proactively without Paul needing to request it.

## Project Context
This is a walleye spawning behavior research project using acoustic telemetry, electrofishing, and habitat data from Hamilton Harbour (2015-2023). The focus is on site fidelity, behavioral consistency, and habitat preferences.

## Summary Statistics Style Guide
When creating or restructuring summary statistics sections:

### Structure and Formatting
- **Use numbered subsections** (1., 2., 3., etc.) with descriptive titles
- **Use `---` for section headers** (e.g., `cat("\n--- SECTION TITLE ---\n")`)
- **Always report SD with means** using format: `mean = X, SD = Y`
- **Use consistent formatting**:
  - Big mark for large numbers: `format(nrow(data), big.mark = ",")`
  - Date formatting: `format(date, "%Y-%m-%d")`
  - Round appropriately: 2 decimals for most metrics, 1 for percentages/minutes
- **Group related metrics** under subheadings with 2-space indentation
- **Include range values** where meaningful (min - max)

### Naming Convention
- **Use `temp_summary_` prefix** for all objects created for summary statistics
  - Examples: `temp_summary_fish`, `temp_summary_timing`, `temp_summary_events_by_year`
  - This distinguishes them from general `temp_` objects used for data processing
- **Name descriptively**: Object name should indicate what it summarizes
- **Clean up at section end**: Remove all `temp_summary_*` objects after use

### Section Organization
Order sections logically:
1. Sample characteristics (fish, study design)
2. Data processing summary (detections, filtering steps)
3. Movement/behavioral metrics (residency, events)
4. Spatial/environmental patterns (depth, stations)
5. Temporal patterns (years, weeks, timing)
6. Contextual patterns (diel, environmental)