# ShiMyP Project Agents.md Guide for OpenAI Codex

This Agents.md file provides essential guidance for OpenAI Codex and other AI agents contributing to the ShiNyP codebase—an R/Shiny-based SNP analysis platform.

## ShiNyP Project Structure Overview

-   `/R`: Core R scripts and functions (including analysis modules, data processing, Shiny UI/server components)
-   `/inst`: Static resources (e.g., demo datasets and icon)
-   `/man`: Function documentation generated via Roxygen2
-   `/tools/Case_Studies_Pipelines`: Case study workflows and analysis pipelines
-   `DESCRIPTION`: Package metadata and dependency declarations
-   `Dockerfile`: Container configuration for deployment
-   `README.md`: Project introduction, installation, and usage instructions

## Coding Conventions for Agent

### General Guidelines

-   **Language**: All primary source code must be written in R. Shiny modules must follow R/Shiny syntax conventions.

-   **Naming**: Use descriptive, snake_case function names; object and variable names must be self-explanatory.

-   **Comments & Documentation**: Complex logic should be clearly explained using `#` comments. All exported functions must use Roxygen2-style documentation.

-   **Code Style**: Follow tidyverse community standards, including 2-space indentation and minimal trailing whitespace.

### Shiny Application Guidelines

-   Each analytical core function should write a core function script under `R/Functions.R`.

-   Use `R/Global.R` to store globally reactive objects, variable initializations, and frequently accessed variable names.

-   Store all fixed parameters, layout options, method choices, and prompt templates within `R/constants.R`.

-   All custom front-end resources (CSS/HTML) should be placed under `R/CSS.R` or `AI_CSS_HTML_Bot.R`

-   Use commented lines of `-` and `=` to break up the file into easily readable chunks.

## Code Checks

All checks must pass before submitting new code.

``` r
devtools::load_all(".")
devtools::test()
devtools::build()
```