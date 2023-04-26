# Gender-based Violence Health Provider Study
This is a repository for Susan Glenn's MPH thesis work. 

## Description of research

## Running this analysis

### API access

In order to access the data as a part of this analysis, you will need a REDCap API key. This key will be stored as an git ignored file in this repository. 

## Code Linting

Linting refers to the process of analyzing source code to detect potential errors, coding style inconsistencies, and other issues that may lead to bugs or make the code hard to read and maintain. Linting is usually performed by a tool called a linter or a static code analyzer, which checks the code against a set of predefined rules or guidelines.

In this repo, we're using Styler, which is an R package that provides a consistent and flexible way to automatically format R code according to the tidyverse style guide.

To run `stylr`: 

- source `dependencies.R`
- `style_dir("~/Repositories/gbv-health-provider-study/")`