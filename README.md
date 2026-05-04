# stat4RDBFIS

`stat4RDBFIS` is an R package developed in the RDBFIS III project
(Activity 10) for checking, preparing and analysing RCG landings (CL) and
sampling (CS) data from Mediterranean and Black Sea fisheries.

The package is maintained by the stat4RDBFIS Core Team within the RDBFIS
consortium.

## Scope

The package includes functions for:

- validating RCG landings templates (CL);
- validating RCG sampling templates (CS);
- cross-checking cleaned landings and sampling data before raising;
- preparing and raising landings LFDs;
- preparing and raising discard LFDs;
- applying generic and country-specific raising methods;
- estimating age-length keys (ALKs) and numbers-at-age;
- producing maturity and sex-ratio outputs from sampling data;
- estimating discard ratios from cleaned sampling data;
- estimating discard LFDs.
  
## Installation

Install from GitHub:

```r
install.packages("remotes")
remotes::install_github("medbsrdb/stat4RDBFIS")
```

Or install from a local source folder:

```r
install.packages("devtools")
devtools::install_local("path/to/stat4RDBFIS")
```

If the repository is later transferred to the RDBFIS organization, update the
GitHub slug accordingly.

## Main Workflow

The tutorial workflow is organised in the following sequence:

1. `landings_qc()`
2. `samplings_qc()`
3. `crosschecks()`
4. `landings_rdbfis()`
5. `ma_rdbfis()` and `ml_rdbfis()`
6. `sra_rdbfis()` and `srl_rdbfis()`
7. `alk_rdbfis()`
8. `discard_ratio_rdbfis()`
9. `discards_rdbfis()`

## Raising Methods

The LFD raising step can use:

- `trip_h()`
- `trip_cc()`
- `vigneau_mahevas()`
- `method_cyp()`
- `method_esp()`
- `method_fra()`
- `method_grc()`
- `method_ita2()`
- `method_ita3()`

## Example Workflow

```r
library(stat4RDBFIS)

# Load your own RCG landings and sampling templates
landings_df <- read_csv(
  "path/to/RCG_landings_template.csv",
  show_col_types = FALSE
)

sampling_df <- read_csv(
  "path/to/RCG_sampling_template.csv",
  show_col_types = FALSE
)

qc_land <- landings_qc(landings_df = landings_df)
qc_samp <- samplings_qc(sampling_df = sampling_df)

qc_cross <- crosschecks(
  sampling_df = qc_samp$sampling_df,
  landings_df = qc_land$landings_df
)

landings_df <- qc_cross$landings_clean
sampling_df <- qc_cross$sampling_clean

raised_lfd <- landings_rdbfis(
  sampling_df = qc_cross$sampling_clean,
  landings_df = qc_cross$landings_clean
)

maturity_age <- ma_rdbfis(
  sampling_df = sampling_df
)

maturity_length <- ml_rdbfis(
  sampling_df = sampling_df
)

sex_ratio_age <- sra_rdbfis(
  sampling_df = sampling_df
)

sex_ratio_length <- srl_rdbfis(
  sampling_df = sampling_df
)

alk <- alk_rdbfis(
  sampling_df = sampling_df
)

discard_ratio <- discard_ratio_rdbfis(
  sampling_df = sampling_df
)

discard_lfd <- discards_rdbfis(
  sampling_df = sampling_df,
  landings_df = landings_df
)
```

This example follows the same order used in the tutorial script. After the
three QC steps, the cleaned landings and sampling datasets are reused in the
subsequent analyses.

## Bundled Reference Files

The package includes internal reference files in `inst/extdata`:

- `validation_lists.json`
- `species_length.json`
- `LW-relationship.csv`

## Core Team

- Vicky Sgardeli, HCMR, `vsgard@hcmr.gr`
- Ilaria Costantini, CNR-IRBIM, `ilaria.costantini@cnr.it`
- Kostas Touloumis, FRI, `touloumisk@inale.gr`
- Alessandro Mannini, CNR-IRBIM, `alessandro.mannini@cnr.it`
- Stefanos Kavadas, HCMR, `stefanos@hcmr.gr`
