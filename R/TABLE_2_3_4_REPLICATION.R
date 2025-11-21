# =====================================================================
# OpenMusE D1.2 — Tables 2, 2-by-country, 3, 4
# Eurostat sbs_sc_ovw  |  Semantic enrichment + sidecar metadata
# Outputs go to: ./OpenMusE_D1_2_OUTPUTS
# Creator: Arianna Martinelli 
# =====================================================================

# --- Setup -----------------------------------------------------------
suppressPackageStartupMessages({
  library(eurostat)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(jsonlite)
})

options(scipen = 999)  # nicer numeric printing

# Create a single output folder
OUTDIR <- "OpenMusE_D1_2_OUTPUTS"
if (!dir.exists(OUTDIR)) dir.create(OUTDIR, recursive = TRUE)

# --- Config ----------------------------------------------------------
cfg <- list(
  dataset        = "sbs_sc_ovw",
  target_geo     = "EU27_2020",
  preferred_year = 2022,
  prefer_later   = FALSE,   # ties: earlier year wins (earlier year chosen)
  keep_nace      = c("J592","C322","C182","G476","J601","J602","M731","R90",
                     "N799","J581","N823","R910"),
  keep_ind_all   = c("VAL_OUT_MEUR","NETTUR_MEUR","ENT_NR","EMP_NR","EMP_ENT_NR","SAL_NR"),
  keep_ind_3     = c("ENT_NR","NETTUR_MEUR","EMP_NR"),
  size_levels    = c("0_1","2-9","10-19","20-49","50-249","GE250","TOTAL"),
  drop_aggs      = c("EU28","EU27_2007","EU27","EA20","EA19","EFTA","EEA","EU27_2020")
)

# --- Group definitions ----------------------------------------------
OPENMUSE_CORE   <- c("J592","C322")
OPENMUSE_BROAD  <- c(OPENMUSE_CORE, "C182","G476","J601","J602","M731","R90")

CICERONE_CORE   <- c("R90","J592","C182","J601","G476","N799")
CICERONE_BROAD  <- c(CICERONE_CORE, "J602","M731","J581","N823","R910")

# --- Helpers (data) --------------------------------------------------
get_sbs <- function(dataset, keep_nace, keep_ind, size_levels=NULL) {
  flt <- list(nace_r2=keep_nace, indic_sbs=keep_ind)
  if (!is.null(size_levels)) flt$size_emp <- size_levels
  get_eurostat(dataset, filters=flt, cache=TRUE)
}

pick_closest_year <- function(df, prefer_year, prefer_later=FALSE,
                              group_vars=c("geo","size_emp","nace_r2","indic_sbs")) {
  df %>%
    mutate(year=as.integer(format(time,"%Y"))) %>%
    group_by(across(all_of(group_vars))) %>%
    arrange(
      is.na(values),
      abs(year - prefer_year),
      if (prefer_later) desc(year) else year,
      .by_group = TRUE
    ) %>%
    slice_head(n = 1) %>%
    ungroup()
}

order_size <- function(x, levels) factor(x, levels = levels[levels %in% unique(c(levels,x))], ordered = TRUE)
`%||%` <- function(a, b) if (is.null(a)) b else a

# Pretty/robust rounding by indicator
round_by_indicator <- function(indic, x) {
  is_money <- grepl("_MEUR$", indic %||% "")
  out <- ifelse(is_money, round(x, 2), round(x, 0))
  as.numeric(out)
}

# --- Download once ---------------------------------------------------
yf <- get_sbs(cfg$dataset, cfg$keep_nace, 
              cfg$keep_ind_all, cfg$size_levels)

# =====================================================================
# TABLE 3 – EU27_2020 TOTAL NACE-level (OpenMusE only)
# Columns: CORE_OPENMUSE | BROAD_OPENMUSE | nace_r2 | indic_sbs | values
# =====================================================================
yf3 <- yf %>%
  filter(
    geo == cfg$target_geo,
    size_emp == "TOTAL",
    indic_sbs %in% cfg$keep_ind_3,
    nace_r2 %in% OPENMUSE_BROAD
  )

yf3_sel <- pick_closest_year(
  yf3, cfg$preferred_year, cfg$prefer_later,
  group_vars = c("nace_r2","indic_sbs","geo","size_emp")
)

t3_by_nace <- yf3_sel %>%
  mutate(
    CORE_OPENMUSE  = as.integer(nace_r2 %in% OPENMUSE_CORE),
    BROAD_OPENMUSE = as.integer(nace_r2 %in% OPENMUSE_BROAD)
  ) %>%
  select(CORE_OPENMUSE, BROAD_OPENMUSE, nace_r2, indic_sbs, values) %>%
  mutate(values = round_by_indicator(indic_sbs, values)) %>%
  arrange(indic_sbs, desc(CORE_OPENMUSE), nace_r2) %>%
  rename ( core_openmuse = CORE_OPENMUSE, 
           broad_openmuse = BROAD_OPENMUSE)

write_csv(t3_by_nace, file.path(OUTDIR, "D1_2_TABLE_3_byNACE_EU27_TOTAL.csv"), na = "")

# =====================================================================
# TABLE 4 – CORE/BROAD by size class (sum of countries, drop EU aggregates)
# =====================================================================
yf4 <- yf %>%
  filter(
    !(geo %in% cfg$drop_aggs),
    size_emp %in% setdiff(cfg$size_levels, "TOTAL"),
    indic_sbs %in% cfg$keep_ind_3
  )

yf4_sel <- pick_closest_year(
  yf4, cfg$preferred_year, cfg$prefer_later,
  group_vars = c("geo", "size_emp", "nace_r2", "indic_sbs")
)

t4_size_nace <- yf4_sel %>%
  group_by(size_emp, nace_r2, indic_sbs) %>%
  summarise(value = sum(values, na.rm = TRUE), .groups = "drop")

t4 <- t4_size_nace %>%
  mutate(
    group = case_when(
      nace_r2 %in% OPENMUSE_CORE  ~ "CORE_OPENMUSE",
      nace_r2 %in% OPENMUSE_BROAD ~ "BROAD_OPENMUSE",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(group)) %>%
  group_by(size_emp, group, indic_sbs) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    size_emp = order_size(size_emp, cfg$size_levels),
    group    = factor(group, levels = c("CORE_OPENMUSE","BROAD_OPENMUSE"))
  ) %>%
  arrange(indic_sbs, group, size_emp) %>%
  mutate(value = round_by_indicator(indic_sbs, value))

write_csv(t4, file.path(OUTDIR, "D1_2_TABLE_4_bySize_COREvsBROAD_sumCountries.csv"), na = "")

# =====================================================================
# TABLE 2 – EU27_2020 TOTAL, FOUR GROUPS (all six variables)
# =====================================================================
yf2 <- yf %>%
  filter(geo == cfg$target_geo, size_emp == "TOTAL")

yf2_sel <- pick_closest_year(
  yf2, cfg$preferred_year, cfg$prefer_later,
  group_vars = c("nace_r2","indic_sbs","geo","size_emp")
)

sum_for <- function(df, codes, label) {
  df %>%
    filter(nace_r2 %in% codes) %>%
    group_by(indic_sbs) %>%
    summarise(value = sum(values, na.rm = TRUE), .groups = "drop") %>%
    mutate(group = label, .before = 1)
}

t2_long <- bind_rows(
  sum_for(yf2_sel, CICERONE_CORE,  "CORE_CICERONE"),
  sum_for(yf2_sel, CICERONE_BROAD, "BROAD_CICERONE"),
  sum_for(yf2_sel, OPENMUSE_CORE,  "CORE_OPENMUSE"),
  sum_for(yf2_sel, OPENMUSE_BROAD, "BROAD_OPENMUSE")
) %>%
  mutate(value = round_by_indicator(indic_sbs, value))

t2_wide <- t2_long %>%
  mutate(group = factor(group, levels = c(
    "CORE_CICERONE","BROAD_CICERONE","CORE_OPENMUSE","BROAD_OPENMUSE"
  ))) %>%
  pivot_wider(names_from = group, values_from = value) %>%
  arrange(indic_sbs)

write_csv(t2_wide, file.path(OUTDIR, "D1_2_TABLE_2_FOURGROUPS_WIDE_byGroup.csv"), na = "")

# =====================================================================
# TABLE 2 by country (drop aggregates), four groups
# =====================================================================
yf2_cty <- yf %>%
  filter(!(geo %in% cfg$drop_aggs), size_emp == "TOTAL")

yf2_cty_sel <- pick_closest_year(
  yf2_cty, cfg$preferred_year, cfg$prefer_later,
  group_vars = c("geo","nace_r2","indic_sbs","size_emp")
)

t2_cty_long <- bind_rows(
  yf2_cty_sel %>% filter(nace_r2 %in% CICERONE_CORE)  %>% group_by(geo, indic_sbs) %>% summarise(value=sum(values,na.rm=TRUE), .groups="drop") %>% mutate(group="CORE_CICERONE"),
  yf2_cty_sel %>% filter(nace_r2 %in% CICERONE_BROAD) %>% group_by(geo, indic_sbs) %>% summarise(value=sum(values,na.rm=TRUE), .groups="drop") %>% mutate(group="BROAD_CICERONE"),
  yf2_cty_sel %>% filter(nace_r2 %in% OPENMUSE_CORE)  %>% group_by(geo, indic_sbs) %>% summarise(value=sum(values,na.rm=TRUE), .groups="drop") %>% mutate(group="CORE_OPENMUSE"),
  yf2_cty_sel %>% filter(nace_r2 %in% OPENMUSE_BROAD) %>% group_by(geo, indic_sbs) %>% summarise(value=sum(values,na.rm=TRUE), .groups="drop") %>% mutate(group="BROAD_OPENMUSE")
) %>%
  mutate(value = round_by_indicator(indic_sbs, value))

t2_cty_wide <- t2_cty_long %>%
  mutate(group = factor(group, levels = c(
    "CORE_CICERONE","BROAD_CICERONE","CORE_OPENMUSE","BROAD_OPENMUSE"
  ))) %>%
  pivot_wider(names_from = group, values_from = value) %>%
  arrange(geo, indic_sbs)

write_csv(t2_cty_wide, file.path(OUTDIR, "D1_2_TABLE_2_FOURGROUPS_WIDE_byCountry.csv"), na = "")

# --- Progress --------------------------------------------------------
message("Saved outputs in ", OUTDIR, ":")
message("  D1_2_TABLE_2_FOURGROUPS_WIDE_byGroup.csv")
message("  D1_2_TABLE_2_FOURGROUPS_WIDE_byCountry.csv")
message("  D1_2_TABLE_3_byNACE_EU27_TOTAL.csv")
message("  D1_2_TABLE_4_bySize_COREvsBROAD_sumCountries.csv")

# =====================================================================
# Semantic packaging (labels, concepts, units, codes) + sidecar metadata
# =====================================================================

# ---- Simple helpers for semantics -----------------------------------
set_var_label <- function(x, lab) { attr(x, "label") <- lab; x }
as_coded <- function(vec, label, codes = NULL, concept = NULL, unit = NULL) {
  if (is.factor(vec) && !is.null(codes)) vec <- factor(as.character(vec), levels = codes)
  attr(vec, "label")   <- label
  if (!is.null(codes)) attr(vec, "codes")   <- codes
  if (!is.null(concept)) attr(vec, "concept") <- concept
  if (!is.null(unit))    attr(vec, "unit")    <- unit
  vec
}
dublincore <- function(title, creator, publisher, datasource, rights, coverage, funding=NULL) {
  list(
    title      = title,
    creator    = creator,   # list of persons (name/affiliation/orcid)
    publisher  = publisher,
    datasource = datasource,
    rights     = rights,
    coverage   = coverage,
    funding    = funding,
    created    = as.character(Sys.Date())
  )
}
mk_dataset <- function(df, meta) {
  attr(df, "dataset_metadata") <- meta
  df
}
write_jsonld_if_supported <- function(ds, file) {
  message("JSON-LD writer not available in this environment — skipping.")
}
write_sidecar_meta <- function(df, meta, file_csv) {
  vars <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    list(
      name        = nm,
      storageMode = typeof(x),
      class       = class(x)[1],
      label       = attr(x, "label") %||% "",
      concept     = attr(x, "concept") %||% "",
      unit        = attr(x, "unit") %||% "",
      codes       = attr(x, "codes") %||% if (is.factor(x)) levels(x) else NULL
    )
  })
  sidecar <- list(
    title        = meta$title %||% "",
    description  = meta$description %||% "",
    keywords     = meta$keywords %||% character(),
    license      = meta$license %||% "",
    creator      = meta$creator %||% list(),
    created      = meta$created %||% as.character(Sys.Date()),
    provenance   = meta$provenance %||% "",
    bibliography = meta$dataset_bibentry %||% list(),
    data = list(
      file = basename(file_csv),
      nrow = nrow(df),
      ncol = ncol(df),
      variables = vars
    )
  )
  json_path <- sub("\\.csv$", ".meta.json", file_csv)
  write_json(sidecar, json_path, auto_unbox = TRUE, pretty = TRUE)
  saveRDS(df, sub("\\.csv$", ".rds", file_csv))
  message("Wrote sidecar: ", json_path)
}

inspect_semantics <- function(obj, name = deparse(substitute(obj))) {
  cat("\n=== Semantic summary for:", name, "===\n")
  meta <- attr(obj, "dataset_metadata") %||% list()
  if (length(meta)) {
    cat("-- Dataset-level metadata --\n")
    for (k in names(meta)) {
      v <- meta[[k]]
      if (is.list(v) && !is.null(v$title) && !is.null(v$publisher)) {
        cat(sprintf("  %s: %s\n", k, v$title))
      } else if (is.character(v)) {
        cat(sprintf("  %s: %s\n", k, paste(v, collapse=", ")))
      } else if (is.list(v)) {
        cat(sprintf("  %s: [list]\n", k))
      }
    }
  }
  cat("\n-- Variable-level attributes --\n")
  for (col in names(obj)) {
    attrs <- attributes(obj[[col]])
    desc  <- attrs$label %||% "<no description>"
    codes <- if (!is.null(attrs$codes)) paste(attrs$codes, collapse=", ") else "<none>"
    concept <- attrs$concept %||% "<none>"
    unit <- attrs$unit %||% "<none>"
    cat(sprintf("  • %s (%s)\n    - description: %s\n    - codes: %s\n    - concept: %s\n    - unit: %s\n",
                col, paste(class(obj[[col]]), collapse="/"), desc, codes, concept, unit))
  }
  invisible(obj)
}

# ---- Shared code lists ----------------------------------------------
codes_indic_all <- c("VAL_OUT_MEUR","NETTUR_MEUR","ENT_NR","EMP_NR","EMP_ENT_NR","SAL_NR")
codes_ind3      <- c("ENT_NR","NETTUR_MEUR","EMP_NR")
codes_group_opm <- c("CORE_OPENMUSE","BROAD_OPENMUSE")
codes_size      <- c("0_1","2-9","10-19","20-49","50-249","GE250")

# =====================================================================
# Semantic enrichment & sidecars for each table
# =====================================================================

# --- Table 2 (EU27 TOTAL) --------------------------------------------
t2_sem <- t2_wide %>%
  mutate(
    indic_sbs = as_coded(
      indic_sbs,
      label   = "Structural Business Statistics (SBS) indicator",
      codes   = codes_indic_all,
      concept = "sbs:indicator",
      unit    = "varies by indicator"
    ),
    CORE_CICERONE = as_coded(
      CORE_CICERONE,
      label   = "Aggregated value for Cicerone core NACE activities",
      concept = "opm:cicerone_core_value",
      unit    = ""
    ),
    BROAD_CICERONE = as_coded(
      BROAD_CICERONE,
      label   = "Aggregated value for Cicerone broad NACE activities (includes core)",
      concept = "opm:cicerone_broad_value",
      unit    = ""
    ),
    CORE_OPENMUSE = as_coded(
      CORE_OPENMUSE,
      label   = "Aggregated value for OpenMusE core NACE activities",
      concept = "opm:openmuse_core_value",
      unit    = ""
    ),
    BROAD_OPENMUSE = as_coded(
      BROAD_OPENMUSE,
      label   = "Aggregated value for OpenMusE broad NACE activities (includes core)",
      concept = "opm:openmuse_broad_value",
      unit    = ""
    )
  )

meta_t2 <- list(
  title = "OpenMusE D1.2 – Table 2 (EU27_2020 TOTAL; four groups: Cicerone/OpenMusE, core/broad)",
  description = "Eurostat SBS summary by Cicerone and OpenMusE groups (Core/Broad). EU27_2020 TOTAL; closest year to 2022 (earlier in ties).",
  keywords = c("Eurostat","SBS","OpenMusE","Cicerone","CORE","BROAD","EU27","Table 2"),
  license  = "CC-BY-4.0",
  creator  = list(
    list(name="Arianna Martinelli", affiliation="Scuola Superiore Sant’Anna", orcid="0000-0002-5425-1038"),
    list(name="Olga Kavkaeva",      affiliation="Scuola Superiore Sant’Anna", orcid="0009-0000-9643-1689")
  ),
  provenance = "Derived from Eurostat sbs_sc_ovw; grouped by OpenMusE and Cicerone core/broad definitions; closest-year selection as per script.",
  dataset_bibentry = dublincore(
    title      = "Eurostat SBS-derived dataset – OpenMusE D1.2 Table 2",
    creator    = list(
      list(name="Arianna Martinelli", affiliation="Scuola Superiore Sant’Anna", orcid="0000-0002-5425-1038"),
      list(name="Olga Kavkaeva",      affiliation="Scuola Superiore Sant’Anna", orcid="0009-0000-9643-1689")
    ),
    publisher  = "Scuola Superiore Sant’Anna",
    datasource = "https://ec.europa.eu/eurostat/databrowser/view/sbs_sc_ovw",
    rights     = "CC-BY-4.0",
    coverage   = "EU27_2020 (Eurostat aggregate)",
    funding    = "European Union – Horizon Europe, OpenMusE (Grant No. 101132620)"
  )
)

ds_t2 <- mk_dataset(t2_sem, meta_t2)
write_sidecar_meta(ds_t2, meta_t2, file.path(OUTDIR, "D1_2_TABLE_2_FOURGROUPS_WIDE_byGroup.csv"))

# --- Table 2 by Country ----------------------------------------------
t2c_sem <- t2_cty_wide %>%
  mutate(
    geo = as_coded(geo, "Country or area (Eurostat GEO)", concept = "sbs:geo"),
    indic_sbs = as_coded(
      indic_sbs,
      label   = "Structural Business Statistics (SBS) indicator",
      codes   = codes_indic_all,
      concept = "sbs:indicator",
      unit    = "varies by indicator"
    ),
    CORE_CICERONE   = as_coded(CORE_CICERONE,  "Cicerone core value",  concept="opm:cicerone_core_value",  unit=""),
    BROAD_CICERONE  = as_coded(BROAD_CICERONE, "Cicerone broad value", concept="opm:cicerone_broad_value", unit=""),
    CORE_OPENMUSE   = as_coded(CORE_OPENMUSE,  "OpenMusE core value",  concept="opm:openmuse_core_value",  unit=""),
    BROAD_OPENMUSE  = as_coded(BROAD_OPENMUSE, "OpenMusE broad value", concept="opm:openmuse_broad_value", unit="")
  )

meta_t2c <- list(
  title = "OpenMusE D1.2 – Table 2 by Country (four groups: Cicerone/OpenMusE, core/broad)",
  description = "Eurostat SBS by country; four group definitions (Cicerone/OpenMusE; Core/Broad). Closest year to 2022 (earlier in ties).",
  keywords = c("Eurostat","SBS","OpenMusE","Cicerone","By country","EU27","Table 2"),
  license  = "CC-BY-4.0",
  creator  = list(
    list(name="Arianna Martinelli", affiliation="Scuola Superiore Sant’Anna", orcid="0000-0002-5425-1038"),
    list(name="Olga Kavkaeva",      affiliation="Scuola Superiore Sant’Anna", orcid="0009-0000-9643-1689")
  ),
  provenance = "Derived from Eurostat sbs_sc_ovw; per-country aggregates by defined groups.",
  dataset_bibentry = dublincore(
    title      = "Eurostat SBS-derived dataset – OpenMusE D1.2 Table 2 by Country",
    creator    = list(
      list(name="Arianna Martinelli", affiliation="Scuola Superiore Sant’Anna", orcid="0000-0002-5425-1038"),
      list(name="Olga Kavkaeva",      affiliation="Scuola Superiore Sant’Anna", orcid="0009-0000-9643-1689")
    ),
    publisher  = "Scuola Superiore Sant’Anna",
    datasource = "https://ec.europa.eu/eurostat/databrowser/view/sbs_sc_ovw",
    rights     = "CC-BY-4.0",
    coverage   = "EU Member States (latest close to 2022)",
    funding    = "European Union – Horizon Europe, OpenMusE (Grant No. 101132620)"
  )
)

ds_t2c <- mk_dataset(t2c_sem, meta_t2c)
write_sidecar_meta(ds_t2c, meta_t2c, file.path(OUTDIR, "D1_2_TABLE_2_FOURGROUPS_WIDE_byCountry.csv"))

# --- Table 3 (EU27 TOTAL by NACE with flags) -------------------------
t3_sem <- t3_by_nace %>%
  rename(value = values) %>%
  mutate(
    CORE_OPENMUSE  = as_coded(CORE_OPENMUSE,  "Core OpenMusE flag (1/0)", concept="opm:openmuse_core_flag",  unit="binary"),
    BROAD_OPENMUSE = as_coded(BROAD_OPENMUSE, "Broad OpenMusE flag (1/0)", concept="opm:openmuse_broad_flag", unit="binary"),
    nace_r2        = as_coded(nace_r2, "NACE Rev.2 activity code", concept="nace:r2"),
    indic_sbs      = as_coded(indic_sbs, "SBS indicator", codes = codes_ind3, concept="sbs:indicator"),
    value          = as_coded(round_by_indicator(indic_sbs, value), "Value", unit="Eurostat SBS units (as per indic_sbs)")
  )

meta_t3 <- list(
  title = "OpenMusE D1.2 – Table 3 (EU27_2020 TOTAL by NACE, with CORE/BROAD flags)",
  description = "Eurostat SBS at NACE level; flags indicate OpenMusE core/broad membership; EU27 TOTAL; closest year to 2022.",
  keywords = c("Eurostat","SBS","OpenMusE","NACE","CORE","BROAD","EU27","Table 3"),
  license  = "CC-BY-4.0",
  creator  = list(
    list(name="Arianna Martinelli", affiliation="Scuola Superiore Sant’Anna", orcid="0000-0002-5425-1038"),
    list(name="Olga Kavkaeva",      affiliation="Scuola Superiore Sant’Anna", orcid="0009-0000-9643-1689")
  ),
  provenance = "Derived from Eurostat sbs_sc_ovw; NACE-level values; flags per OpenMusE mapping.",
  dataset_bibentry = dublincore(
    title      = "Eurostat SBS-derived dataset – OpenMusE D1.2 Table 3",
    creator    = list(
      list(name="Arianna Martinelli", affiliation="Scuola Superiore Sant’Anna", orcid="0000-0002-5425-1038"),
      list(name="Olga Kavkaeva",      affiliation="Scuola Superiore Sant’Anna", orcid="0009-0000-9643-1689")
    ),
    publisher  = "Scuola Superiore Sant’Anna",
    datasource = "https://ec.europa.eu/eurostat/databrowser/view/sbs_sc_ovw",
    rights     = "CC-BY-4.0",
    coverage   = "EU27_2020 (Eurostat aggregate)",
    funding    = "European Union – Horizon Europe, OpenMusE (Grant No. 101132620)"
  )
)

ds_t3 <- mk_dataset(t3_sem, meta_t3)
write_sidecar_meta(ds_t3, meta_t3, file.path(OUTDIR, "D1_2_TABLE_3_byNACE_EU27_TOTAL.csv"))

# --- Table 4 (by size class; sum of countries) -----------------------
t4_sem <- t4 %>%
  mutate(
    size_emp  = as_coded(size_emp, "Enterprise size class", codes = codes_size, concept = "sbs:size"),
    group     = as_coded(group, "Group (OpenMusE)", codes = codes_group_opm, concept = "opm:group"),
    indic_sbs = as_coded(indic_sbs, "SBS indicator", codes = codes_ind3, concept = "sbs:indicator"),
    value     = as_coded(round_by_indicator(indic_sbs, value), "Value", unit="Eurostat SBS units (as per indic_sbs)")
  )

meta_t4 <- list(
  title = "OpenMusE D1.2 – Table 4 (by size class; sum of countries)",
  description = "Eurostat SBS aggregated across countries; by enterprise size and OpenMusE group; closest year to 2022 (earlier in ties).",
  keywords = c("Eurostat","SBS","OpenMusE","Size class","EU27","Table 4"),
  license  = "CC-BY-4.0",
  creator  = list(
    list(name="Arianna Martinelli", affiliation="Scuola Superiore Sant’Anna", orcid="0000-0002-5425-1038"),
    list(name="Olga Kavkaeva",      affiliation="Scuola Superiore Sant’Anna", orcid="0009-0000-9643-1689")
  ),
  provenance = "Derived from Eurostat sbs_sc_ovw; country-summed; grouped by size class and OpenMusE group.",
  dataset_bibentry = dublincore(
    title      = "Eurostat SBS-derived dataset – OpenMusE D1.2 Table 4",
    creator    = list(
      list(name="Arianna Martinelli", affiliation="Scuola Superiore Sant’Anna", orcid="0000-0002-5425-1038"),
      list(name="Olga Kavkaeva",      affiliation="Scuola Superiore Sant’Anna", orcid="0009-0000-9643-1689")
    ),
    publisher  = "Scuola Superiore Sant’Anna",
    datasource = "https://ec.europa.eu/eurostat/databrowser/view/sbs_sc_ovw",
    rights     = "CC-BY-4.0",
    coverage   = "EU27_2020 (aggregated from Member States)",
    funding    = "European Union – Horizon Europe, OpenMusE (Grant No. 101132620)"
  )
)

ds_t4 <- mk_dataset(t4_sem, meta_t4)
write_sidecar_meta(ds_t4, meta_t4, file.path(OUTDIR, "D1_2_TABLE_4_bySize_COREvsBROAD_sumCountries.csv"))

# --- Final inspection (prints a concise summary in console) ----------
inspect_semantics(ds_t2,  "Table 2 (EU27 TOTAL)")
inspect_semantics(ds_t2c, "Table 2 (by Country)")
inspect_semantics(ds_t3,  "Table 3 (by NACE)")
inspect_semantics(ds_t4,  "Table 4 (by size class)")

message("All done — CSV, .meta.json, and .rds written to: ", OUTDIR)

# =====================================================================
# Create a single README.md for the repository (after tables exist)
# =====================================================================

# -- 1) Find an output directory --------------------------------------
if (exists("OUTDIR") && dir.exists(OUTDIR)) {
  readme_dir <- OUTDIR
} else {
  # Try to infer from existing files
  guess_files <- c(
    "D1_2_TABLE_2_FOURGROUPS_WIDE_byGroup.csv",
    "D1_2_TABLE_2_FOURGROUPS_WIDE_byCountry.csv",
    "D1_2_TABLE_3_byNACE_EU27_TOTAL.csv",
    "D1_2_TABLE_4_bySize_COREvsBROAD_sumCountries.csv"
  )
  existing_paths <- guess_files[file.exists(guess_files)]
  readme_dir <- if (length(existing_paths)) dirname(existing_paths[1]) else getwd()
}

# -- 2) Helpers --------------------------------------------------------
md <- function(...) paste0(..., collapse = "")
nz <- function(x) !is.null(x) && length(x) > 0 && !(is.character(x) && all(x == ""))

bullet_list <- function(vec) {
  if (!nz(vec)) return("")
  paste0(paste0("- ", vec), collapse = "\n")
}

fmt_creator <- function(c) {
  # c is a list with name, affiliation, orcid (optional)
  nm  <- c$name  %||% ""
  aff <- c$affiliation %||% ""
  orc <- c$orcid %||% ""
  bits <- c(
    if (nz(nm)) nm else NULL,
    if (nz(aff)) paste0("(", aff, ")") else NULL,
    if (nz(orc)) paste0("[ORCID:", orc, "]") else NULL
  )
  paste(bits, collapse = " ")
}

collect_creators <- function(meta) {
  # Prefer Dublin Core creators if present, else top-level creator
  dc_creators <- meta$dataset_bibentry$creator %||% NULL
  top_creators <- meta$creator %||% NULL
  creators <- dc_creators %||% top_creators %||% list()
  # Coerce to list-of-lists if needed
  if (is.list(creators) && !is.null(creators$name)) creators <- list(creators)
  unlist(lapply(creators, fmt_creator))
}

collect_funding <- function(meta) {
  meta$dataset_bibentry$funding %||% ""
}

files_block <- function(dir, base_csv) {
  csv <- file.path(dir, base_csv)
  jld <- sub("\\.csv$", ".meta.json", csv)
  rds <- sub("\\.csv$", ".rds", csv)
  items <- c()
  if (file.exists(csv)) items <- c(items, paste0("CSV: `", basename(csv), "`"))
  if (file.exists(jld)) items <- c(items, paste0("Sidecar JSON: `", basename(jld), "`"))
  if (file.exists(rds)) items <- c(items, paste0("RDS (with attributes): `", basename(rds), "`"))
  bullet_list(items)
}

# -- 3) Gather metas if available -------------------------------------
# (Your script created meta_t2, meta_t2c, meta_t3, meta_t4 above.)
meta_safe <- function(m) if (exists(m)) get(m) else list()
M2   <- meta_safe("meta_t2")
M2C  <- meta_safe("meta_t2c")
M3   <- meta_safe("meta_t3")
M4   <- meta_safe("meta_t4")

# -- 4) Compose README text -------------------------------------------
title_line <- "# OpenMusE D1.2 – Eurostat SBS Derived Tables\n\n"
about <- paste0(
  "This repository contains derived datasets produced from Eurostat **SBS** (`sbs_sc_ovw`) for ",
  "**OpenMusE D1.2**. The outputs include CSV files, sidecar **`.meta.json`** files describing ",
  "variable-level metadata (labels, concepts, units, codes) and dataset-level metadata ",
  "(creators with ORCIDs, funding, provenance), plus **`.rds`** files preserving metadata ",
  "attributes in R.\n\n"
)

# Creators / Funding — take from Table 2 metadata as canonical, fallback to others
creators_lines <- character()
funding_line <- ""

for (MM in list(M2, M2C, M3, M4)) {
  if (length(creators_lines) == 0) creators_lines <- collect_creators(MM)
  if (!nz(funding_line)) funding_line <- collect_funding(MM)
}
if (!nz(funding_line)) {
  funding_line <- "European Union – Horizon Europe, OpenMusE (Grant No. 101132620)"
}

creators_block <- if (length(creators_lines)) {
  paste0("**Creators:**\n\n", bullet_list(creators_lines), "\n\n")
} else {
  ""
}

funding_block <- paste0(
  "**Funding acknowledgement:** ", funding_line, "\n\n"
)

# Dataset sections
sec <- function(meta, header, base_csv) {
  if (!length(meta)) {
    # If no metadata list is found, still include a minimal section if the file exists
    fb <- files_block(readme_dir, base_csv)
    if (!nz(fb)) return("")
    return(paste0("## ", header, "\n\n", fb, "\n\n"))
  }
  t <- meta$title        %||% header
  d <- meta$description  %||% ""
  k <- if (nz(meta$keywords)) paste(meta$keywords, collapse = ", ") else ""
  dc <- meta$dataset_bibentry %||% list()
  src <- dc$datasource %||% ""
  cov <- dc$coverage   %||% ""
  fb  <- files_block(readme_dir, base_csv)
  
  paste0(
    "## ", t, "\n\n",
    if (nz(d)) paste0(d, "\n\n") else "",
    if (nz(src)) paste0("- **Source:** ", src, "\n") else "",
    if (nz(cov)) paste0("- **Coverage:** ", cov, "\n") else "",
    if (nz(k))   paste0("- **Keywords:** ", k, "\n") else "",
    if (nz(fb))  paste0("\n**Files:**\n\n", fb, "\n") else "",
    "\n"
  )
}

body <- paste0(
  sec(M2,  "Table 2 (EU27 TOTAL; four groups)", "D1_2_TABLE_2_FOURGROUPS_WIDE_byGroup.csv"),
  sec(M2C, "Table 2 by Country (four groups)",  "D1_2_TABLE_2_FOURGROUPS_WIDE_byCountry.csv"),
  sec(M3,  "Table 3 (EU27 TOTAL by NACE)",      "D1_2_TABLE_3_byNACE_EU27_TOTAL.csv"),
  sec(M4,  "Table 4 (by size class; sum of countries)", "D1_2_TABLE_4_bySize_COREvsBROAD_sumCountries.csv")
)


`%+%` <- function(a, b) paste0(a, b)

usage <- paste0(
  "### How to use\n\n",
  "- **CSV**: Load into your tool of choice. Values are rounded consistently by indicator type.\n",
  "- **Sidecar JSON**: Machine-readable metadata (variable labels, concepts, units, codes; dataset provenance, creators, funding).\n",
  "- **RDS**: Load via `readRDS()` to preserve R attributes for semantic inspection.\n\n",
  "```r\n",
  "# Example in R\n",
  "df <- read.csv(file.path('", basename(readme_dir), "', 'D1_2_TABLE_2_FOURGROUPS_WIDE_byGroup.csv'))\n",
  "meta <- jsonlite::fromJSON(file.path('", basename(readme_dir), "', 'D1_2_TABLE_2_FOURGROUPS_WIDE_byGroup.meta.json'))\n",
  "str(meta$data$variables)  # variable-level metadata\n",
  "```",
  "\n\n"
)

provenance <- paste0(
  "### Provenance\n\n",
  "Derived from Eurostat `sbs_sc_ovw` via scripted extraction and aggregation. ",
  "Year per series chosen as closest to 2022 (earlier in ties). Group definitions include ",
  "**OpenMusE** (core/broad) and **Cicerone** (core/broad). See metadata sidecars for details.\n\n"
)

readme_text <- paste0(
  title_line,
  about,
  creators_block,
  funding_block,
  body,
  usage,
  provenance
)

# -- 5) Write README.md ------------------------------------------------
readme_path <- file.path(readme_dir, "README.md")
writeLines(readme_text, readme_path, useBytes = TRUE)
message("README.md written to: ", readme_path)
