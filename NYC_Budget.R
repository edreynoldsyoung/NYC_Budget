# NYC budget per capita (real dollars) — FY2003–FY2022
# Data sources:
# - IBO AgencyExpenditures.xlsx (Total Citywide Expenditures, in $000s)
# - FRED CPIAUCSL (monthly CPI-U)
# - Census ACS 1-year API (NYC population, B01003_001E)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(stringr)
  library(purrr)
  library(ggplot2)
  library(httr2)
  library(janitor)
})

# ----------------------------
# 1) NYC Budget table (IBO)
# ----------------------------
ibo_xlsx_url <- "https://www.ibo.nyc.ny.us/RevenueSpending/AgencyExpenditures.xlsx"
ibo_tmp <- tempfile(fileext = ".xlsx")
download.file(ibo_xlsx_url, ibo_tmp, mode = "wb")

# The totals are on the sheet named "In $000's"
raw <- readxl::read_excel(ibo_tmp, sheet = "In $000's", col_names = TRUE)

# Row 3 (1-indexed) contains the year headers in this workbook layout; we detect years robustly
# Find the header row by searching for a row that contains many 4-digit years
is_yearish <- function(x) suppressWarnings(!is.na(as.numeric(x)) & as.numeric(x) >= 1980 & as.numeric(x) <= 2100)

year_row_idx <- which(apply(raw, 1, function(r) sum(is_yearish(r), na.rm = TRUE) >= 10))[1]
year_row <- raw[year_row_idx, ]

# Build a mapping: column name -> fiscal year
col_year_map <- tibble(
  col = names(raw),
  fy  = suppressWarnings(as.integer(unlist(year_row[1, ])))
) %>%
  filter(!is.na(fy))

# Find the row for Total Citywide Expenditures
total_row_idx <- which(str_detect(tolower(raw[[1]]), "^total citywide expenditures$"))[1]

budget_wide <- raw[total_row_idx, ] %>%
  select(all_of(col_year_map$col)) %>%
  pivot_longer(cols = everything(), names_to = "col", values_to = "exp_k") %>%
  left_join(col_year_map, by = "col") %>%
  transmute(
    fiscal_year = fy,
    # values are "in $000s" -> multiply by 1,000 to get nominal dollars
    exp_nominal = as.numeric(exp_k) * 1000
  ) %>%
  filter(!is.na(fiscal_year), !is.na(exp_nominal))

# Keep the most recent 20 fiscal years available in the IBO file
budget <- budget_wide %>%
  arrange(fiscal_year) %>%
  slice_tail(n = 20)

# ----------------------------
# 2) CPI table (FRED CPIAUCSL) -> fiscal-year averages (July–June)
# ----------------------------
fred_cpi_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCSL"
cpi_raw <- readr::read_csv(fred_cpi_url, show_col_types = FALSE) %>%
  rename_with(tolower) %>%
  rename(cpi = cpiaucsl) %>%
  mutate(date = as.Date(.data$date))

# Convert each month to NYC fiscal year (FY runs Jul 1 – Jun 30; e.g., Aug 2021 is FY2022)
cpi_fy <- cpi_raw %>%
  mutate(
    fiscal_year = if_else(as.integer(format(date, "%m")) >= 7,
                          as.integer(format(date, "%Y")) + 1L,
                          as.integer(format(date, "%Y")))
  ) %>%
  group_by(fiscal_year) %>%
  summarize(cpi_fy_avg = mean(cpi, na.rm = TRUE), .groups = "drop")

# Choose base year for "real dollars" (you can change this)
base_fy <- max(cpi_fy$fiscal_year, na.rm = TRUE)
base_cpi <- cpi_fy %>% filter(fiscal_year == base_fy) %>% pull(cpi_fy_avg)

# ----------------------------
# 3) NYC population table (ACS 1-year): B01003_001E for place:51000 in state:36
# ----------------------------
# ACS1 begins in 2005; if your budget years go earlier than 2005, we’ll drop them later.

fetch_acs_pop <- function(year) {
  # Example endpoint pattern:
  # https://api.census.gov/data/2022/acs/acs1?get=NAME,B01003_001E&for=place:51000&in=state:36
  req <- request(paste0("https://api.census.gov/data/", year, "/acs/acs1")) |>
    req_url_query(
      get = "NAME,B01003_001E",
      `for` = "place:51000",
      `in` = "state:36"
    )
  
  resp <- req_perform(req)
  dat <- resp_body_json(resp, simplifyVector = TRUE)
  
  # First row is headers; second row is data
  tibble(
    year = year,
    population = as.numeric(dat[2, "B01003_001E"])
  )
}

years_needed <- sort(unique(budget$fiscal_year))
# Approximate population for FY t using ACS year (t-1) or (t); here we use calendar year = fiscal year
# You can choose fiscal_year-1 instead if you prefer a "mid-FY" approximation.
acs_years <- years_needed
acs_years <- acs_years[acs_years >= 2005]  # ACS1 availability constraint

pop <- map_dfr(acs_years, fetch_acs_pop) %>%
  rename(fiscal_year = year)

# ----------------------------
# 4) Combine -> real per capita -> plot
# ----------------------------
combined <- budget %>%
  left_join(cpi_fy, by = "fiscal_year") %>%
  left_join(pop, by = "fiscal_year") %>%
  filter(!is.na(cpi_fy_avg), !is.na(population)) %>%
  mutate(
    exp_real = exp_nominal * (base_cpi / cpi_fy_avg),     # convert to base_fy dollars
    exp_real_per_capita = exp_real / population
  )

# View the assembled table (this is your “data table” output)
print(combined %>% select(fiscal_year, exp_nominal, cpi_fy_avg, population, exp_real_per_capita))

# Plot
ggplot(combined, aes(x = fiscal_year, y = exp_real_per_capita)) +
  geom_line() +
  geom_point() +
  labs(
    title = "NYC Total Citywide Expenditures per Capita (Inflation-Adjusted)",
    subtitle = paste0("Expenditures from IBO; population from ACS 1-year; dollars in FY", base_fy, " terms"),
    x = "NYC Fiscal Year",
    y = paste0("Real dollars per capita (FY", base_fy, " $)")
  ) +
  theme_minimal()
