# ABS Transactions Analysis — Fully Annotated
**Author:** Arash Nateghiyan  
**Purpose:** Post-cleaning analytics, sales performance, and market basket analysis on ABS transaction data.

> This document expands your working script with rich commentary on **what** each block does and **why** it matters.

---
## 1) Load Libraries & Dataset

```r
# Core data and wrangling
library(dplyr)       # verbs: filter, select, group_by, summarise, mutate
library(tidyverse)   # brings ggplot2, readr, tidyr, etc.
library(stringr)     # robust string/regex helpers
library(lubridate)   # date parsing, wday/month/year helpers
library(ggplot2)     # plots
library(purrr)       # functional map/reduce style helpers
library(arules)      # market basket analysis (Apriori)
```

```r
transactions <- read.csv(
  "TransactionData.csv",
  header = TRUE,
  sep = ",",
  encoding = "UTF-8",
  quote = "\""
)
```
---
## 2)  Filter out unwanted rows and handle missing/invalid data

```r
transactions <- transactions |>
  filter(
    STORENAME != "Westwood",
    CLASSIFICATIONDEPARTMENT != "DONATIONS",
    CLASSIFICATIONDEPARTMENT != "NON INVENTORY",
    SIZE != "UNIT",
    (SIZE != "" & !is.na(SIZE)),
    (TAGDESC != "NULL" & !is.na(TAGDESC)),
    (TAGDESC == "STOCK")
  )
```
---
## 3) Clean and normalize text columns

```r
transactions <- transactions |>
  mutate(
    CLASSIFICATIONDEPARTMENT = trimws(CLASSIFICATIONDEPARTMENT),
    STORENAME = trimws(STORENAME),
    STORENAME = ifelse(STORENAME == "White Oak", "White Oak Town Center", STORENAME),
    SIZE = ifelse(SIZE == "5LTR", "5L", SIZE),
    SIZE = ifelse(SIZE == "3LTR", "3L", SIZE),
    SIZE = ifelse(SIZE == "1LTR", "1L", SIZE),
    TRANSDATE = as.Date(TRANSDATE, format = "%Y-%m-%d"),
    YEAR = year(TRANSDATE),
    DESCRIPTION = DESCRIPTION %>% str_trim() %>% str_squish() %>% str_to_upper()
  )
```
---
## 4) Normalize PACKUNIT values

```r
transactions <- transactions %>%
  mutate(
    PACKUNIT = case_when(
      grepl("^[0-9]+$", PACKUNIT) & PACKUNIT == "1" ~ "Btl",
      grepl("^[0-9]+$", PACKUNIT) ~ paste0(PACKUNIT, "pk"),
      TRUE ~ PACKUNIT
    )
  )
```
---
## 5) Fix inconsistent pack unit labels

```r
transactions <- transactions %>%
  mutate(
    PACKUNIT = case_when(
      PACKUNIT == "08pk" ~ "8pk",
      PACKUNIT == "06pk" ~ "6pk",
      PACKUNIT == "05pk" ~ "5pk",
      PACKUNIT == "04pk" ~ "4pk",
      PACKUNIT == "03pk" ~ "3pk",
      PACKUNIT == "02pk" ~ "2pk",
      PACKUNIT == "01pk" ~ "Btl",
      TRUE ~ PACKUNIT
    )
  )
```
---
## 6) Fix missing BEER classification type

```r
transactions$CLASSIFICATIONTYPE <- ifelse(
  (transactions$CLASSIFICATIONTYPE == "" |
     transactions$CLASSIFICATIONTYPE == "NULL" |
     is.na(transactions$CLASSIFICATIONTYPE)) &
    transactions$CLASSIFICATIONDEPARTMENT == "BEER",
  transactions$CLASSIFICATIONDEPARTMENT,
  transactions$CLASSIFICATIONTYPE
)
```
---
## 7) # Standardize Product Descriptions

```r
# Identify items with conflicting descriptions

item_desc_conflicts <- transactions %>%
  select(ITEMID, DESCRIPTION) %>%
  distinct() %>%
  group_by(ITEMID) %>%
  filter(n_distinct(DESCRIPTION) > 1) %>%
  arrange(ITEMID)

# Choose preferred description (the one that includes size)

dict <- item_desc_conflicts %>%
  mutate(has_size = str_detect(DESCRIPTION, "\\b[0-9.]+(ML|L|OZ|Z|LTR)\\b")) %>%
  group_by(ITEMID) %>%
  mutate(
    preferred_desc = DESCRIPTION[has_size][1]  # choose version with size
  ) %>%
  filter(!has_size) %>%  # only the short ones need replacing
  ungroup()

# Output suggested case_when rules for manual review

dict %>%
  mutate(
    rule = paste0(
      'DESCRIPTION == "', DESCRIPTION, '" ~ "', preferred_desc, '"'
    )
  ) %>%
  pull(rule) %>%
  cat(sep = ",\n")

# Automated cleaning method

transactions_clean <- transactions %>%
  group_by(ITEMID) %>%
  mutate(
    has_size = str_detect(DESCRIPTION, "\\b[0-9.]+(ML|L|OZ|Z)\\b"),
    DESCRIPTION = if (any(has_size)) DESCRIPTION[has_size][1] else DESCRIPTION
  ) %>%
  ungroup() %>%
  select(-has_size)

# Manual override rules for edge cases 

transactions <- transactions %>%
  mutate(
    DESCRIPTION = case_when(
      DESCRIPTION == "BOWMANS PET VODKA" ~ "BOWMANS PET VODKA 200ML",
      DESCRIPTION == "NEW AMSTERDAM APPLE" ~ "NEW AMSTERDAM APPLE - 50ML",
      DESCRIPTION == "NEW AMSTERDAM PEACH" ~ "NEW AMSTERDAM PEACH - 50ML",
      DESCRIPTION == "NEW AMSTERDAM- PASSION F" ~ "NEW AMSTERDAM- PASSION F - 50ML",
      DESCRIPTION == "NEW AMSTERDAM GRAPEFRUIT" ~ "NEW AMSTERDAM GRAPEFRUIT - 50ML",
      DESCRIPTION == "L MARCA PRO -ROSE" ~ "L MARCA PRO -ROSE - 750ML",
      DESCRIPTION == "MR BOSTON TRIPLE SEC" ~ "MR BOSTON TRIPLE SEC - 1LTR",
      DESCRIPTION == "MR BOSTON PEACH SCHNAPPS 1" ~ "MR BOSTON PEACH SCHNAPPS 1 - 1LTR",
      
      TRUE ~ DESCRIPTION
    )
  )
```
---
## 8) Data Quality & Structure Diagnostics

```r
cat("\n=== NA Value Check ===\n")
na_counts <- colSums(is.na(transactions))
print(na_counts)

if(any(na_counts > 0)) {
  cat("\nColumns with missing values:\n")
  print(na_counts[na_counts > 0])
} else {
  cat("No missing values found.\n")
}
```

---
## 9) Product Mix & Size Performance

```r
# Sales & quantity by package size

transactions %>%
  group_by(SIZE) %>%
  summarise(
    TotalSales = sum(NETAMOUNT, na.rm = TRUE),
    TotalQty   = sum(LINEQTY,  na.rm = TRUE)
  ) %>%
  arrange(desc(TotalSales))

# Sales by category type (e.g., VODKA, PROSECCO, etc.)

transactions %>%
  group_by(CLASSIFICATIONTYPE) %>%
  summarise(TotalSales = sum(NETAMOUNT, na.rm = TRUE))
```

---
## 10) High-Value Transactions (> $200)

```r
# Count & sales of high-value receipts per store

top_high_value_stores <- transactions %>%
  filter(NETAMOUNT > 200) %>%
  group_by(STORENAME) %>%
  summarise(
    HighValueCount      = n(),
    TotalHighValueSales = sum(NETAMOUNT),
    .groups = "drop"
  ) %>%
  arrange(desc(HighValueCount))

# % of receipts that are > $200 per store

store_high_value_share <- transactions %>%
  group_by(STORENAME) %>%
  summarise(
    TotalTransactions      = n(),
    HighValueTransactions  = sum(NETAMOUNT > 200, na.rm = TRUE),
    HighValueShare         = round(100 * HighValueTransactions / TotalTransactions, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(HighValueShare))
```

---
## 11) Bottle / Pack Size Popularity

```r
transactionsnew <- transactions %>%
  mutate(UnitType = ifelse(CLASSIFICATIONDEPARTMENT == "BEER", PACKUNIT, SIZE))

size_counts <- transactionsnew %>%
  group_by(CLASSIFICATIONDEPARTMENT, UnitType) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))
```

---
## 12) Category Mix by Store (Pivot)

```r

# Receipt counts by department x store

dept_by_store <- transactions %>%
  group_by(STORENAME, CLASSIFICATIONDEPARTMENT) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = CLASSIFICATIONDEPARTMENT, values_from = Count, values_fill = 0)


# Sales by department x store

dept_sales_by_store <- transactions %>%
  group_by(STORENAME, CLASSIFICATIONDEPARTMENT) %>%
  summarise(TotalSales = sum(NETAMOUNT, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = CLASSIFICATIONDEPARTMENT, values_from = TotalSales, values_fill = 0)
```

---
## 13) Weekday / Weekend / Holiday Sales

```r
transactions$TRANSDATE <- as.Date(transactions$TRANSDATE)

ny_range  <- seq(as.Date("2023-12-21"), as.Date("2024-12-24"), by="day")
ny_range1 <- seq(as.Date("2024-12-21"), as.Date("2025-12-24"), by="day")
ny_range2 <- seq(as.Date("2023-12-29"), as.Date("2024-01-02"), by="day")
ny_range3 <- seq(as.Date("2024-12-29"), as.Date("2025-01-02"), by="day")

us_holidays <- as.Date(c(
  "2023-07-04","2023-09-04","2023-11-23","2023-12-25",
  ny_range, ny_range1,
  "2024-05-27","2024-07-04","2024-09-02","2024-11-28","2024-12-25",
  ny_range2, ny_range3,
  "2025-05-26"
))

transactions <- transactions %>%
  mutate(
    DayOfWeek = wday(TRANSDATE, label = TRUE),
    DayType   = case_when(
      TRANSDATE %in% us_holidays         ~ "Holiday",
      DayOfWeek %in% c("Sat","Sun")      ~ "Weekend",
      TRUE                               ~ "Weekday"
    )
  )

# Aggregate daily by DayType

daily_sales <- transactions %>%
  group_by(TRANSDATE, DayType) %>%
  summarise(DailySales = sum(NETAMOUNT, na.rm = TRUE), .groups = "drop")

# Mean per DayType

avg_totals <- daily_sales %>%
  group_by(DayType) %>%
  summarise(AverageDailySales = mean(DailySales), .groups = "drop")
```

---
## 14) Daily Sales — FY23-24 vs FY24-25

```r
daily_sales <- transactions %>%
  group_by(TRANSDATE) %>%
  summarise(TotalSales = sum(NETAMOUNT, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Day   = day(TRANSDATE),
    Month = month(TRANSDATE, label = TRUE, abbr = TRUE),
    Year  = year(TRANSDATE)
  )

fy23_24 <- daily_sales %>% filter(TRANSDATE >= as.Date("2023-07-01") & TRANSDATE <= as.Date("2024-06-30"))
fy24_25 <- daily_sales %>% filter(TRANSDATE >= as.Date("2024-07-01") & TRANSDATE <= as.Date("2025-06-30"))

fy23_24_aligned <- fy23_24 %>% mutate(FirstOfMonth=floor_date(TRANSDATE,"month"),
                                      WeekdayOffset=wday(FirstOfMonth,week_start=1)-1,
                                      DayAligned=Day+WeekdayOffset)

fy24_25_aligned <- fy24_25 %>% mutate(FirstOfMonth=floor_date(TRANSDATE,"month"),
                                      WeekdayOffset=wday(FirstOfMonth,week_start=1)-1,
                                      DayAligned=Day+WeekdayOffset)
```

---
## 15) Weekday Sales Ranking

```r
weekday_sales <- transactions %>%
  mutate(Weekday = wday(TRANSDATE, label = TRUE, abbr = TRUE)) %>%
  group_by(Weekday) %>%
  summarise(TotalSales = sum(NETAMOUNT, na.rm = TRUE), .groups = "drop") %>%
  arrange(TotalSales)
```

---

## 16) Monthly Sales — Compare Fiscal Years

```r
transactions <- transactions %>%
  mutate(YearMonth = format(TRANSDATE, "%Y-%m"))

fy23_24_monthly <- transactions %>%
  filter(TRANSDATE >= "2023-07-01" & TRANSDATE <= "2024-06-30") %>%
  group_by(YearMonth) %>%
  summarise(TotalSales = sum(NETAMOUNT), .groups = "drop") %>%
  arrange(YearMonth) %>%
  mutate(FiscalYear="FY23-24", MonthIndex=row_number())

fy24_25_monthly <- transactions %>%
  filter(TRANSDATE >= "2024-07-01" & TRANSDATE <= "2025-06-30") %>%
  group_by(YearMonth) %>%
  summarise(TotalSales = sum(NETAMOUNT), .groups = "drop") %>%
  arrange(YearMonth) %>%
  mutate(FiscalYear="FY24-25", MonthIndex=row_number())

combined_monthly <- rbind(fy23_24_monthly, fy24_25_monthly)
```

---
## 17) Store Summary — Sales, Transactions, Baskets (FY24-25)

```r
fy24_25 <- transactions %>%
  filter(TRANSDATE >= "2024-07-01" & TRANSDATE <= "2025-06-30")

store_fy24_25_summary <- fy24_25 %>%
  group_by(STORENAME) %>%
  summarise(
    TotalSales        = sum(NETAMOUNT, na.rm=TRUE),
    TotalTransactions = n(),
    .groups = "drop"
  )

baskets_fy24_25 <- fy24_25 %>%
  group_by(STORENAME) %>%
  summarise(TotalBaskets = n_distinct(TRANSACTIONID), .groups = "drop")

store_fy24_25_final <- store_fy24_25_summary %>%
  left_join(baskets_fy24_25, by="STORENAME") %>%
  arrange(STORENAME)
```

---
## 18) Add Square Footage & Efficiency Metrics

```r
store_sqft <- readr::read_csv("Designation.csv") # expect columns: STORENAME, SquareFootage

store_fy24_25_final <- store_fy24_25_final %>%
  left_join(store_sqft %>% select(STORENAME, SquareFootage), by="STORENAME") %>%
  mutate(
    TotalSalesPerSqFt        = TotalSales        / SquareFootage,
    TotalTransactionsPerSqFt = TotalTransactions / SquareFootage,
    TotalBasketsPerSqFt      = TotalBaskets      / SquareFootage
  ) %>%
  arrange(STORENAME)

traffic_median <- median(store_fy24_25_final$TotalTransactionsPerSqFt, na.rm = TRUE)
sales_median   <- median(store_fy24_25_final$TotalSalesPerSqFt,        na.rm = TRUE)
basket_median  <- median(store_fy24_25_final$TotalBasketsPerSqFt,      na.rm = TRUE)
```

---
## 19) Basket Structure — Avg Basket $ and Items

```r
basket_metrics <- fy24_25 %>%
  group_by(STORENAME, TRANSACTIONID) %>%
  summarise(
    BasketTotal = sum(NETAMOUNT, na.rm = TRUE),
    Items       = sum(LINEQTY,  na.rm = TRUE),
    .groups = "drop"
  )

basket_summary <- basket_metrics %>%
  group_by(STORENAME) %>%
  summarise(
    AvgBasketValue    = mean(BasketTotal, na.rm = TRUE),
    AvgItemsPerBasket = mean(Items,       na.rm = TRUE),
    .groups = "drop"
  )

store_fy24_25_final <- store_fy24_25_final %>%
  left_join(basket_summary, by = "STORENAME") %>%
  arrange(STORENAME)
```

---
## 20) Quadrant Classification: Sales, Traffic & Basket Density

```r
store_fy24_25_quad <- store_fy24_25_final %>%
  mutate(
    Quad_Sales_Traffic = case_when(
      TotalSalesPerSqFt >= sales_median  & TotalTransactionsPerSqFt >= traffic_median ~ "High Spend & High Traffic",
      TotalSalesPerSqFt >= sales_median  & TotalTransactionsPerSqFt <  traffic_median ~ "High Spend & Low Traffic",
      TotalSalesPerSqFt <  sales_median  & TotalTransactionsPerSqFt >= traffic_median ~ "Low Spend & High Traffic",
      TRUE ~ "Low Spend & Low Traffic"
    ),
    Quad_Sales_Baskets = case_when(
      TotalSalesPerSqFt >= sales_median  & TotalBasketsPerSqFt >= basket_median ~ "High Spend & High Baskets",
      TotalSalesPerSqFt >= sales_median  & TotalBasketsPerSqFt <  basket_median ~ "High Spend & Low Baskets",
      TotalSalesPerSqFt <  sales_median  & TotalBasketsPerSqFt >= basket_median ~ "Low Spend & High Baskets",
      TRUE ~ "Low Spend & Low Baskets"
    ),
    Quad_Traffic_Baskets = case_when(
      TotalTransactionsPerSqFt >= traffic_median & TotalBasketsPerSqFt >= basket_median ~ "High Traffic & High Baskets",
      TotalTransactionsPerSqFt >= traffic_median & TotalBasketsPerSqFt <  basket_median ~ "High Traffic & Low Baskets",
      TotalTransactionsPerSqFt <  traffic_median & TotalBasketsPerSqFt >= basket_median ~ "Low Traffic & High Baskets",
      TRUE ~ "Low Traffic & Low Baskets"
    )
  )
```

---
## 21) Demographics vs Store Performance


```r
demographics <- read.csv(
  "Demographics.csv",
  header = TRUE, sep = ",", encoding = "UTF-8", quote = "\""
)

demographics_selected <- demographics %>%
  select(STORENAME, SquareFootage, TotalPopulation, White, Black, Hispanic,
         X25YearsOld, PovertyLevel, TwicePovertyLevel)

store_fy24_25_summary <- store_fy24_25_summary %>%
  left_join(demographics_selected, by = "STORENAME")

# 15a) Size vs Sales

cor(store_fy24_25_summary$TotalSales, store_fy24_25_summary$SquareFootage, use = "complete.obs")
summary(lm(TotalSales ~ SquareFootage, data = store_fy24_25_summary))

# 15b) Population vs Sales

cor(store_fy24_25_summary$TotalSales, store_fy24_25_summary$TotalPopulation, use = "complete.obs")
summary(lm(TotalSales ~ TotalPopulation, data = store_fy24_25_summary))

# 15c) 25+ vs Sales

cor(store_fy24_25_summary$TotalSales, store_fy24_25_summary$X25YearsOld, use = "complete.obs")
summary(lm(TotalSales ~ X25YearsOld, data = store_fy24_25_summary))

# 15d) Poverty vs Sales

cor(store_fy24_25_summary$TotalSales, store_fy24_25_summary$PovertyLevel, use = "complete.obs")
summary(lm(TotalSales ~ PovertyLevel, data = store_fy24_25_summary))

# 15e) Twice Poverty vs Sales

cor(store_fy24_25_summary$TotalSales, store_fy24_25_summary$TwicePovertyLevel, use = "complete.obs")
summary(lm(TotalSales ~ TwicePovertyLevel, data = store_fy24_25_summary))
```

---
## 22) Market Basket Analysis (Apriori)

```r
# Aggregate items per basket

baskets <- transactions %>%
  group_by(TRANSACTIONID) %>%
  summarise(
    Items      = paste(unique(ITEMID), collapse = ", "),
    Item_Count = n_distinct(ITEMID),
    .groups = "drop"
  ) %>%
  filter(Item_Count > 1) # keep multi-item baskets only

# Convert to list-of-vectors for 'arules'

basket_list <- baskets %>%
  mutate(Items = str_split(Items, ",\\s*")) %>%
  pull(Items)

basket_trans <- as(basket_list, "transactions")

# Apriori parameters: tune these by business tolerance

rules <- apriori(
  basket_trans,
  parameter = list(supp = 0.001, conf = 0.2, minlen = 2)
)

# Inspect top rules by lift

inspect(head(sort(rules, by = "lift"), 20))
```

---
## 23) Top/Bottom SKUs — Store & District

```r
# Scope: FY24-25 window

fy24_25 <- transactions %>%
  filter(TRANSDATE >= "2024-07-01" & TRANSDATE <= "2025-06-30")

# Top 5 by qty per store

top5_items_per_store <- fy24_25 %>%
  group_by(STORENAME, ITEMID, DESCRIPTION) %>%
  summarise(
    total_qty   = sum(TOTALQTY,  na.rm = TRUE),
    total_sales = sum(NETAMOUNT, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  slice_max(total_qty, n = 5, with_ties = FALSE) %>%
  arrange(STORENAME, desc(total_qty)) %>%
  ungroup()

# Top 30 overall by qty

top10_items_all_stores <- fy24_25 %>%
  group_by(ITEMID, DESCRIPTION) %>%
  summarise(
    total_qty   = sum(TOTALQTY,  na.rm = TRUE),
    total_sales = sum(NETAMOUNT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  slice_max(total_qty, n = 30, with_ties = FALSE) %>%
  arrange(desc(total_qty))

# Bottom 5 by qty per store (stock only)

bottom5_items_per_stores <- fy24_25 %>%
  filter(ITEMTAG == "ST") %>%
  group_by(STORENAME, ITEMID, DESCRIPTION) %>%
  summarise(
    total_qty   = sum(TOTALQTY,  na.rm = TRUE),
    total_sales = sum(NETAMOUNT, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  slice_min(total_qty, n = 5, with_ties = FALSE) %>%
  arrange(STORENAME, total_qty) %>%
  ungroup()

# Bottom 30 overall (stock only)

bottom30_items_all_stores <- fy24_25 %>%
  filter(ITEMTAG == "ST") %>%
  group_by(ITEMID, DESCRIPTION) %>%
  summarise(
    total_qty   = sum(TOTALQTY,  na.rm = TRUE),
    total_sales = sum(NETAMOUNT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  slice_min(total_qty, n = 30, with_ties = FALSE) %>%
  arrange(total_qty)
```

---
## Data Processing Flowchart
```
Clean CSV (TransactionData_Clean.csv)
   ↓
Load & Validate
   ↓
Size/Mix/Category KPIs
   ↓
Weekday/Weekend/Holiday Effects
   ↓
Time Series FY vs FY
   ↓
Store KPIs + SqFt Efficiency
   ↓
Demographics Correlations
   ↓
MBA (Apriori) — Cross-Sell
   ↓
Top/Bottom SKU Reports
```

---

## Save Key Tables (optional)

```r
readr::write_csv(store_fy24_25_final, "store_fy24_25_final.csv")
readr::write_csv(top5_items_per_store, "top5_items_per_store.csv")
readr::write_csv(top10_items_all_stores, "top10_items_all_stores.csv")
readr::write_csv(bottom5_items_per_stores, "bottom5_items_per_stores.csv")
readr::write_csv(bottom10_items_all_stores, "bottom30_items_all_stores.csv")
```

---

## Final Notes
- The Apriori thresholds (`supp`, `conf`) should be tuned to your **basket size** and **SKU breadth**.
- Re-run this notebook **only** after refreshing `TransactionData_Clean.csv` from your ETL.
