## Owner Sensitivity =======================
## =========================================

library(FinCal)
## This R script calculates how many future homeowners could afford units based on different price points. 

## The baseline assumptions are defined in the residential_baseline_materials.Rmd so that users can easily alter them.

baseline_downpayment <- .05
baseline_hoa_fees <- 100
baseline_utilities <- 221
baseline_taxes <- 0.55
baseline_insurance <- 1600
baseline_pmi_rate <- 0.08
baseline_interest_rate <- .028
baseline_amo <- 30
baseline_cost_burden <- .3

baseline_product_cost <- 150000
baseline_total_mortgage <- baseline_product_cost - (baseline_product_cost*baseline_downpayment)


additional_housing_cost <- baseline_hoa_fees + baseline_utilities + ((baseline_product_cost/100)*baseline_taxes)/12 + (baseline_insurance/12) 


mortgage_payment <- pmt(baseline_interest_rate/12, baseline_amo*12, -baseline_total_mortgage, fv = 0, type = 0)

mortgage_payment 

total_cost <- mortgage_payment + additional_housing_cost

minimum_income <- (total_cost*12)/baseline_cost_burden
