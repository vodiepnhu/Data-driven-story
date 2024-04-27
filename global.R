library(dplyr)

# Read pre-processed file into global environment
undergrad_data = readRDS(file = "./data.Rda")


# Select and rename columns to create df for user interface datatable
undergrad_table = undergrad_data %>% select(., College = college, State = state, 
                                            `Admission Rate` = adm_rate, `Average Total Cost` = avg_cost, 
                                            `Average Tuition` = avg_tuition, `Student Population` = population, 
                                            `Students With Loans` = pct_loan, `Median Debt` = md_debt, 
                                            `Median Earnings` = md_earnings_10, `Comp Sci Majors` = comp_deg,
                                            `Math Majors` = math_deg)
