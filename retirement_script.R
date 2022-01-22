library(tidyverse)
library(lubridate)

birthdate <- ymd("1983-11-04")
basd <- ymd("2007-05-26")
o5_promo <- ymd("2023-06-01")
o6_promo <- ymd("2028-06-01")
active_retirement_date <- ymd("2037-05-26")
death_date <- ymd("2063-11-04")
start_date <- ymd("2022-07-01")
inflation <- .03
reserve_rank <- 

pay_scale <- tibble(rank= c(rep("o4",16),rep("o5",16),rep("o6",16)),
                    years = rep(15:30,3),
                    pay= c(8557,rep(8715,2),rep(8805,13),8977,rep(9544,2),rep(9814,2),rep(10081,2),rep(10384,9),9548,rep(10455,2),rep(10988,2),rep(11520,2),rep(11824,2),rep(12131,2),rep(12725,4),12980))
ny_bah <- tibble(rank = c("o4","o5","o6"), bah = c(3357,3627,3657))



retirement_pay <- function(ets_date = ymd("2022-05-26"),
                        o5_promo = ymd("2023-06-01"),
                        o6_promo = ymd("2028-06-01"),
                        reserve_years = 6){
  retired_active = interval(basd,ets_date) %/% years(1) >= 20
  years_of_active_service <- interval(basd,ets_date)%/% years(1) + reserve_years
  high_3_percent <- ifelse(years_of_active_service < 20, 0, .5 + (years_of_active_service - 20)* .025)
  
  by_month <- tibble(mth = floor_date(start_date, "months") + months(0:(interval(start_date,death_date)  %/% months(1))))
  
  pay_table <- by_month %>% 
    mutate(rank = case_when(
      mth >= ets_date & retired_active ~ "retired",
      mth >= ets_date + years(reserve_years) & years_of_active_service >= 20 ~ "retired",
      mth >= ets_date + years(reserve_years) & years_of_active_service < 20 ~ "out",
      mth >= o6_promo ~ "o6",
      mth >= o5_promo ~ "o5",
      TRUE ~ "o4")) %>% 
    mutate(years_service = case_when(
      mth > ets_date + years(reserve_years) ~ years_of_active_service,
      mth > ets_date ~ interval(basd,mth) %/% years(1),
      TRUE ~ interval(basd,mth) %/% years(1))) %>% 
    mutate(active = mth < ets_date) %>% 
    left_join(pay_scale, by = c("rank","years_service"="years")) %>% 
    rename(base_pay=pay) %>% 
    mutate(base_pay = ifelse(mth > active_retirement_date,0, base_pay * (1 + inflation) ^ (interval(today(),mth)  %/% years(1)))) %>% 
    left_join(ny_bah) %>% 
    replace_na(list(bah = 0, base_pay = 0)) %>% 
    mutate(total_active = ifelse(month(mth)==month(ets_date-months(1)) & !active & !(rank %in% c("retired","out")), 
                                 base_pay*2,
                                 base_pay*active + bah*active))
  
  retirement_pay <- pay_table %>% 
    slice_max(order_by = base_pay,n=36, with_ties= FALSE) %>% 
    summarize(retirement_pay = mean(base_pay)*high_3_percent) %>% pull()
  
  pay_table <- 
    pay_table %>% 
    mutate(retire_pay = ifelse(rank != "retired", 0, retirement_pay))
  
  # print(pay_table %>% summarize(all_active = sum(total_active), all_retire = sum(retire_pay), total = all_active + all_retire, retire_year_pay = retirement_pay*12,years_of_active_service=years_of_active_service))
  return(pay_table %>% summarize(all_active = sum(total_active), all_retire = sum(retire_pay), total = all_active + all_retire, retire_year_pay = retirement_pay*12,years_of_active_service=years_of_active_service))
  # return(pay_table)
  
  }


retirement_pay(ets_date = ymd("2033-05-26"),
                            o5_promo = ymd("2023-06-01"),
                            o6_promo = ymd("2028-06-01"),
                            reserve_years = 0)


pmap(list(ets_date = ymd("2027-05-26") + years(0:10),
     o5_promo = rep(ymd("2023-06-01"),11),
     o6_promo = rep(ymd("2028-06-01"),11),
     reserve_years = rep(0,11)), retirement_pay) %>% 
  map_dfr(bind_rows) %>% 
  mutate(diff = c(0,diff(total)))
  
