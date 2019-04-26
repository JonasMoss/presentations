## =============================================================================
## This file makes the election.rds file. 
## =============================================================================
 
# County level race data is taken from
# https://www.nber.org/data/census-intercensal-county-population-age-sex-race-hispanic.html

# Election data from MIT election lab:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ

library("tidyverse")
library("magrittr")

## =============================================================================
## County-level race data. 
## =============================================================================

countypopmonthasrh = readr::read_csv("election_data/countypopmonthasrh.csv")
coest00intalldata = readr::read_csv("election_data/coest00intalldata.csv")

coest00intalldata %>% 
  dplyr::filter(agegrp == 0, yearref != 1, yearref != 12) %>%
  dplyr::transmute(year = year,
                   state = stname, 
                   county_id = as.integer(county),
                   county = ctyname,
                   white = (wa_male + wa_female)/tot_pop) ->
  whites_early

countypopmonthasrh %>% 
  dplyr::filter(agegrp == 0, yearref > 3) %>%
  dplyr::transmute(year = year,
                   state = stname, 
                   county_id = as.integer(county),
                   county = ctyname,
                   white = (wa_male + wa_female)/tot_pop) ->
  whites_late

whites = rbind(whites_early, whites_late)

## =============================================================================
## County-level election data.
## =============================================================================

load("election_data/countypres_2000-2016.Rdata")

x %>% 
  dplyr::group_by(year, FIPS) %>%
  dplyr::filter(candidatevotes == max(candidatevotes)) %>%
  dplyr::ungroup() ->
  election

## =============================================================================
## Combining the data and saving.
## =============================================================================

election %>% dplyr::transmute(year = year,
                       county_id = FIPS,
                       democrat_won = (party == "democrat"),
                       democrat_percent = (1 - democrat_won) + 
                        candidatevotes/totalvotes*(2*democrat_won - 1)) ->
  election

election = dplyr::inner_join(whites, election)
saveRDS(election, file = "election.rds")

## =============================================================================
## Cleanup.
## =============================================================================

rm(x)
rm(whites)
rm(election)
rm(whites_late, whites_early)
rm(countypopmonthasrh, coest00intalldata)
