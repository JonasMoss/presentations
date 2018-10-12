# This file massages the data from Cuddy et al. 2017.
# The xlsx file is from https://osf.io/pfh6r/, the supplementaty material of
# 
# @article{cuddy2018p,
# title={P-Curving a More Comprehensive Body of Research on Postural Feedback 
#   Reveals Clear Evidential Value For Power-Posing Effects: Reply to Simmons and 
#   Simonsohn (2017)},
# author={Cuddy, Amy JC and Schultz, S Jack and Fosse, Nathan E},
# journal={Psychological science},
# volume={29},
# number={4},
# pages={656--666},
# year={2018},
# publisher={SAGE Publications Sage CA: Los Angeles, CA}
# }

cuddy2018 = readxl::read_xlsx("data/cuddy2018.xlsx")

cuddy2018 %>%
  dplyr::filter(!is.na(`(Main) Results`)) %>%
  dplyr::select(study = Study, results = `(Main) Results`) ->
  dat

splited = matrix(unlist(strsplit(dat$results, "=")), nrow = 2)
z = as.numeric(splited[2, ])
z = sqrt(z)*(substr(splited[1, ], 1, 1) != "t") + 
    z*(substr(splited[1, ], 1, 1) == "t")

df = c(83,  83,  40,  83,  26,  67,  39, Inf, Inf, Inf, 
       164, 60, Inf,  34,  95, Inf, 147, 126,  73, Inf,
       53,  68,  79,  39,  28,  70, 101,  79,  58, 196,
       20,  37,  37,  18,  16,  18,  51, Inf,  37, Inf,
       79,  58,  84, Inf, 118,  87,  80,  58, Inf, Inf,
       29,  66,  Inf)

dat %>% 
  dplyr::filter(!is.infinite(df)) ->
  dat

dat$vi = 1/df[!is.infinite(df)]
dat$yi = z[!is.infinite(df)]*sqrt(dat$vi)
dat$type = 1
dat$lower = qt(0.975, df = 1/dat$vi - 1)
dat$upper = qt(0.025, df = 1/dat$vi - 1)
saveRDS(dat, file = "data/cuddy2018.rds")
