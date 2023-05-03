dof <- readRDS("/mnt/projects/FusionData/0.CCB/myUpstream/lifeTables/dataOut/e0ciState.RDS") %>%
  arrange(nyrs, GEOID, sex, raceCode, year)

dcdc <- readRDS("/mnt/projects/FusionData/0.CCB/myUpstream/lifeTables/dataOut/Archive_LTCountyState_using_DCDC_1282021/e0ciState.RDS") %>%
  mutate(sex = str_to_title(sex)) %>%
  arrange(nyrs, GEOID, sex, raceCode, year)


check <- filter(dof, year %in% 2000:2009)
check1 <- filter(dcdc, year %in% 2000:2009)
identical(check, check1)


check <- dof %>%
  full_join(dcdc, by = c("nyrs", "GEOID", "sex", "raceCode", "year")) %>%
  mutate(diff = ex.x - ex.y,
         abs = abs(diff))
