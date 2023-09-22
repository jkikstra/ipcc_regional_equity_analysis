# Prepare data =====
library("here")
library("tidyverse")
library("vroom")
library("readxl")
library("ggsci")
library("patchwork")
library("ggthemes")
library("scales")
# install.packages("DescTools")
library("DescTools")

here::i_am("ipcc_regional_equity_analysis.Rproj")

source(here("utils.R"))

# ar6 scenario data locations ====

ar6.data.location <- "C:/Users/kikstra/OneDrive - IIASA/_Other/Data/Scenario data/Scenario Databases/AR6_Scenarios_Database_R10_regions_v1.1"
engage.extra.data.location <- "C:/Users/kikstra/OneDrive - IIASA/_Other/Data/Scenario data/Scenario Databases/Engage2.0_data/ENGAGE_scenario_data_r10_regions_r2.0.csv"
path.led.r5 <- here("data","AR6_Scenarios_Database_R5_regions_v1.1_LED.csv")
ar6.data.file <- "AR6_Scenarios_Database_R10_regions_v1.1.csv"
ar6.meta.file <- "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"

ALREADY.PROCESSED <- FALSE

# selection of data ====

if(ALREADY.PROCESSED){
  ar6 <- vroom(here("data", "ar6db_processed.csv"))
  ar6.pc <- vroom(here("data", "ar6db_processed_percapita.csv"))

  ar6.meta <- read_excel(file.path(ar6.data.location, ar6.meta.file),
                         sheet = "meta_Ch3vetted_withclimate") %>%
    rename(model=Model, scenario=Scenario) %>%
    select(model, scenario, Category, IMP_marker,
           Project_study, Ssp_family)
} else {
  variables <- c(
    "Emissions|CO2|Energy and Industrial Processes",
    "Population",
    "GDP|PPP",
    "Consumption",
    "Final Energy"
  )
  regions <- c(
    "R10AFRICA",
    "R10CHINA+",
    "R10INDIA+",
    "R10LATIN_AM",
    "R10MIDDLE_EAST",
    "R10REST_ASIA",
    "R10EUROPE",
    "R10NORTH_AM",
    "R10PAC_OECD",
    "R10REF_ECON",
    "R10ROWO"
  )

  # ar6 scenario data read in

  ar6.data <- vroom(
    file.path(
      ar6.data.location,
      ar6.data.file
    )
  ) %>% filter(
    Variable %in% variables,
    Region %in% regions
  ) %>% iamc_wide_to_long(upper.to.lower = T)

  engage.poles.data <- vroom(
    engage.extra.data.location
  ) %>%
    mutate_cond(Region=="Sub-Saharan Africa (R10)",Region="R10AFRICA") %>%
    mutate_cond(Region=="China (R10)",Region="R10CHINA+") %>%
    mutate_cond(Region=="European Union and Western Europe (R10)",Region="R10EUROPE") %>%
    mutate_cond(Region=="South Asia (R10)",Region="R10INDIA+") %>%
    mutate_cond(Region=="Latin America (R10)",Region="R10LATIN_AM") %>%
    mutate_cond(Region=="Middle East (R10)",Region="R10MIDDLE_EAST") %>%
    mutate_cond(Region=="North America (R10)",Region="R10NORTH_AM") %>%
    mutate_cond(Region=="Pacific OECD (R10)",Region="R10PAC_OECD") %>%
    mutate_cond(Region=="Reforming Economies (R10)",Region="R10REF_ECON") %>%
    mutate_cond(Region=="Other Asian countries (R10)",Region="R10REST_ASIA") %>%
    filter(
      Variable %in% variables,
      Region %in% regions,
      Model == "POLES-JRC ENGAGE"
    ) %>%
    iamc_wide_to_long(upper.to.lower = T) %>%
    mutate(model="POLES ENGAGE")

  ar6.meta <- read_excel(file.path(ar6.data.location, ar6.meta.file),
                         sheet = "meta_Ch3vetted_withclimate") %>%
    rename(model=Model, scenario=Scenario) %>%
    select(model, scenario, Category, IMP_marker,
           Project_study, Ssp_family)

  ar6 <- ar6.data %>% bind_rows(engage.poles.data) %>%
    left_join(ar6.meta)

  # rename models to model framework
  ar6 <- ar6 %>%
    mutate(model=ifelse(grepl(model,pattern="REMIND",fixed=T),"REMIND",model)) %>%
    mutate(model=ifelse(grepl(model,pattern="MESSAGE",fixed=T),"MESSAGE",model)) %>%
    mutate(model=ifelse(grepl(model,pattern="WITCH",fixed=T),"WITCH",model)) %>%
    mutate(model=ifelse(grepl(model,pattern="AIM",fixed=T),"AIM",model)) %>%
    mutate(model=ifelse(grepl(model,pattern="GCAM",fixed=T),"GCAM",model)) %>%
    mutate(model=ifelse(grepl(model,pattern="IMAGE",fixed=T),"IMAGE",model))

  # delete rest of world (mostly REMIND)
  ar6 <- ar6 %>% filter(region!="R10ROWO")


  write_delim(
    x = ar6,
    file = here("data","ar6db_processed.csv"),
    delim = ","
  )


  # now divide all variables by population
  ar6.pc <- ar6 %>% to_per_capita()

  write_delim(
    x = ar6.pc,
    file = here("data","ar6db_processed_percapita.csv"),
    delim = ","
  )
}



colour.vector <- c(
  "#ADD8E6","#ADD8E6", "#ADD8E6","#ADD8E6",
  "#ADD8E6","#ADD8E6", "#FFD700","#FFD700",
  "#FFD700","#FFD700", "lightgrey"
)
region.order <- c(
  "R10AFRICA",
  "R10INDIA+",
  "R10REST_ASIA",
  "R10MIDDLE_EAST",
  "R10LATIN_AM",
  "R10CHINA+",
  "R10REF_ECON",
  "R10EUROPE",
  "R10PAC_OECD",
  "R10NORTH_AM")
region.order <- rev(region.order)

# Numbers of Kanitkar et al. preprint (revision 1) ====
region.vector <- c(
  "R10AFRICA",
  "R10CHINA+",
  "R10INDIA+",
  "R10LATIN_AM",
  "R10MIDDLE_EAST",
  "R10REST_ASIA",
  "R10EUROPE",
  "R10NORTH_AM",
  "R10PAC_OECD",
  "R10REF_ECON",
  "R10ROWO"
)
table1.kanitkar <- tibble(
  model = "MESSAGE",
  region = region.vector,
  mean.value = c(
    -0.16, # "R10AFRICA",
    -0.11, # "R10CHINA+",
    -0.08, # "R10INDIA+",
    -0.12, # "R10LATIN_AM",
    -0.04, # "R10MIDDLE_EAST",
    -0.06, # "R10REST_ASIA",
    -0.07, # "R10EUROPE",
    -0.07, # "R10NORTH_AM",
    -0.10, # "R10PAC_OECD",
    -0.09, # "R10REF_ECON",
    NA # "R10ROWO"
  ),
  median.value = c(
    -0.16, # "R10AFRICA",
    -0.11, # "R10CHINA+",
    -0.08, # "R10INDIA+",
    -0.11, # "R10LATIN_AM",
    -0.03, # "R10MIDDLE_EAST",
    -0.06, # "R10REST_ASIA",
    -0.07, # "R10EUROPE",
    -0.07, # "R10NORTH_AM",
    -0.10, # "R10PAC_OECD",
    -0.09, # "R10REF_ECON",
    NA # "R10ROWO"
  )
) %>%
  bind_rows(
    tibble(
      model = "REMIND",
      region = region.vector,
      mean.value = c(
        -0.10, # "R10AFRICA",
        -0.07, # "R10CHINA+",
        -0.03, # "R10INDIA+",
        -0.08, # "R10LATIN_AM",
        -0.02, # "R10MIDDLE_EAST",
        -0.05, # "R10REST_ASIA",
        -0.06, # "R10EUROPE",
        -0.07, # "R10NORTH_AM",
        -0.07, # "R10PAC_OECD",
        -0.06, # "R10REF_ECON",
        NA # "R10ROWO"
      ),
      median.value = c(
        -0.11, # "R10AFRICA",
        -0.07, # "R10CHINA+",
        -0.03, # "R10INDIA+",
        -0.07, # "R10LATIN_AM",
        -0.02, # "R10MIDDLE_EAST",
        -0.05, # "R10REST_ASIA",
        -0.07, # "R10EUROPE",
        -0.07, # "R10NORTH_AM",
        -0.08, # "R10PAC_OECD",
        -0.06, # "R10REF_ECON",
        NA # "R10ROWO"
      )
    )

  ) %>% filter(region!="R10ROWO")

figure2.kanitkar <-
  # panel A start
  tibble(
    panel = "A",
    variable = "GDP|PPP",
    ipcc.category = "C1",
    year = 2020,
    region = region.vector,
    value = c(
      3, # "R10AFRICA",
      17, # "R10CHINA+",
      6, # "R10INDIA+",
      14, # "R10LATIN_AM",
      14, # "R10MIDDLE_EAST",
      9, # "R10REST_ASIA",
      34, # "R10EUROPE",
      55, # "R10NORTH_AM",
      39, # "R10PAC_OECD",
      17, # "R10REF_ECON",
      NA # "R10ROWO"
    )
  ) %>%
  # panel A end
  bind_rows(
    tibble(
      panel = "A",
      variable = "GDP|PPP",
      ipcc.category = "C1",
      year = 2050,
      region = region.vector,
      value = c(
        9, # "R10AFRICA",
        45, # "R10CHINA+",
        16, # "R10INDIA+",
        27, # "R10LATIN_AM",
        24, # "R10MIDDLE_EAST",
        20, # "R10REST_ASIA",
        51, # "R10EUROPE",
        72, # "R10NORTH_AM",
        54, # "R10PAC_OECD",
        33, # "R10REF_ECON",
        NA # "R10ROWO"
      )
    )
  ) %>%
  # panel B start
  bind_rows(
    tibble(
      panel = "B",
      variable = "GDP|PPP",
      ipcc.category = "C3",
      year = 2020,
      region = region.vector,
      value = c(
        3, # "R10AFRICA",
        18, # "R10CHINA+",
        6, # "R10INDIA+",
        15, # "R10LATIN_AM",
        14, # "R10MIDDLE_EAST",
        9, # "R10REST_ASIA",
        34, # "R10EUROPE",
        54, # "R10NORTH_AM",
        38, # "R10PAC_OECD",
        17, # "R10REF_ECON",
        NA # "R10ROWO"
      )
    )
  ) %>%
  # panel B end
  bind_rows(
    tibble(
      panel = "B",
      variable = "GDP|PPP",
      ipcc.category = "C3",
      year = 2050,
      region = region.vector,
      value = c(
        9, # "R10AFRICA",
        46, # "R10CHINA+",
        17, # "R10INDIA+",
        27, # "R10LATIN_AM",
        25, # "R10MIDDLE_EAST",
        22, # "R10REST_ASIA",
        51, # "R10EUROPE",
        72, # "R10NORTH_AM",
        54, # "R10PAC_OECD",
        34, # "R10REF_ECON",
        NA # "R10ROWO"
      )
    )
  ) %>%
  # panel C start
  bind_rows(
    tibble(
      panel = "C",
      variable = "Consumption",
      ipcc.category = "C1",
      year = 2020,
      region = region.vector,
      value = c(
        1, # "R10AFRICA",
        5, # "R10CHINA+",
        1, # "R10INDIA+",
        6, # "R10LATIN_AM",
        5, # "R10MIDDLE_EAST",
        3, # "R10REST_ASIA",
        22, # "R10EUROPE",
        35, # "R10NORTH_AM",
        28, # "R10PAC_OECD",
        6, # "R10REF_ECON",
        NA # "R10ROWO"
      )
    )
  ) %>%
  # panel C end
  bind_rows(
    tibble(
      panel = "C",
      variable = "Consumption",
      ipcc.category = "C1",
      year = 2050,
      region = region.vector,
      value = c(
        3, # "R10AFRICA",
        16, # "R10CHINA+",
        4, # "R10INDIA+",
        12, # "R10LATIN_AM",
        10, # "R10MIDDLE_EAST",
        8, # "R10REST_ASIA",
        40, # "R10EUROPE",
        59, # "R10NORTH_AM",
        49, # "R10PAC_OECD",
        12, # "R10REF_ECON",
        NA # "R10ROWO"
      )
    )
  ) %>%
  # panel D start
  bind_rows(
    tibble(
      panel = "D",
      variable = "Consumption",
      ipcc.category = "C3",
      year = 2020,
      region = region.vector,
      value = c(
        1, # "R10AFRICA",
        6, # "R10CHINA+",
        1, # "R10INDIA+",
        6, # "R10LATIN_AM",
        5, # "R10MIDDLE_EAST",
        4, # "R10REST_ASIA",
        22, # "R10EUROPE",
        37, # "R10NORTH_AM",
        28, # "R10PAC_OECD",
        6, # "R10REF_ECON",
        NA # "R10ROWO"
      )
    )
  ) %>%
  # panel D end
  bind_rows(
    tibble(
      panel = "D",
      variable = "Consumption",
      ipcc.category = "C3",
      year = 2050,
      region = region.vector,
      value = c(
        3, # "R10AFRICA",
        19, # "R10CHINA+",
        5, # "R10INDIA+",
        12, # "R10LATIN_AM",
        10, # "R10MIDDLE_EAST",
        9, # "R10REST_ASIA",
        39, # "R10EUROPE",
        58, # "R10NORTH_AM",
        46, # "R10PAC_OECD",
        14, # "R10REF_ECON",
        NA # "R10ROWO"
      )
    )
  ) %>% filter(region!="R10ROWO")

# Reproducing and comparing Kanitkar et al. Table 1 =====

regions.decline.data.v1v2 <- ar6 %>%
  normalise_iamc_long(starting.year = 2020) %>%
  filter(variable=="Emissions|CO2|Energy and Industrial Processes") %>%
  mutate(value = value-1) %>% # instead of normalised, show percentage change
  add_percentile_columns(group.cols = c("model","region","variable","year","Category"),
                         percentiles = c(0.05,0.25,0.5,0.75,0.95))
regions.decline.data.mean.values.v1v2 <- regions.decline.data.v1v2 %>%
  reframe(mean.value=mean(value), .by = c("model","region","variable","year","Category"))
regions.decline.data.table1.v1v2 <- regions.decline.data.v1v2 %>%
  distinct(model,region,variable,year,Category,p5,p25,p50,p75,p95) %>%
  left_join(
    regions.decline.data.mean.values.v1v2
  ) %>%
  filter(
    year%in%c(2030),
    Category%in%c("C1"),
    model%in%c("MESSAGE","REMIND")
  )

regions.decline.data.v3 <- ar6.pc %>%
  normalise_iamc_long(starting.year = 2020) %>%
  filter(variable=="Emissions|CO2|Energy and Industrial Processes") %>%
  mutate(value = value-1) %>% # instead of normalised, show percentage change
  add_percentile_columns(group.cols = c("model","region","variable","year","Category"),
                         percentiles = c(0.05,0.25,0.5,0.75,0.95))
regions.decline.data.mean.values.v3 <- regions.decline.data.v3 %>%
  reframe(mean.value=mean(value), .by = c("model","region","variable","year","Category"))
regions.decline.data.table1.v3 <- regions.decline.data.v3 %>%
  distinct(model,region,variable,year,Category,p5,p25,p50,p75,p95) %>%
  left_join(
    regions.decline.data.mean.values.v3
  ) %>%
  filter(
    year%in%c(2030),
    Category%in%c("C1"),
    model%in%c("MESSAGE","REMIND")
  )

p.regions.decline.table1.v1 <- ggplot(regions.decline.data.table1.v1v2 %>%
                                      mutate(
                                      p5annual=ifelse(p5<0,   -(1-((1+p5)^(1/10))),  ((1+p5)^(1/10))-1),
                                      p25annual=ifelse(p25<0, -(1-((1+p25)^(1/10))), ((1+p25)^(1/10))-1),
                                      p50annual=ifelse(p50<0, -(1-((1+p50)^(1/10))), ((1+p50)^(1/10))-1),
                                      p75annual=ifelse(p75<0, -(1-((1+p75)^(1/10))), ((1+p75)^(1/10))-1),
                                      p95annual=ifelse(p95<0, -(1-((1+p95)^(1/10))), ((1+p95)^(1/10))-1)
                                    ) %>% mutate(region = factor(region, levels=region.order)),
                                          aes(colour=region)) +
  facet_grid(.~model) +


  # add annual median and ranges
  geom_linerange(
    aes(ymin=p25,ymax=p75,
        colour=region,x=region,
        alpha=0.5,
        linewidth=1.5)
  ) +
  geom_linerange(
    aes(ymin=p5,ymax=p95,
        colour=region,x=region,
        alpha=0.5,
        linewidth=0.75)
  ) +
  geom_point(
    aes(y=p50,
        colour=region,x=region),
    alpha=1
  ) +

  # add kanitkar numbers
  geom_point(
    aes(y=mean.value,x=region),
    shape = 17, colour = "black", size=3,
    data = table1.kanitkar
  ) +



  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +

  # legend, labels, etc.
  labs(
    title = "Emissions Reductions (C1) - total emissions.",
    subtitle = "Kanitkar et al. = black triangles",
    caption = "Coloured ranges: 5-95th (thin), 25-75th (thick), median (point).\nUsing variable 'Emissions|CO2|Energy and Industrial Processes'."
  ) +


  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent, name = "Percentage change from 2020 to 2030") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  scale_color_manual(values = colour.vector, breaks = region.vector) +
  coord_flip() +
  theme(legend.position = "none")

p.regions.decline.table1.v1

save_ggplot(
  p=p.regions.decline.table1.v1,
  f=here("figures","Table1-jsk-reproduce-v1"),
  h=100,
  w=200
)



p.regions.decline.table1.v2 <- ggplot(regions.decline.data.table1.v1v2 %>%
                                        mutate(
                                          p5annual=ifelse(p5<0,   -(1-((1+p5)^(1/10))),  ((1+p5)^(1/10))-1),
                                          p25annual=ifelse(p25<0, -(1-((1+p25)^(1/10))), ((1+p25)^(1/10))-1),
                                          p50annual=ifelse(p50<0, -(1-((1+p50)^(1/10))), ((1+p50)^(1/10))-1),
                                          p75annual=ifelse(p75<0, -(1-((1+p75)^(1/10))), ((1+p75)^(1/10))-1),
                                          p95annual=ifelse(p95<0, -(1-((1+p95)^(1/10))), ((1+p95)^(1/10))-1)
                                        ) %>% mutate(region = factor(region, levels=region.order)),
                                      aes(colour=region)) +
  facet_grid(.~model) +


  # add annual median and ranges
  geom_linerange(
    aes(ymin=p25annual,ymax=p75annual,
        colour=region,x=region,
        alpha=0.5,
        linewidth=1.5)
  ) +
  geom_linerange(
    aes(ymin=p5annual,ymax=p95annual,
        colour=region,x=region,
        alpha=0.5,
        linewidth=0.75)
  ) +
  geom_point(
    aes(y=p50annual,
        colour=region,x=region),
    alpha=1
  ) +

  # add kanitkar numbers
  geom_point(
    aes(y=mean.value,x=region),
    shape = 17, colour = "black", size=3,
    data = table1.kanitkar
  ) +



  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +

  # legend, labels, etc.
  labs(
    title = "Emissions Reductions (C1) - average annual reduction rates",
    subtitle = "Kanitkar et al. = black triangles",
    caption = "Coloured ranges: 5-95th (thin), 25-75th (thick), median (point).\nUsing variable 'Emissions|CO2|Energy and Industrial Processes'."
  ) +


  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent, name = "Percentage change from 2020 to 2030") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  scale_color_manual(values = colour.vector, breaks = region.vector) +
  coord_flip() +
  theme(legend.position = "none")

p.regions.decline.table1.v2

save_ggplot(
  p=p.regions.decline.table1.v2,
  f=here("figures","Table1-jsk-reproduce-v2"),
  h=100,
  w=200
)




p.regions.decline.table1.v3 <- ggplot(regions.decline.data.table1.v3 %>%
                                        mutate(
                                          p5annual=ifelse(p5<0,   -(1-((1+p5)^(1/10))),  ((1+p5)^(1/10))-1),
                                          p25annual=ifelse(p25<0, -(1-((1+p25)^(1/10))), ((1+p25)^(1/10))-1),
                                          p50annual=ifelse(p50<0, -(1-((1+p50)^(1/10))), ((1+p50)^(1/10))-1),
                                          p75annual=ifelse(p75<0, -(1-((1+p75)^(1/10))), ((1+p75)^(1/10))-1),
                                          p95annual=ifelse(p95<0, -(1-((1+p95)^(1/10))), ((1+p95)^(1/10))-1)
                                        ) %>% mutate(region = factor(region, levels=region.order)),
                                      aes(colour=region)) +
  facet_grid(.~model) +


  # add annual median and ranges
  geom_linerange(
    aes(ymin=p25annual,ymax=p75annual,
        colour=region,x=region,
        alpha=0.5,
        linewidth=1.5)
  ) +
  geom_linerange(
    aes(ymin=p5annual,ymax=p95annual,
        colour=region,x=region,
        alpha=0.5,
        linewidth=0.75)
  ) +
  geom_point(
    aes(y=p50annual,
        colour=region,x=region),
    alpha=1
  ) +

# add kanitkar numbers
geom_point(
  aes(y=mean.value,x=region),
  shape = 17, colour = "black", size=3,
  data = table1.kanitkar
) +



  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +

  # legend, labels, etc.
  labs(
    title = "Emissions Reductions (C1) - average annual reduction rates per capita.",
    subtitle = "Kanitkar et al. = black triangles",
    caption = "Coloured ranges: 5-95th (thin), 25-75th (thick), median (point).\nUsing variable 'Emissions|CO2|Energy and Industrial Processes'."
  ) +


  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent, name = "Percentage change from 2020 to 2030") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  scale_color_manual(values = colour.vector, breaks = region.vector) +
  coord_flip() +
  theme(legend.position = "none")

p.regions.decline.table1.v3

save_ggplot(
  p=p.regions.decline.table1.v3,
  f=here("figures","Table1-jsk-reproduce-v3"),
  h=100,
  w=200
)




























regions.decline.data.table1.jsk <- ar6 %>%
  normalise_iamc_long(starting.year = 2020) %>%
  filter(variable=="Emissions|CO2|Energy and Industrial Processes") %>%
  mutate(value = value-1) %>% # instead of normalised, show percentage change
  mutate(Project_study=ifelse(is.na(Project_study),"Other",Project_study)) %>%
  filter(
    Category%in%c("C1", "C2", "C3", "C4")
  ) %>%
  add_percentile_columns(group.cols = c("model","region","variable","year","Project_study"),
                         percentiles = c(0.05,0.25,0.5,0.75,0.95)) %>%
  distinct(model,region,variable,year,Project_study,p5,p25,p50,p75,p95) %>%
  left_join(
    ar6 %>%
      normalise_iamc_long(starting.year = 2020) %>%
      filter(variable=="Emissions|CO2|Energy and Industrial Processes") %>%
      mutate(value = value-1) %>% # instead of normalised, show percentage change
      mutate(Project_study=ifelse(is.na(Project_study),"Other",Project_study)) %>%
      filter(
        Category%in%c("C1", "C2", "C3", "C4")
      ) %>%
      reframe(mean.value=mean(value), .by = c("model","region","variable","year","Project_study"))
  ) %>%
  filter(
    year%in%c(2030)
  )

p.regions.decline.data.ipcc <-  ggplot(regions.decline.data.table1.jsk,
                                      aes(colour=region)) +
  facet_grid(region~.) +

  # add range
  geom_linerange(
    aes(ymin=p25,ymax=p75,
        colour=model,x=Project_study,
        alpha=0.7,
        linewidth=1.5)
  ) +
  geom_linerange(
    aes(ymin=p5,ymax=p95,
        colour=model,x=Project_study,
        alpha=0.7,
        linewidth=0.75)
  ) +
  # add average
  geom_point(
    aes(y=mean.value,
        colour=model,x=Project_study)
  ) +

  # add kanitkar numbers
  geom_point(
    aes(y=mean.value,
        colour=model,x="Kanitkar et al."),
    shape = 17, colour = "black", size = 3,
    data = table1.kanitkar
  ) +



  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +

  # legend, labels, etc.
  labs(
    title = "Emissions|CO2|Energy and Industrial Processes",
    subtitle = "Including all projects & scenarios, C1, C2, C3.\nNot including outliers (0-5th or 95-100th percentiles).\nColours: different models.\nBlack triangles: Kanitkar et al." ,
    caption = "Coloured ranges: 5-95th (thin), 25-75th (thick), mean (point)"
  ) +


  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent, name = "Percentage change from 2020 to 2030") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  scale_color_cosmic() +
  coord_flip() +
  theme(legend.position = "none")
p.regions.decline.data.ipcc

save_ggplot(
  p=p.regions.decline.data.ipcc,
  f=here("figures","Table1-jsk-detail-allC1234"),
  h=600
)

# Reproducing and comparing Kanitkar et al. Figure 2 =====

# reproducing
figure2.kanitkar.data <- figure2.kanitkar %>% mutate(Project_study = "Kanitkar et al.")

p.figure2 <-  ggplot(figure2.kanitkar.data %>% pivot_wider(names_from = year, values_from = value) %>%
                       mutate(region = factor(region, levels=region.order)),
                     aes()) +
  facet_wrap(panel~., nrow = 4) +

  # add 2050 points of AR6 scenarios
  geom_point(
    # position = position_jitterdodge(jitter.width = 0, jitter.height = 0, dodge.width = 0.25),
    aes(x = `2050`, y = region,
        colour = region),
    shape = 124, size = 5,
    alpha=0.7,
    data = ar6.pc %>%
      filter(variable%in%c("GDP|PPP","Consumption")) %>%
      mutate(Project_study=ifelse(is.na(Project_study),"Other",Project_study)) %>% pivot_wider(names_from = year, values_from = value) %>%
      filter(`2050`!=0, model!="MAgPIE 4.2") %>%

      mutate(panel = ifelse(
        variable=="GDP|PPP"&Category=="C1", "A",
        ifelse(
          variable=="GDP|PPP"&Category=="C3", "B",
          ifelse(
            variable=="Consumption"&Category=="C1", "C",
            ifelse(
              variable=="Consumption"&Category=="C3", "D",
              NA
            )
          )
        )
      )) %>% filter(!is.na(panel)) %>%

      mutate(region = factor(region, levels=region.order))
  ) +


  # add kanitkar et al.
  geom_segment(aes(x = `2020`, y = region, xend = `2050`, yend = region),
               colour = "black",
               arrow = arrow(length = unit(0.2, "cm")),
               data = figure2.kanitkar.data %>% pivot_wider(names_from = year, values_from = value) %>%
                 mutate(region = factor(region, levels=region.order))) +


  # add 2050 points of AR6 IMPs
  geom_point(
    # position = position_jitterdodge(jitter.width = 0, jitter.height = 0, dodge.width = 0.25),
    aes(x = `2050`, y = region,
        colour = region),
    shape = 42, size = 4, colour="purple",
    alpha=1,
    data = ar6.pc %>%
      filter(variable%in%c("GDP|PPP","Consumption")) %>%
      mutate(Project_study=ifelse(is.na(Project_study),"Other",Project_study)) %>% pivot_wider(names_from = year, values_from = value) %>%
      filter(`2050`!=0, model!="MAgPIE 4.2") %>%

      mutate(panel = ifelse(
        variable=="GDP|PPP"&Category=="C1", "A",
        ifelse(
          variable=="GDP|PPP"&Category=="C3", "B",
          ifelse(
            variable=="Consumption"&Category=="C1", "C",
            ifelse(
              variable=="Consumption"&Category=="C3", "D",
              NA
            )
          )
        )
      )) %>% filter(!is.na(panel)) %>%

      filter(IMP_marker!="non-IMP") %>%

      mutate(region = factor(region, levels=region.order))
  ) +


  # legend, labels, etc.
  labs(
    title = "Consumption and GDP per capita",
    subtitle = "Each line is 1 scenario (only 2050)\nBlack arrows: Kanitkar et al. (2020 -> 2050)\nPurple asterisks: IMPs (only 2050)" ,
    caption = "2020 only shown for the Kanitkar et al. values"
  ) +


  # geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Thousand dollar per capita") +
  ylab(NULL) +
  theme_classic() +
  theme_hc() +
  scale_color_manual(values = colour.vector, breaks = (region.vector)) +
  # coord_flip() +
  theme(legend.position = "none")
p.figure2

save_ggplot(
  p=p.figure2,
  f=here("figures","Figure2-jsk-reproduce"),
  h=200,
  w=120
)


# check absolute and relative growth

figure2.kanitkar.data.absgrowth <- figure2.kanitkar.data %>% pivot_wider(names_from = year, values_from = value) %>%
  mutate(absolute.growth = `2050`-`2020`)
figure2.kanitkar.data.relgrowth <- figure2.kanitkar.data %>% pivot_wider(names_from = year, values_from = value) %>%
  mutate(relative.growth = (`2050`-`2020`)/`2020`)
figure2.jsk.allc1234.relgrowth <- ar6.pc %>%
  filter(variable%in%c("GDP|PPP","Consumption")) %>%
  mutate(Project_study=ifelse(is.na(Project_study),"Other",Project_study)) %>%
  filter(
    year %in% c(2020, 2050),
    Category%in%c("C1", "C2", "C3", "C4")
  ) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  mutate(value = (`2050`-`2020`)/`2020`) %>%
  add_percentile_columns(group.cols = c("model","region","variable","Project_study"),
                         percentiles = c(0.05,0.25,0.5,0.75,0.95)) %>%
  distinct(model,region,variable,Project_study,p5,p25,p50,p75,p95) %>%
  filter(
    p50!=-1, # Guo 2021
    model!="EPPA 6" # does not report these variables?
    )
figure2.jsk.allc1234.relgrowth.perscenario <- ar6.pc %>%
  filter(variable%in%c("GDP|PPP","Consumption")) %>%
  mutate(Project_study=ifelse(is.na(Project_study),"Other",Project_study)) %>%
  filter(
    year %in% c(2020, 2050),
    Category%in%c("C1", "C2", "C3", "C4")
  ) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  mutate(value = (`2050`-`2020`)/`2020`) %>%
  filter(
    value!=-1, # don't inlcude variables that are 0 but should have been NA (Guo 2021)
    model!="EPPA 6" # does not report these variables
  )



p.figure2.jsk.allc1234.relgrowth <-  ggplot() +
  facet_grid(region~variable) +

  geom_point(
    aes(x = model, y = value,
        colour = Project_study),
    shape = 124, size = 5,
    alpha=0.3,
    data = figure2.jsk.allc1234.relgrowth.perscenario
  ) +

  # add median
  geom_point(
    aes(y=p50,
        colour=Project_study,x=model),
    # colour = "black",
    data = figure2.jsk.allc1234.relgrowth
  ) +

  # add kanitkar numbers
  geom_point(
    aes(y=relative.growth, shape = ipcc.category,
        colour=model,x="Kanitkar et al."),
    # shape = 17,
    colour = "black", size = 3,
    data = figure2.kanitkar.data.relgrowth
  ) +



  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +

  # legend, labels, etc.
  labs(
    title = "Economic growth (relative) from 2020 to 2050",
    caption = "Coloured line: a scenario. Median of study: point.\nAcross IPCC categories C1, C2, C3, C4."
  ) +


  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent, name = "Percentage change from 2020 to 2050") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  # scale_color_cosmic() +
  guides(color=guide_legend("Study"),
        shape=guide_legend("IPCC category\nKanitkar et al."),
        linewidth="none",
        alpha="none"
        ) +
  coord_flip()
p.figure2.jsk.allc1234.relgrowth

save_ggplot(
  p=p.figure2.jsk.allc1234.relgrowth,
  f=here("figures","Figure2-jsk-detail-allc1234-relative-economic-growth"),
  h=400,
  w=300
)

# SSP: checking Kanitkar et al. numbers alignment =====
# compare kanitkar et al. estimates (of GDP per cap) with SSP ranges (e.g. india, africa, europe, northamerica)
ssp.gdp.data.selected <- ar6.pc %>%
  filter(variable%in%c("GDP|PPP")) %>%
  filter(
    year %in% seq(2010,2050,10),
    Category%in%c("C1", "C2", "C3", "C4"),
    !is.na(Ssp_family),
    region %in% c("R10AFRICA","R10INDIA+",
                  "R10NORTH_AM","R10EUROPE")
  ) %>%
  add_percentile_columns(group.cols = c("region", "year","Ssp_family"),
                         percentiles = c(0.05,0.25,0.5,0.75,0.95)) %>%
  distinct(region,year,Ssp_family,p5,p25,p50,p75,p95)

p.ssp.gdp <- ggplot(ssp.gdp.data.selected %>% mutate(SSP = paste0("SSP",as.character(Ssp_family))),
                     aes(x=year)) +
  facet_grid(~SSP) +
  geom_ribbon(
    aes(ymin=p5,ymax=p95,
        fill = region),
    alpha = 0.15
  ) +
  geom_ribbon(
    aes(ymin=p25,ymax=p75,
        fill = region),
    alpha = 0.45
  ) +
  geom_line(
    aes(y=p50,
        colour = region),
    alpha = 1
  ) +

  # add kanitkar numbers
  geom_point(
    aes(y=value, colour=region),
    shape = 17,
    size = 2,
    data = figure2.kanitkar.data %>% filter(
      variable=="GDP|PPP",
      region %in% c("R10AFRICA","R10INDIA+",
                    "R10NORTH_AM","R10EUROPE")
      )
  ) +

  theme_classic() +
  theme_hc() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  # legend, labels, etc.
  labs(
    title = "GDP per capita (PPP)",
    subtitle = "Kanitkar et al. = coloured triangles.\nIPCC ranges across C1, C2, C3, C4.",
    caption = "Coloured ranges: 5-95th (light), 25-75th (darker), median (line).\nUsing variable 'GDP|PPP'."
  ) +
  xlab(NULL) + ylab("Thousand dollar per capita per year") +
  scale_color_cosmic() +
  scale_fill_cosmic() +
  guides(color=guide_legend("Selected regions"),
         fill=guide_legend("Selected regions"),
         shape="none",
         alpha="none"
  )

p.ssp.gdp

save_ggplot(
  p=p.ssp.gdp,
  f=here("figures","SSP-gdp_per_capita-allc1234"),
  h=130,
  w=200
)

# Gini: checking inequality statements of Kanitkar et al. =====

pop2020message <- ar6 %>% filter(variable=="Population") %>%
  filter(year==2020,model=="MESSAGE",scenario=="EN_NPi2100") %>%
  pull(value)
pop2050message <- ar6 %>% filter(variable=="Population") %>%
  filter(year==2050,model=="MESSAGE",scenario=="EN_NPi2100") %>%
  pull(value)

interregional.gini.kanitkar.data <- figure2.kanitkar.data %>% reframe(
  gini = ifelse(year==2020,Gini(value,pop2020message),
                ifelse(year==2050,Gini(value,pop2050message),
                       NA
  )),
  .by = c("variable","ipcc.category","year", "Project_study")
) %>% rename(Category = ipcc.category)

interregional.gini.ipcc.data <- ar6.pc %>%
  # filter(variable%in%c("GDP|PPP","Consumption", "Final Energy")) %>%
  left_join(ar6 %>% filter(variable=="Population") %>% rename(population=value) %>%
              select(model,scenario,region,year,population)) %>%
  filter(
    year %in% c(2020, 2030, 2040, 2050),
    Category%in%c("C1", "C2", "C3", "C4")
  ) %>% reframe(
    gini = Gini(value,
                population
                # rep(1,10)
                ),
    .by = c("model","scenario","variable","Category","year","Ssp_family")
  ) %>%
  filter(
    !is.na(Ssp_family)
  )

ar6.led <- vroom(
    path.led.r5
  ) %>% filter(
    Variable %in% variables
  ) %>% iamc_wide_to_long(upper.to.lower = T) %>% left_join(ar6.meta)
ar6.pc.led <- ar6.led %>%
  to_per_capita()
interregional.gini.ipcc.data.LED <- ar6.pc.led %>%
  left_join(ar6.led %>% filter(variable=="Population") %>%
              rename(population=value) %>%
              select(model,scenario,region,year,population)) %>%
  filter(
    year %in% c(2020, 2030, 2040, 2050)
  ) %>% reframe(
    gini = Gini(value,
                population
    ),
    .by = c("model","scenario","variable","Category","year","Ssp_family")
  )


p.gini <- ggplot() +
  facet_grid(Ssp_family~variable) +


  geom_boxplot(
    data=interregional.gini.ipcc.data %>%
      filter(
        year %in% c(2020,2030,2050),
        variable!="Consumption",variable!="Emissions|CO2|Energy and Industrial Processes"
      ) %>%
      filter(
        Ssp_family %in% c(1,2)
      ) %>%
      mutate(Ssp_family=paste0("SSP",Ssp_family)),
    colour = "darkgrey",
    alpha=0.3,
    aes(x = as.factor(year),
        colour=variable,
        # linetype=Category,
        y = gini)
  ) +

  geom_point(
    data=interregional.gini.ipcc.data.LED %>%
      mutate(scenario = "IMP Low Demand") %>%
      filter(
        year %in% c(2020,2030,2050),
        variable%in%c("Final Energy", "GDP|PPP")
      ) %>%
      filter(
        Ssp_family %in% c(1,2)
      ) %>%
      mutate(Ssp_family=paste0("SSP",Ssp_family)),
    # colour = "red",
    position = position_dodge(width = 0.2),
    # alpha=0.3,
    size = 3,
    aes(x = as.factor(year),
        colour=scenario,
        shape=scenario,
        y = gini)
  ) +

  geom_point(
    data=interregional.gini.ipcc.data %>% filter(scenario == "SusDev_SDP-PkBudg1000") %>%
      mutate(scenario = "IMP Shifting Pathways") %>%
      filter(
        year %in% c(2020,2030,2050),
        variable!="Consumption",variable!="Emissions|CO2|Energy and Industrial Processes"
      ) %>%
      filter(
        Ssp_family %in% c(1,2)
      ) %>%
      mutate(Ssp_family=paste0("SSP",Ssp_family)),
    # colour = "red",
    position = position_dodge(width = 0.2),
    # alpha=0.3,
    size = 3,
    aes(x = as.factor(year),
        colour=scenario,
        shape=scenario,
        y = gini)
  ) +

  # kanitkar et al.
  geom_point(
    data=interregional.gini.kanitkar.data %>%
      mutate(Category = paste0("Kanitkar et al. ", Category)) %>%
      filter(
        variable!="Consumption"
      ),
    # shape = 17,
    # colour = "black",
    size = 3,
    position = position_dodge(width = 0.2),
    aes(shape=Category,
        colour=Category,
        x = as.factor(year),
        y = gini)
  ) +




  geom_hline(yintercept = 0) +
  scale_y_continuous(name = "Gini coefficient (population weighted)", limits = c(0,0.6)) +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  # scale_fill_cosmic() +
  # scale_color_cosmic() +
  scale_color_manual(values = c("dodgerblue", "red", "black", "black")) +
  guides(
    color=guide_legend("Study",nrow = 2),
    shape=guide_legend("Study",nrow = 2)
  )

p.gini

save_ggplot(
  p=p.gini,
  f=here("figures","Interregional-inequality-by-ssp"),
  h=120,
  w=150
)


# Trying to reproduce the weighting step:

# You could try to follow these steps to see if this helps with the replication issue:
# 1. Take the REMIND C1 scenarios and cut them into bins of 10 Gt CO2 along the column "Cumulative net CO2 (2020 to netzero, Gt CO2) (Harm-Infilled)"
# 2. Compute weights for each bin as (1/n) where n is the number of scenarios (drop the bins with no scenarios)
# 3. Multiply the quantity of interest with the weights per scenario and divide by the sum across weights.
cumulative.emissions.remind <- read_excel(file.path(ar6.data.location, ar6.meta.file),
                                   sheet = "meta_Ch3vetted_withclimate") %>%
  rename(model=Model, scenario=Scenario) %>%
  select(model, scenario,
         Category,
         Ssp_family,
         `Cumulative net CO2 (2020 to netzero, Gt CO2) (Harm-Infilled)`) %>%
  filter(
    grepl(model,pattern="REMIND",fixed=T),
    Category%in%c("C1","C3")
  ) %>% mutate(`model`="REMIND") %>%
  mutate(
    bin = cut(`Cumulative net CO2 (2020 to netzero, Gt CO2) (Harm-Infilled)`, breaks=seq(0, 2000, 10))
  )

ar6.remind.cumemissions <- ar6.pc %>% left_join(
  cumulative.emissions.remind
  ) %>% drop_na(
    `Cumulative net CO2 (2020 to netzero, Gt CO2) (Harm-Infilled)`
  ) %>% mutate(
    bin = cut(`Cumulative net CO2 (2020 to netzero, Gt CO2) (Harm-Infilled)`, breaks=seq(0, 2000, 10))
  )
ar6.remind.cumemissions.binnumbers <- cumulative.emissions.remind %>% count(bin)
# number.of.bins <- nrow(ar6.remind.cumemissions.binnumbers)

ar6.remind.cumemissions.weighted <- ar6.remind.cumemissions %>%
  left_join(ar6.remind.cumemissions.binnumbers) %>%
  reframe(
    value = weighted.mean(value, weights = 1/n),
    .by = c(region,variable,unit,year,Category)
  )

weighting.panel.bins <- ggplot(cumulative.emissions.remind %>% mutate_cond(grepl(scenario,pattern="SDP"),Ssp_family=1) %>% mutate(Ssp_family=as.character(Ssp_family)),
       aes(x=`Cumulative net CO2 (2020 to netzero, Gt CO2) (Harm-Infilled)`)) +
  facet_grid(Category~.) +
  geom_histogram(binwidth = 10,
                 aes(fill=Ssp_family)) +
  theme_classic() +
  theme_hc() +
  scale_y_continuous(breaks = c(0,4,8,12)) +
  scale_fill_manual(breaks = c("1","2","5"), values = c("purple","dodgerblue","darkgrey")) +
  labs(
    caption = "Bin width of 10GtCO2"
  )
weighting.panel.bins

scenarios.vs.weighted.numbers.finalenergy <- ggplot() +
  facet_grid(Category~region) +
  geom_line(
    data = ar6.remind.cumemissions %>% filter(variable=="Final Energy") %>%
      filter(
        region%in%c("R10AFRICA","R10INDIA+","R10NORTH_AM")
      ),
    aes(x=year,y=value*1e3,
        group=scenario,
        colour=as.character(Ssp_family)),
    # colour="darkgrey",
    alpha=0.7, linewidth = 1.5,
  ) +
  geom_line(
    data = ar6.remind.cumemissions.weighted %>% filter(variable=="Final Energy") %>%
      filter(
        region%in%c("R10AFRICA","R10INDIA+","R10NORTH_AM")
      ),
    aes(x=year,y=value*1e3),
    colour="red", alpha=1, linetype = "dashed",
    size = 1.5
  ) +
  scale_color_manual(breaks = c("1","2","5"), values = c("purple","dodgerblue","darkgrey")) +
  theme_classic() +
  theme_hc() +
  xlab(NULL) +
  ylab("Final Energy per capita (GJ/cap/yr)") +
  labs(
    subtitle = "Final Energy",
    caption = "Red dashed line is weighted mean by weighting per bin."
  ) +
  guides(
    color=guide_legend("SSP",nrow = 1)
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
scenarios.vs.weighted.numbers.finalenergy

scenarios.vs.weighted.numbers.GDP <- ggplot() +
  facet_grid(Category~region) +
  geom_line(
    data = ar6.remind.cumemissions %>% filter(variable=="GDP|PPP") %>%
      filter(
        region%in%c("R10AFRICA","R10INDIA+","R10NORTH_AM")
      ),
    aes(x=year,y=value,group=scenario,
        colour=as.character(Ssp_family)),
    # colour="darkgrey",
    alpha=0.7, linewidth = 1.5,
  ) +
  geom_line(
    data = ar6.remind.cumemissions.weighted %>% filter(variable=="GDP|PPP") %>%
      filter(
        region%in%c("R10AFRICA","R10INDIA+","R10NORTH_AM")
      ),
    aes(x=year,y=value),
    colour="red", alpha=1, linetype = "3131",
    linewidth = 1.5
  ) +
  scale_color_manual(breaks = c("1","2","5"), values = c("purple","dodgerblue","darkgrey")) +
  theme_classic() +
  theme_hc() +
  xlab(NULL) +
  ylab("GDP (Thousand dollar per capita per year)") +
  labs(
    subtitle = "GDP (PPP)",
    caption = "Red dashed line is weighted mean by weighting per bin."
  ) +
  guides(
    color=guide_legend("SSP",nrow = 1)
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
scenarios.vs.weighted.numbers.GDP


p.weighting.example <- (weighting.panel.bins) / (scenarios.vs.weighted.numbers.finalenergy | scenarios.vs.weighted.numbers.GDP)
p.weighting.example

save_ggplot(
  p=p.weighting.example,
  f=here("figures","Weighting-example"),
  h=200,
  w=225
)

