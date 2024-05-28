
# 0. libraries, functions, default options

library(data.table)
library(ggplot2)
library(scales)
library(sf)
library(gglaplot)
library(tidyr)
library(ggrepel)
library(cowplot)
library(extrafont)
library(svglite)

options(scipen = 999) # disabling scientific notation


#--------------------------------------------------------------------------

## NB This code is included for information only 

## Go to line c.#100 for code to create the plots  


pal <- gla_pal(gla_theme = "default", n = 10) # extracting the colours of the gla palette
theme_set(theme_gla(gla_theme = "default")) # setting the default plotting theme to the GLA theme

bar_catcols <- c('#ee266d', '#6da7de') # saving colours for zones of London - Inner pink, Outer blue

loadfonts(device = "win", quiet = TRUE) # loading fonts

# 1 Reading in datasets

cob_age_sex <- fread("data/matched_inner_outer/ftb_cob_age_sex_21_dc2103_11.csv")

cob_age_sex_young <- fread("data/matched_inner_outer/ftb_cob_age_sex_21_dc2103_11_st015_01.csv")

hhcomp_ethgroup <- fread("C:/Families/Data_for_github/Census_ethgroup_withdep_children.csv")

hhcomp_ethgroup_without <- fread("C:/Families/Data_for_github/Census_ethgroup_withoutdep_children.csv")

age_youngest <- fread("data/matched_inner_outer/rm006_21_dc1113_11_st014_01.csv")

hhcomp_tenure <- fread("data/matched_inner_outer/rm132_21_LC4412_11_st053_01.csv")

hhcomp_tenure_without <- fread("data/matched_inner_outer/rm132_21_LC4412_11_st053_01_without.csv")

hhcomp_nssec <- fread("data/matched_inner_outer/ftb_hhcomp_nssec_21_dc6115_11_st044_01.csv")

hhcomp_nssec_without <- fread("data/matched_inner_outer/ftb_hhcomp_nssec_21_dc6115_11_st044_01_without.csv")


  ## 1.b Reading in Local Authority Level datasets

cob_age_sex_la <- fread("data/category_matched_at_la/ftb_cob_age_sex_21_dc2103_11.csv")

cob_age_sex_young_la <- fread("data/category_matched_at_la/ftb_cob_age_sex_21_dc2103_11_st015_01.csv")

hhcomp_ethgroup_la <- fread("data/category_matched_at_la/ftb_hhcomp_ethgroup_21_dc1201_11_st106_01.csv")

hhcomp_nssec_la <- fread("data/category_matched_at_la/ftb_hhcomp_nssec_21_dc6115_11_st044_01.csv")

age_youngest_la <- fread("data/category_matched_at_la/rm006_21_dc1113_11_st014_01.csv")

hhcomp_tenure_la <- fread("data/category_matched_at_la/rm132_21_LC4412_11_st053_01.csv")

#------------------------------------------------------------------------------

# 2. For plot 3_3A 
# Household composition by Ethnic Group of Household Representative Person by zone.

  ## creating new variables needed for the plots
hhcomp_ethgroup[, eth_broad := tstrsplit(eth_common, ": ")[[1]]] # extracting the 5-level ethnic group categories from the 19/20-level ethnic group categories
hhcomp_ethgroup[, change := value_21 - value_11] # calculating change from 2011 to 2021

#------------------------------------------------------------------------------------

  ## printing out the aggregations, because these figures are referenced in the report
hhcomp_ethgroup[, .(value_agg_21 = sum(value_21), value_agg_11 = sum(value_11), change = sum(change)),
                by = list(eth_broad)]

hhcomp_ethgroup[, .(value_agg_21 = sum(value_21), value_agg_11 = sum(value_11), change = sum(change)),
                by = list(eth_broad, inner_outer)]

  ## printing out the aggregations for households without dependent children

    ### creating the aggregation first, for the without section 
hhcomp_ethgroup_without[, eth_broad := tstrsplit(eth_common, ": ")[[1]]] # extracting the 5-level ethnic group categories from the 19/20-level ethnic group categories
hhcomp_ethgroup_without[, change := value_21 - value_11] # calculating change from 2011 to 2021

hhcomp_ethgroup_without[, .(value_agg_21 = sum(value_21), value_agg_11 = sum(value_11), change = sum(change)),
                        by = list(eth_broad)]

hhcomp_ethgroup_without[, .(value_agg_21 = sum(value_21), value_agg_11 = sum(value_11), change = sum(change)),
                        by = list(eth_broad, inner_outer)]

#------------------------------------------------------------------------------------

  ## plot of change in aggregated categories

ethgroup_agg <- hhcomp_ethgroup[, .(value_agg_21 = sum(value_21), value_agg_11 = sum(value_11), change = sum(change)),
                                by = list(eth_broad, inner_outer)]

ethgroup_agg[, perc_change := round(100*(change/value_agg_11),1)]

# Create a datafile to save on github

write.csv(ethgroup_agg, "C:/Families/Data_for_github/Census_hhcomp_ethgroup_agg.csv")

#-------------------------------------------------------------------------------------

### Start here to make plot 3_3A

# read in data
ethgroup_agg <- read.csv("C:/Families/Data_for_github/Census_hhcomp_ethgroup_agg.csv") %>%
  data.frame

svg(filename = "C:/Families/Charts_for_github/33_A_ethgroup_aggregated_change.svg", 
     width = 8, height = 6)

ggplot(data = ethgroup_agg, aes(x = eth_broad, y = perc_change, fill = inner_outer)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.text.x = element_text(color = "black", family = "Arial",
                                   size = 14, angle=0, vjust = -0.5, hjust=0)) + 
  theme(axis.text.y = element_text(color = "black", family = "Arial",
                                   size = 14, angle=0, vjust = -0.5, hjust=0)) + 
  theme(legend.text = element_text(color = "black", family = "Arial",
                                   size = 14)) + 
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = "%")) + 
  scale_fill_manual(values = bar_catcols) + 
  coord_flip() 

dev.off()

#--------------------------------------------------------------------------------

# 3. For plot 3_3B 
# Just for black ethnic group
# Household composition by Ethnic Group of Household Representative Person (making the plots separately for those with and without dependent children)

  ## making separate plots

# First for families with dependent children

svg(filename = "C:/Families/Charts_for_github/33_B1_ethnic_grp_with_dc.svg", 
     width = 8, height = 6)


ggplot(data = hhcomp_ethgroup[eth_broad == "Black"], aes(x = eth_common, y = change/1000, fill = inner_outer)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.text.x = element_text(color = "black", family = "Arial",
                                   size=18, angle=0, vjust = -0.5, hjust=0)) + 
  theme(axis.text.y = element_text(color = "black", family = "Arial",
                                   size=18, angle=0, vjust = -0.5, hjust=0)) + 
  theme(legend.text = element_text(color = "black", family = "Arial",
                                   size = 18)) + 
  #theme(panel.grid.major.y = element_blank(),
  #      panel.grid.major.x = calc_element("panel.grid.major.y", theme_get())) + 
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = "")) + 
  scale_fill_manual(values = bar_catcols) + 
  coord_flip() 

dev.off()

  ## Second making the plot for families without dependent children
svg(filename = "C:/Families/Charts_for_github/33_B2_ethnic_grp_without_dc.svg", 
    width = 8, height = 6)

ggplot(data = hhcomp_ethgroup_without[eth_broad == "Black"], aes(x = eth_common, y = change/1000, fill = inner_outer)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.text.x = element_text(color = "black", family = "Arial",
                                   size=18, angle=0, vjust = -0.5, hjust=0)) + 
  theme(axis.text.y = element_text(color = "black", family = "Arial",
                                   size=18, angle=0, vjust = -0.5, hjust=0)) + 
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = "")) + 
  theme(legend.text = element_text(color = "black", family = "Arial",
                                   size = 18)) + 
  scale_fill_manual(values = bar_catcols) + 
  coord_flip() 

dev.off()

## Now making a single chart with both plots together 

  ## plot 1 - with dependent children
with_plot <- ggplot(data = hhcomp_ethgroup[eth_broad == "Black"], aes(x = eth_common, y = change/1000, fill = inner_outer)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.text.x = element_text(color = "black", family = "Arial",
                                   size = 14, angle=0, vjust=0, hjust=0)) + 
  theme(axis.text.y = element_text(color = "black", family = "Arial",
                                   size = 14, angle=0, vjust = -0.5, hjust=0)) + 
  theme(legend.text = element_text(color = "black", family = "Arial",
                                   size = 14)) + 
  #theme(panel.grid.major.y = element_blank(),
  #      panel.grid.major.x = calc_element("panel.grid.major.y", theme_get())) + 
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = "")) + 
  scale_fill_manual(values = bar_catcols) + 
  coord_flip() 

  ## plot 2 - without dependent children
without_plot <- ggplot(data = hhcomp_ethgroup_without[eth_broad == "Black"], aes(x = eth_common, y = change/1000, fill = inner_outer)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.text.x = element_text(color = "black", family = "Arial",
                                   size = 14, angle=0, vjust=0, hjust=0)) + 
  theme(axis.text.y = element_text(color = "black", family = "Arial",
                                   size = 14, angle=0, vjust = -0.5, hjust=0)) + 
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = "")) + 
  theme(legend.text = element_text(color = "black", family = "Arial",
                                   size = 14)) + 
  scale_fill_manual(values = bar_catcols) + 
  coord_flip() 

svg(filename = "C:/Families/Charts_for_github/33_B_ethnic_grp_combined.svg", 
     width = 9, height = 5)

plot_grid(with_plot, without_plot)

dev.off()

#---------------------------------------------------------------------------------

# 4. For plot 2_1O 

# Country of birth by age by sex for potential mothers, alongside births by mother's country of birth

  ## dot-quadrant plot of cob potential mothers vs births by cob mothers. 

#----------------------------------------------------------------------------

# This code is provided for information only

## Go to line c.#300 for code to create the plots  

    ### First read in and fixing up the births by mothers' country of birth file. 

births <- fread("data/other_data/births_by_mothers_country_of_birth_2001_to_2022.csv")

      #### (1) aggregating to inner and outer London

births <- births[grep("E09", gss_code),] # narrowing to London boroughs. Identifiable by the "E09" GSS code. 

i_o_lookup <- fread("data_resources/inner_outer_lookup.csv") # reading in the inner/outer lookup

setkey(i_o_lookup, "lad22cd") ## setting joining key
setkey(births, "gss_code") ## setting the joining key

births <- i_o_lookup[births] ## joining the datasets

births <- melt(births, 
          id.vars = c("lad22cd", "lad22nm", "inner_outer", "year", "usual_residence_of_mother", "type")) # pivoting to longer. It makes the aggregation much easier - only have to aggregate the value column, rather than all of the numerical columns as we'd have to do if we aggregated a wide dataset

births <- births[, .(value = sum(value)),
                by = list(inner_outer, year, variable)] # the final aggregation from borough detail to inner/outer london


      #### (2) aggregating/renaming to aligned countries/regions. Categories are: UK, Middle east and Asia, Europe (non-UK), Africa, Rest of world

names_lookup <- data.table( # creating a lookup from old names to new names
  categories_from = c("total_births_uk_mothers", "total_europe_non_uk", "overseas_mothers_asia", "overseas_mothers_africa",  "overseas_mothers_rest_of_world"),
  cob_common = c("United Kingdom", "Europe (non-UK)", "Middle East and Asia", "Africa", "Rest of world")
)

setkey(names_lookup, "categories_from") ## setting the joining key

setkey(births, "variable") ## setting the joining key

births <- births[names_lookup] ## joining the datasets so that the category names are now the right ones. Joining in this order because it means we only keep the categories that are in the lookup and discard the rest. This is what we want.

births <- births[,-"variable"] ## getting rid of the old country names

      #### (3) narrowing to 2011 and 2021, pivoting and calculating change and then percentage change

births <- births[year %in% c(2021, 2011),] # narrowing to 2011 and 2021, to be (roughly comparable) to census years

births <- 
  dcast(births, 
        inner_outer + cob_common ~ year, value.var = "value") # pivoting the year variable to wide format. To make comparable with the census datasets and to allow for easier calculation of changes and percentage changes
  
births[, change_21_11 := `2021` - `2011`] # calculating change

births[, perc_change_births := round(100*(change_21_11/`2011`))] # calculating percentage change


  ### any fixes to the mothers dataset, then joining both of them
cob_age_sex <- cob_age_sex[sex_2 == "Female",] # keeping to female only

cob_age_sex[cob_common %in% c("Ireland", "Rest of Europe"), cob_common := "Europe (non-UK)"] # renaming all Europe non-EU

cob_age_sex[cob_common %in% c("Antarctica, Oceania, and Other", "The Americas and the Caribbean"), cob_common := "Rest of world"] # renaming all Rest of world

cob_age_sex[, change := value_21 - value_11]

cob_age_sex <- cob_age_sex[, .(value_21 = sum(value_21), value_11 = sum(value_11), change = sum(change)),
                    by = list(inner_outer, cob_common)] ## aggregating the countries/regions of birth

cob_age_sex[, perc_change_mothers := round(100*(change/value_11))] # calculating percentage change


setkey(births, "inner_outer", "cob_common") # setting joining key
setkey(cob_age_sex, "inner_outer", "cob_common") # setting joining key

mothers_births_change <- cob_age_sex[births]

# Create a datafile to save on github

write.csv(mothers_births_change, "C:/Families/Data_for_github/Census_mothers_births_change.csv")

#-------------------------------------------------------------------------------------

    ### Start here to make the plot

catcolour2 = c('#ee266d', '#6da7de')

# read in data
mothers_births_change <- read.csv("C:/Families/Data_for_github/Census_mothers_births_change.csv") %>%
  data.frame

theme_set(theme_gla(gla_theme = "default", x_axis_title = TRUE, y_axis_title = TRUE, free_y_facets = TRUE))

svg(filename = "C:/Families/Charts_for_github/21_O_mothers_births_change.svg",
     width = 9, height = 8)


cob_scatterplot <- ggplot(data = mothers_births_change, aes(x = perc_change_mothers, y = perc_change_births, color = inner_outer)) + 
  geom_abline(slope = 1, intercept = 0, color = "gray", linewidth = 0.75, alpha = 0.5) + 
  geom_point(shape = 18, size = 4) + 
  scale_colour_manual(name = "London Zone:", values = catcolour2) +
  scale_y_continuous(labels = dollar_format(prefix = "" , suffix = "%"), limits = c(-50, 50)) + 
  scale_x_continuous(labels = dollar_format(prefix = "" , suffix = "%"), limits = c(-60, 60)) + 
  labs(x = "Change in potential mothers from 2011 to 2021",
       y = "Change in births from 2011 to 2021") + 
  geom_text_repel(aes(label = cob_common)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "right") + 
  theme(axis.text.x = element_text(color = "black", family = "Arial",
                                   size = 12, angle=0, vjust=0, hjust=0)) + 
  theme(axis.text.y = element_text(color = "black", family = "Arial",
                                   size = 12, angle=0, vjust=0, hjust=0)) + 
  theme(legend.text = element_text(color = "black", family = "Arial",
                                   size = 12, angle=0, vjust=0, hjust=0)) + 
  geom_vline(xintercept = 0, color = "gray", linewidth = 0.75) + 
  geom_hline(yintercept = 0, color = "gray", linewidth = 0.75)


cob_scatterplot

dev.off()

theme_set(theme_gla(gla_theme = "default"))




# 5. For Chart 2_3D, plotting country of birth by age by sex, for children

#----------------------------------------------------------------------------

# This code is provided for information only

## Go to line c.#400 for code to create the plots  

  ## calculating change
cob_age_sex_young[, change := value_21 - value_11]


  ## aggregating categories, dividing
cob_age_sex_young[cob_common == "Ireland", cob_common := "Rest of Europe"]

cob_age_sex_young[cob_common == "Antarctica, Oceania, and Other", cob_common := "The Americas, the Caribbean, and Other"]

cob_age_sex_young[cob_common == "The Americas and the Caribbean", cob_common := "The Americas, the Caribbean, and Other"]

cob_age_sex_young <- cob_age_sex_young[, .(value_21 = sum(value_21),
                                           value_11 = sum(value_11),
                                           change = sum(change)),
                                       by = list(inner_outer, age_common, cob_common)]


  ## ordering by size of outer London
cob_orders <- c("Africa", "The Americas, the Caribbean, and Other", "Middle East and Asia",
                "Rest of Europe", "United Kingdom")


cob_age_sex_young[, cob_common := factor(cob_common,
                                            levels = cob_orders)]

# Create a datafile to save on github

write.csv(cob_age_sex_young, "C:/Families/Data_for_github/Census_cob_age_sex_young.csv")

#---------------------------------------------------------------------------------------

### Start here to make the plot

catcolour2 = c('#ee266d', '#6da7de')

# read in data
mothers_births_change <- read.csv("C:/Families/Data_for_github/Census_cob_age_sex_young.csv") %>%
  data.frame

svg(filename = "C:/Families/Charts_for_github/23_D_cob_children.svg", 
     width = 8, height = 6)

#jpeg(filename = "output_plots/5_cob_children.jpg", 
#     width = 8, height = 6, units = "in", res = 850)

ggplot(data = cob_age_sex_young, aes(x = cob_common, y = change/1000, fill = inner_outer)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.text.x = element_text(color = "black", family = "Arial",
                                   size = 14, angle=0, vjust=0, hjust=0)) + 
  theme(axis.text.y = element_text(color = "black", family = "Arial",
                                   size = 14, angle=0, vjust = -0.5, hjust=0)) + 
  theme(legend.text = element_text(color = "black", family = "Arial",
                                   size = 14)) + 
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = ""), limits = c(-20, 80)) + 
  scale_fill_manual(values = bar_catcols) + 
  coord_flip()

dev.off()


#----------------------------------------------------------------------------------
# 6. For chart 3_2C plotting age of youngest dependent child 

#----------------------------------------------------------------------------

# This code is provided for information only

## Go to line c.#450 for code to create the plots  

  ## calculating change
age_youngest[, change := value_21 - value_11]

age_youngest[, perc_change := round(100*(change/value_11), 1)]

  ## changing the order that the categories will plotted in
age_youngest[, age_common := gsub("Aged ", "", age_common)]

age_youngest[, age_common := factor(age_common)]

levels(age_youngest$age_common) <- c("0 to 4", "5 to 9",
                                     "10 to 15", "16 to 18")

# Create a datafile to save on github

write.csv(mothers_births_change, "C:/Families/Data_for_github/Census_age_youngest.csv")


#-----------------------------------------------------------------------------

### Start here to make the plot

catcolour2 = c('#ee266d', '#6da7de')

# read in data
mothers_births_change <- read.csv("C:/Families/Data_for_github/Census_age_youngest.csv") %>%
  data.frame

  ## creating the plot
svg(filename = "C:/Families/Charts_for_github/32_C_age_youngest.svg", 
     width = 8, height = 6)

#jpeg(filename = "output_plots/6_age_youngest.jpg", 
#     width = 8, height = 6, units = "in", res = 850)

ggplot(data = age_youngest, aes(x = age_common, y = change/1000, fill = inner_outer)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.text.x = element_text(color = "black", family = "Arial",
                                   size = 14, angle=0, vjust=0, hjust=0)) + 
  theme(axis.text.y = element_text(color = "black", family = "Arial",
                                   size = 14, angle=0, vjust = -0.5, hjust=0)) + 
  theme(legend.text = element_text(color = "black", family = "Arial",
                                   size = 14)) + 
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = "")) + 
  scale_fill_manual(values = bar_catcols) + 
  coord_flip() 

dev.off()

#------------------------------------------------------------------------------

# 7. For chart 3_3C plotting household composition by tenure

#----------------------------------------------------------------------------

# This code is provided for information only

## Go to line c.#500 for code to create the plots  

  ## calculating percentages (separately for inner and outer), calculating percentage point difference
  ## note - there must be a totally within-bracket solution to this....put some thought in later

hhcomp_tenure$perc_21 <-  hhcomp_tenure[, .(perc_21 = 100*(value_21/sum(value_21))),
                                        by = list(inner_outer)]$perc_21

hhcomp_tenure$perc_11 <-  hhcomp_tenure[, .(perc_11 = 100*(value_11/sum(value_11))),
                                        by = list(inner_outer)]$perc_11

hhcomp_tenure$pp_diff <- round(hhcomp_tenure$perc_21 - hhcomp_tenure$perc_11,1)

# Create a datafile to save on github

write.csv(hhcomp_tenure, "C:/Families/Data_for_github/Census_hhcomp_tenure_agg.csv")

#--------------------------------------------------------------------------------------

### Start here to make the plot

catcolour2 = c('#ee266d', '#6da7de')

# read in data
hhcomp_tenure <- read.csv("C:/Families/Data_for_github/Census_hhcomp_tenure_agg.csv") %>%
  data.frame

## making the plot

svg(filename = "C:/Families/Charts_for_github/33_C_tenure_with_dc.svg", 
     width = 8, height = 6)

ggplot(data = hhcomp_tenure, aes(x = ten_common, y = pp_diff, fill = inner_outer)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.text.x = element_text(color = "black", family = "Arial",
                                   size = 14, angle=0, vjust=0, hjust=0)) + 
  theme(axis.text.y = element_text(color = "black", family = "Arial",
                                   size = 14, angle=0, vjust = -0.5, hjust=0)) + 
  theme(legend.text = element_text(color = "black", family = "Arial",
                                   size = 14)) + 
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = "")) + 
  scale_fill_manual(values = bar_catcols) + 
  coord_flip() 

dev.off()


#-------------------------------------------------------------------------------------

# 8. For chart 3_3D plotting household composition by NS-SEC of Household Representative Person

#----------------------------------------------------------------------------

# This code is provided for information only

## Go to line c.#580 for code to create the plots  

  ## aggregate the NS-SEC categories. (1) add intermediate + lower supervisory, (2) routine and semi-routine, (3) never worked and students (not because they are really similar. Because they are low in value and aren't really important)
new_cats <- c("1. Higher managerial & professional", "2. Lower managerial & professional",
              "3&5. Intermediate, lower supervisory, technical", "4. Small employers and own account workers",
              "3&5. Intermediate, lower supervisory, technical", "6&7. Semi-routine and routine", 
              "6&7. Semi-routine and routine", "8&15. Never worked, long-term unemployed, students",
              "8&15. Never worked, long-term unemployed, students")

hhcomp_nssec$nssec_common_agg <- rep(new_cats, 2)

hhcomp_nssec_agg <- hhcomp_nssec[, .(value_21 = sum(value_21), 
                                     value_11 = sum(value_11), 
                                     value_01 = sum(value_01)),
                                 by = list(inner_outer, hhcomp_common, nssec_common_agg)]

  ## calculating percentage point difference
hhcomp_nssec_agg$perc_21 <-  hhcomp_nssec_agg[, .(perc_21 = 100*(value_21/sum(value_21))),
                                              by = list(inner_outer)]$perc_21

hhcomp_nssec_agg$perc_11 <-  hhcomp_nssec_agg[, .(perc_11 = 100*(value_11/sum(value_11))),
                                              by = list(inner_outer)]$perc_11

hhcomp_nssec_agg$pp_diff <- round(hhcomp_nssec_agg$perc_21 - hhcomp_nssec_agg$perc_11, 1)


# Create a datafile to save on github

write.csv(hhcomp_nssec_agg, "C:/Families/Data_for_github/Census_hhcomp_nssec_agg.csv")

#--------------------------------------------------------------------------------------

### Start here to make the plot

catcolour2 = c('#ee266d', '#6da7de')

# read in data
hhcomp_nssec_agg <- read.csv("C:/Families/Data_for_github/Census_hhcomp_nssec_agg.csv") %>%
  data.frame

  ## making the plot

hhcomp_nssec_agg$nssec_common_agg <- factor(hhcomp_nssec_agg$nssec_common_agg,
                                            levels = rev(unique(hhcomp_nssec_agg$nssec_common_agg))) # turning into a factor and reversing the order, so that it will plot in the right order


bar_labs <- rev(c("1. Higher managerial & professional",
              "2. Lower managerial & professional",
              "3&5. Intermediate, lower supervisory,\ntechnical",
              "4. Small employers and \nown account workers",
              "6&7. Semi-routine and routine",
              "8&15. Never worked, long-term \nunemployed, students"))

svg(filename = "C:/Families/Charts_for_github/33_D_nssec_with_dc.svg", 
     width = 8, height = 6) 

ggplot(data = hhcomp_nssec_agg, aes(x = nssec_common_agg, y = pp_diff, fill = inner_outer)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.text.x = element_text(color = "black", family = "Arial",
                                   size = 14, angle=0, vjust=0, hjust=0)) + 
  theme(axis.text.y = element_text(color = "black", family = "Arial",
                                   size = 14, angle=0, vjust=-0.2, hjust=0)) + 
  theme(legend.text = element_text(color = "black", family = "Arial",
                                   size = 14)) + 
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = ""), 
                     breaks = c(-4, 0, 4, 8)) + 
  scale_x_discrete(labels = bar_labs) + 
  scale_fill_manual(values = bar_catcols) + 
  coord_flip() 

dev.off() 


warnings()

