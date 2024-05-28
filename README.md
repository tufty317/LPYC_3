# LPYC
Data and scripts for the charts in report London's Population of Young Children

This project contains scripts for creating the charts in the report “London’s population of young children – current and future”.
The report is available on the GLA Datastore, available here https://data.london.gov.uk/dataset/london-s-population-of-young-children---current-and-future

Links to the original data sources are provided in the report. This folder only includes the data (in the folder Data_for_Github) needed to create the charts, together with the R scripts (in the folder Scripts_for_Github).
The charts without titles are included in the folder Charts_for_Github. 

Some data files could not be uploaded as they are larger than Github's maximum permitted. These can be provided on request. 

For this and any other queries and comments please contact Veronica Tuffrey at the Greater London Authority
(Veronica.Tuffrey@london.gov.uk)

Number;	Script; Data file(s);

11_A;	Schools_boroughs_lines_primary_20240514.Rmd;	london_SF_primary_schools.csv;
                                                London_orNot_SF_primary_schools.csv
						
11_B1;	Schools_maps_20240515.Rmd;		Primary_schools_cats.csv

11_B2;	Schools_maps_20240515.Rmd;			Secondary_schools_cats.csv

21_A;	Births_20240514.Rmd;				London_ONS_births_pre2013.csv;
                                                London_MYE_births_zone_post2012.csv
						
21_B;	Births_and_pop_20240514.Rmd;			total_London_births_pop.csv

21_C;	TFR_20240514.Rmd;				EandW_TFR_allYears.csv;
                                                London_TFR_allYears_zone.csv	
						
21_D;	TFR_20240514.Rmd;				London_TFR_2021_boroughs.csv

21_E;	TFR_20240514.Rmd;				EandW_ASFR_allYears.csv;
                                                London_ASFR_allYears_zone.csv
						
21_F;	ASFR_20240514.Rmd;				London_ASFR_age_group.csv	

21_G;	ASFR_20240514.Rmd;				London_ASFR_3selected_years.csv	

21_H;	SMA_20240514.Rmd;				London_SMA_allYears_zone.csv

21_I1;	SMA_20240514.Rmd;				AbsChange_SMA.csv

21_I2;	SMA_20240514.Rmd;				London_SMA.csv	

21_J;	TFR_20240514.Rmd;				London_SMA_absChange.csv

21_K;	TFR_20240514.Rmd;				London_SMA_absChange.csv

21_L;	Population_analysis_withCOB_20240514.Rmd;	COB_BirthsChange_MothersUKnonUK.csv

21_M;	Population_analysis_withCOB_20240514.Rmd;	COB_BirthsChange_nonUK_MothersRegion.csv

21_N;	ASFR_COB_20240514.Rmd;			ASFR_Mothers_BornInorOutsideUK.csv

21_O;	Census_data_charts_20240514.Rmd;		Census_mothers_births_change.csv

22_A1;	Population_analysis_withOutsideLondon_20240514.Rmd;	change_2011to2021.csv

22_A2;	Population_analysis_withOutsideLondon_20240514.Rmd;	change_2001to2011.csv

22_B;	POP_DomMig_longit_20240516.Rmd;		london_domMig_longit_wide_corrected.csv	

22_C;	POP_DomMig_2022_AgeGroup_20240516.Rmd;	summarised_london_net_AgeandYear.csv

22_D;	POP_DomMig_2022_0-10_20240516.Rmd;		london_within_2002to2022.csv;
						london_origin_2002to2022.csv;
                                                london_destination_2002to2022.csv
THESE DATA FILES WERE TOO LARGE TO BE UPLOADED - please contact Veronica
						
22_E;	POP_DomMig_2022_AgeGroup_20240516.Rmd;	london_within_2022.csv;
                                                london_origin_2022.csv;	
                                                london_destination_2022.csv
						
23_A;	POP_IntMig_Trends_20240514.Rmd;		UK_Intnet.csv;
                                                UK_Intin.csv;
                                                UK_Intout.csv
						
23_B;	POP_IntMig_Trends_20240514.Rmd;		UK_EUIntnet.csv;
						UK_EUIntin.csv;
                                                UK_EUIntout.csv
						
23_C;	Population_analysis_withMYE_20240514.Rmd;	MYE22_London_CompsofChange.csv

23_D;	Census_data_charts_20240514.R;		Census_cob_age_sex_young.csv

31_A;	Population_analysis_withOutsideLondon_20240514.Rmd;	2011and2021_agezeroto10_change.csv	

31_B;	Population_analysis_withOutsideLondon_20240514.Rmd;	change_2011to2021_depChildren.csv

31_C;	Population_analysis_withOutsideLondon_20240514.Rmd;	HH_depChildren_2021.csv

32_A;	Population_analysis_byAgeGroup_20240514.Rmd;		London_pop_Agegrouped_1991.csv	

32_B;	Population_analysis_byAgeGroup_20240514.Rmd;	London_pop_Agegrouped_2011_ind.csv

32_C;	Census_data_charts_20240514.R;		Census_age_youngest.csv

33_A;	Census_data_charts_20240514.R;		Census_hhcomp_ethgroup_agg.csv

33_B;	Census_data_charts_20240514.R;		Census_ethgroup_withdep_children.csv;
						Census_ethgroup_withoutdep_children.csv
      
33_B1;	Census_data_charts_20240514.R;		Census_ethgroup_withdep_children.csv;
                                                Census_ethgroup_withoutdep_children.csv
						
33_B2;	Census_data_charts_20240514.R;		Census_ethgroup_withdep_children.csv;
						Census_ethgroup_withoutdep_children.csv
      
33_C;	Census_data_charts_20240514.R;		Census_hhcomp_tenure_agg.csv

33_D;	Census_data_charts_20240514.R;		Census_hhcomp_nssec_agg.csv

42_A;	Rents_costs_20240514.Rmd;			Rentals_Data_Regions.csv

42_B;	Purchase_costs_20240514.Rmd;		Purchase_regions_Data.csv

42_C1;	Rents_costs_20240514.Rmd;			Rentals_LAsEng_Data_cat.csv

42_C2;	Purchase_costs_20240514.Rmd;			Purchase_LAsEng_cat.csv	
