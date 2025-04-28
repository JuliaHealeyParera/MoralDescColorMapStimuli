# MoralDescColorMapStimuli

This is the study repository for Duke's Bass Connections Data Visualization with Cognitive Neuroscience Team's Moral Decision Making Resource Allocation study.

This study sought to investigate the effects of using intuitive vs. unintuitive mappings of diverging and sequential color palettes on moral decision-making as it relates to resource allocation decisions made using choropleth maps.

The repository is organized as follows:
1. curr_map_scripts: Scripts that generate the final maps used in the visualization portion of the experiment. These maps follow much more regimented requirements in terms of polygon shapes, rotations, and (dis)similarity between maps. **label_positions.R** was the main script used for map generation. blank_basemap was used for previous iterations and for creating the blank example version of the map that is used in the experiment's scenario explanation.
2. old_map_scripts: Scripts that were formerly used in generating previous iterations of the stimuli. Inside are various old scripts which may contain useful base code for polygon manipulation if, for some reason, less standardized maps are desired for future trials. **basemap_all_circles** is the most recent of these scripts, and it populates the map_attributes table (with individual identifiers for each map created) as well as the map_plots folder with the stimuli themselves.
3. result_visualizations: Folder with visualizations created in preliminary analyses.
4. analysis_code: Contains all analysis code for the pilot and visual experiments. Data cleaning code for both pilot and visual data is in visual_analysis folder. Preliminary analyses include investigating accuracy of participant interpretations of values between each color mapping and stepwise regressions and investigations of demographic variables and moral decision questionnaire responses as they relate to scenario responses.
