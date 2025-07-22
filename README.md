# MoralDescColorMapStimuli
This repository supports the **Moral Decision-Making Resource Allocation** study conducted by Duke's *Bass Connections: Data Visualization with Cognitive Neuroscience* team. The project investigates how intuitive vs. unintuitive color encodings in choropleth maps influence moral judgments and resource allocation decisions.

## Study Overview
Participants view choropleth maps that vary in color palette structure and geographic representation, then make moral or policy decisions based on the mapped data. The goal is to understand how the perceptual qualities of color mappings influence interpretations of need, fairness, and moral salience.

## Evolution of Map Generation Approaches
### Old Map Generation (Exploratory)
Earlier iterations of the map stimuli were **randomized and exploratory** in nature:
- **Random positioning and sizing** of circular polygons.
- Inconsistent rotation and alignment across maps.
- High variability made it difficult to standardize map perception.
- Generated using scripts like `basemap_all_circles.R`, with each map logged in the `map_attributes` table.
While visually diverse, these maps introduced **uncontrolled visual noise** that could confound interpretation of spatial patterns and color-based inference.

### New Map Generation (Regimented & Controlled)
The updated approach prioritizes **experimental control and reproducibility**:
- **Uniform shapes and grid layout** of polygons ensure consistent geography across maps.
- **Controlled rotation, spacing, and labeling**, making maps comparable across color conditions.
- Maps generated using `label_positions.R`, which enforces constraints on shape placement, rotation, and (dis)similarity.
This shift enables **cleaner experimental manipulations** and more reliable attribution of decision-making differences to color encoding — not arbitrary spatial variation.

## Example Stimuli (IRB-Approved and Used) 
<img width="1000" height="800" alt="intuitive_blues_8642" src="https://github.com/user-attachments/assets/ae577487-3887-4203-94a8-47c33b777c70" />

##  Repository Structure (Briefly)
- `curr_map_scripts/`: Finalized, standardized map generation code (used in the experiment).
- `old_map_scripts/`: Previous versions of randomized stimuli generation.
- `analysis_code/`: Scripts for pilot and visual experiment analysis, including data cleaning and regression.
- `result_visualizations/`: Visualizations from preliminary analyses.
