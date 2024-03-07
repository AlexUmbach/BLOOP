# What is AOViz?
AOViz is an R and RShiny based app for visualizing amplicon sequence data. It is designed to allow users to quickly and efficiently visualize their amplicon datasets and produce near-publication-ready figures. Importantly, AOViz provides users with a high level of interactivity and customization during figure generation. By seemlessly combining amplicon datasets with provided metadata, users can more readily explore and visualize microbial community compositional patterns. AOViz is, and will continue to be, a work in progress. The main branch is stable and should do everything it says it will, but if you encounter any errors please let me know and I'll do my best to address them in a timely manner. 

# Usage summary
Because AOViz is built using RShiny, users can simply clone this repository to any location on their local machine and execute it by running the server.R script in RStudio (after installing the appropriate packages, of course)

Users require two files: an ASV table and an associated metadata table, both in a .txt or .tsv format. It is <b>critical</b> that the column names match the examples below <i>exactly</i>. Specifically, "Feature ID", "Consensus.Lineage", and "ReprSequence" in the ASV table, and "SampleName" in the metadata table. Note that the metadata "SampleName" column contains sample names that match the sample names in the ASV table. If you are having errors using AOViz, this is the first place to check. 

<b>ASV table</b>
![ASV_example](https://github.com/AlexUmbach/AOViz/assets/56092913/4af226dc-c0b0-4f96-a7bd-8ce8fac773f2)

<b>Metadata table</b>

![meta_example](https://github.com/AlexUmbach/AOViz/assets/56092913/92f7a906-9470-4a62-9d37-ee022e306062)

# About the author
Hello! I am a biology PhD student studying the microbial ecology of aquarium biofilters and wastewater treatment systems -- the two are closely related, I promise. AOViz represents a columination of my visualization scripts to date, largely influenced by the types of visualizations often produced in the Neufeld lab. I will be the first to admit: I am by <i>no</i> means an expert in R, but I am confident saying that I have a good working knowledge of the R environment. AOViz came out of personal dissatisfaction with how tedious R scripts are to quickly modify, particularly when wanting to change thematic or visual elements, and was developed in parallel to the AXIOME3-GUI extension designed specifically to address this issue. Since AXIOME3's release, I had a personal interest in refining some of it's elements and making additions useful to myself and others in our lab. I hope you enjoy the AOViz experience as much as I enjoyed developing it. 


