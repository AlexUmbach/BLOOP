# What is BLOOP?
BLOOP is an R and RShiny based extension to the AXIOME3-GUI pipeline. It is designed to allow users to quickly and efficiently visualize their amplicon datasets and produce near-publication-ready figures. Importantly, BLOOP provides users with a high level of interactivity and customization during figure generation. By seemlessly combining amplicon datasets with provided metadata, users can more readily explore and visualize microbial community compositional patterns.

# Usage summary
Because BLOOP is built using RShiny, users can simply clone this repository to any location on their local machine and execute it by running the server.R script in RStudio (after installing the appropriate packages, of course)

Users require two files: an ASV table and an associated metadata table, both in a .txt or .tsv format. It is <b>critical</b> that the column names match the examples below <i>exactly</i>. Specifically, "Feature ID", "Consensus.Lineage", and "ReprSequence" in the ASV table, and "SampleName" in the metadata table. Note that the metadata "SampleName" column contains sample names that match the sample names in the ASV table. If you are having errors using BLOOP, this is the first place to check. 

<b>ASV table</b>

![ASV_example](https://github.com/AlexUmbach/BLOOP/assets/56092913/9e7d211b-d2c7-457d-9edd-d4a812d6f1bb)

<b>Metadata table</b>

![meta_example](https://github.com/AlexUmbach/BLOOP/assets/56092913/939998f0-e352-44d6-ba45-2856e552d027)


