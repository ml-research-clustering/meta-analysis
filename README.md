# Towards Understanding Clustering Problems and Algorithms: an Instance Space Analysis (supplementary material)

<a href="https://doi.org/10.5281/zenodo.4647985"><img src="https://zenodo.org/badge/DOI/10.5281/zenodo.4647985.svg" alt="DOI"></a>

Various criteria and algorithms can be used for clustering, leading to very distinct outcomes and potential biases towards datasets with certain structures. More generally, the selection of the most effective algorithm to be applied for a given dataset, based on its characteristics, is a problem that has been largely studied in the field of meta-learning. Recent advances in the form of a new methodology known as Instance Space Analysis provide an opportunity to extend such meta-analyses to gain greater visual insights of the relationship between datasets’ characteristics and the performance of different algorithms. The aim of this study is to perform an Instance Space Analysis for the first time for clustering problems and algorithms. As a result, we are able to analyze the impact of the choice of the test instances employed, and the strengths and weaknesses of some popular clustering algorithms, for datasets with different structures. 

To generate the paper results, the following steps must be taken:

1) Generation of meta-features: main_features.R and meta_features_function.R;
2) Generation of the number of clusters, clusters, performance measures and ranking: script_performance.R, script_performance_stability.R, script_ranking.R and results_analysis.R;
3) Consolidation of meta-data;
4) Submission to MATILDA: https://matilda.unimelb.edu.au/matilda/login 

To facilitate the control of algorithms running, we divided the datasets in 7 groups: gaussian, ellipsoidal, small, medium, large, github and OpenML. This classification differs from the paper categorization (Gaussian, Ellipsoidal and Multiple Shapes).

You can also submit the meta-data provided here directly into MATILDA.

Obs.: Parameters used can be consulted in the reference: https://doi.org/10.3390/a14030095


