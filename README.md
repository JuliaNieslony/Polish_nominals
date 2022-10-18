# Polish nominals

With the help of distributional semantics, characteristics of the Polish noun system are explored with a focus on the relationship between case and number. 
At first, a generative model is produced to statistically yield semantic predictions about Polish inflections by reproducing the procedure of Luo, Chuang and Baayen who established the *JudiLing* implementation of Discriminative Learning in *julia*. The code script is to be found in the folder **generative_model**.
Subsequently, *t-distributed stochastic neighbor embedding*(t-SNE) is used to examine interparadigmatic relationships between inflected noun forms in semantic space via a clustering analysis. The semantic distributional embedding of the nouns is accessed through multidimensional *fasttext* vectors.
In the folder **t-SNE**, R code is to be found producing t-SNE modeling of
- the relationship of case and number also taking into consideration gender and *WordNet supersense* semantic categories in a general matter, in both number features comparatively and in the individual cases: **tSNE_all**,
- the impact of case on number directly accessed via number shift vectors, additionally taking into account gender and semantic categories: **shift_vectors** and **shift_vectors_individual_tSNE_plots**,
- average vectors of case and semantics: **average_vectors**.

The folder **interactive_plots** provides an overview over the findings produced via the t-SNE technique by comprising all cluster analysis graphics in the form of interactive plots. 
All findings produced in the study combined allow implications about the additive vector model and its applicability for the Polish nominal system.