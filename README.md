# Machine_Learning_of_iKnife_Data_Project2

Background: A tumour-free surgical resection margin is a significant prognostic factor for many neoplasms. Scientists have developed the intelligent knife (iKnife) to assist surgeons by improving the accuracy of malignant tissue clearance. The iKnife uses mass spectrometry to measure the tissue’s metabolomic composition, i.e. information regarding the biochemical metabolic profile that facilitates tissue type differentiation between cancer and normal tissues types. Previous studies have examined smaller cohorts of samples within single tissue type. The single-tissue approach can be effectively used for distinguishing between normal and cancer tissue types. However, these metabolite changes may not be relevant in other tissue types. It was therefore hypothesised that a small subset of molecular features that were recognised across more than one type of tissue may be powerful diagnostic biomarkers and can be used to improve tumour identification. 

Materials and Methods: Mass spectrometry data from the iKnife was used to test the hypothesis. Initially, mass spectrometry data were normalised using Probabilistic Quotient Normalisation (PQN) and then analysed using a wide range of machine learning techniques in order to find the optimal algorithm using repeated stratified K-fold cross-validation for 10 iterations. The final predictive model was utilised to identify a group of informative features using recursive feature elimination for 100 iterations. Then, the Kruskal-Willis test was employed to emphasise whether differences in the median intensities of the selected features were statistically significant between cancer and normal tissues. 

Results: The top 80 molecular features (m/z) from 931 features, which were selected by RFE, improved the classification performance between normal and cancer tissues from 90.7% to 95.1%. Also, the eight statistically significant features were identified based on the criteria of FDR < 0.01 by the Kruskal-Willis test and selection rate by RFE more than 30%. 

Discussion: Since the tissue types were imbalanced, the samples needed to be carefully separated into training and testing datasets. Stratified K-Fold cross-validation was selected as it maintains the balance or ratio between dataset labels. Our results of the subset of molecular features have been documented in many previous publications. These informative molecular features could be potential diagnostic markers that could distinguish between normal and cancer tissues across a range of cancer types.