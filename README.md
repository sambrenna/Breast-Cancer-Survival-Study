# Breast Cancer: an Overview

Breast cancer stands prominently as one of the three most frequently diagnosed cancers worldwide. The significance of early detection increases the chances of successful treatment and favorable prognosis. Once diagnosed, surgical removal of the tumor often becomes the primary course of action. However, the advent of modern medical research has introduced targeted therapies, which aim to specifically treat individual tumor characteristics. These advances have led not only to more effective treatments but also to a shift towards personalized therapy. This personalization ensures that each patient receives a treatment tailored to their unique cancer profile, optimizing outcomes and reducing unnecessary treatment intensity.

# Description of the Analyses:

In this comprehensive project, our primary objective is to understand and characterize tumors, particularly focusing on their receptor statuses. By doing so, we will classify tumors into specific categories: Luminal, Triple Negative, and HER2 positive. Each of these classifications provides insights into the tumor's behavior, prognosis, and potential treatment strategies.

One of the key metrics we are introducing is the Lymph Node Ratio (LNR). The LNR is calculated by dividing the number of positive lymph nodes by the total nodes examined. Previous studies have indicated that LNR can be a significant prognostic factor for patients. For ease of analysis and interpretation, patients will be segmented into three distinct LNR groups: low (≤0.2), medium (0.21–0.65), and high (>0.65).

Additionally, we recognize the potential impact of menopause on tumor behavior and patient prognosis. Hence, we are categorizing patient age based on the onset of menopause into specific risk categories. This categorization into low, medium, and high risk will provide a more nuanced understanding of how menopause potentially interacts with tumor development and progression.

Our analytical approach will make use of the Chi-Squared Test, a statistical method designed to examine the relationships between categorical variables. In this analysis, we'll particularly focus on the relationships between LNR, Metastasis Tumor Stage, and Histological Type. Such an analysis will help us discern patterns, dependencies, and significant associations between these variables.

Furthermore, our study will utilize Survival Analysis techniques. This method will allow us to estimate the time it takes for a particular event of interest to occur. In our case, the events include factors such as Tumor Type, Tumor Stage, LNR, Margin Status, and Menopause Status-age. Through this analysis, we aim to understand better how each of these factors potentially influences the survival outcomes of patients.
