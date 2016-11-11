This panel is provided to allow view and download of statistics related to differential expression.

p and adjusted p values are calculated as part of analysis procedures prior to creation of this resource. Fold changes are calculated dynamically depending on the choice of expression matrix. 

#### Controls

Controls are provided to define which comprisons should be displayed, and which filters should be supplied. p value thresholds only have an effect where values are provided. The value of the fold change will depend on the way in which a group-wise average is calculated, mean by default.

#### Dynamic filters

You can build up complex queries using the options under 'Contrasts'. You can specify one set of filters for one set of contrasts, and another set for another set, and combine by either the intersection (default) or the union of the resulting feature sets. This allows you to say things like "give me features that are highly significantly up in contrast A, but don't change at all in contrast B". 
