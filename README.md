# K-RandomForest
Based on the Paper- M. Bicego, "K-Random Forests: a K-means style algorithm for Random Forest clustering," 2019 International Joint Conference on Neural Networks (IJCNN), 2019, pp. 1-8, doi: 10.1109/IJCNN.2019.8851820. 
You can download the paper from https://ieeexplore.ieee.org/document/8851820

Random Forest, first by Breiman represent one of the most popular approach for Pattern Recognition. This technique uses a group of decision trees, which realizes a hierarchical splitting of a feature space. Random Forests are based on the “divide and conquer” principle: in their classic version, different sets of random samples are extracted from the training set, each one used to build a decision tree.

Generally speaking, Random Forest approaches have been almost always studied as regression or classification tools: in these contexts they represent state-of-the-art machines, able to compete with the most effective and established approaches.

Clustering approach involves grouping of the data points into a specific group or so called clusters, if the data points have similar properties between them, they tend to fall in the same cluster, else in different clusters. 

In the clustering context, the most used approach involve single Random Forest rather than a group of forests.

The paper provided to us proposed a new novel Approach namely K Random Forest Method. This Approach is based on Isolation Tree and for each cluster, an Isolation Tree is made.

The results after implementing them and comparing to alternative clustering techniques such as ProxRF comes out to be very promising.
