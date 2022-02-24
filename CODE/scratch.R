The purpose of this script is to convert lookup tables from cNORM, that have been
modified by a project director (e.g., hand-smoothing  has been applied) into print-format
lookup tables. Because there is a step of human-intervention outside cNORM, the process
cannot be automated as part of the basic cNORM workflow.

The input tables embody a hierarchy of data in which there is one table per test, and each
of these tables holds the lookup columns for all age strats. In addition, you lookup raw scores
in the left-ward column and read to the right to find the associated standard scores. here
is the head of one such table.

# ```
# # A tibble: 34 × 10
# raw `5.0-5.3` `5.4-5.7` `5.8-5.11` `6.0-6.5` `6.6-6.11` `7.0-7.5` `7.6-7.11` `8.0-8.5`
# <dbl>     <dbl>     <dbl>      <dbl>     <dbl>      <dbl>     <dbl>      <dbl>     <dbl>
#   1     0        66        62         58        55         50        47         44        40
# 2     1        70        66         62        58         53        50         47        44
# 3     2        74        70         66        61         57        53         49        46
# 4     3        77        74         70        65         60        56         53        49
# 5     4        80        77         73        68         63        58         55        51
# 6     5        83        80         75        70         65        60         56        53
# 7     6        86        82         77        72         67        62         58        55
# 8     7        89        84         79        74         68        64         60        56
# 9     8        91        86         81        76         70        65         61        57
# 10     9        93        88         83        77         72        67         62        59
# # … with 24 more rows, and 1 more variable: 8.6-9.3 <dbl>
# 
# ```

# The desired output, on the other hand, reverses the two hierarchies described for the input
# tables. In the output tables, there is one table per age strat, and each of these tables
# holds the lookup columns for all tests. On the output tables, you look up raw scores
# in the rightward columns and read to the left to find the associated standard scores. here
# is the head of one such output table.

# ```
# # A tibble: 91 × 8
# perc     ss `LSK-E` `LSW-E` `RHY-E` `RLN-E` `SEG-E` `SPW-E`
# <chr> <dbl> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
#   1 98      130 27--33  25--38  21--30  73--120 21--25  20--32 
# 2 97      129 -       24      -       72      20      19     
# 3 97      128 26      -       20      71      -       -      
#   4 96      127 -       23      -       70      -       -      
#   5 96      126 -       -       19      68--69  19      18     
# 6 95      125 25      22      -       67      -       -      
#   7 95      124 -       -       18      66      -       -      
#   8 94      123 24      21      17      65      18      17     
# 9 93      122 -       -       -       64      -       -      
#   10 92      121 23      20      16      63      -       -      
#   # … with 81 more rows
# ```

# What the script accomplishes is a transformation of the essential hierarchies
# of the data structures of the input, and a reformation of the cells
# themselves. Recall that the lookup relationship between raw scores and
# standard scores is many-to-one. That is, a range of raw scores (multiple raw
# scores) can  map onto a single standard score, but each raw score on its own
# maps onto one and only one standard score. 

########## NEXT TWO PARAGRAPHS ARE OPTIMIZED TEXT IMPORTED FROM FINISHED DOCUMENTATION.

# The lookup relationship between raw and T-scores is many-to-one, meaning that
# each value of raw maps onto one and only one value of T, but each value of T
# may map onto more than one value of raw.

# Print format: The left-most column contains all possible values of T
# (reflecting any truncation of the T-score distribution), sorted descending.
# Rightward columns contain corresponding raw scores for each scale. Reflecting
# the many-to-one lookup relationship, some of the rightward cells contain
# single raw scores, while others contain a range of raw scores.

###########

# Thus, the initial transformation of
# the work flow is to collapse the incremental sequence of raw scores so that
# each standard score occupies only a single row in the output. Prior to this
# transformation, there could be duplicate standard score rows. To preserve the
# many-to-one raw to SS relationship, any ss row may map onto a range of raw
# scores (e.g., 88-91)

# The second transformation is to invert the hierarchy of the inputs, in which each test is a 
# container for all age strats. This hierarchy is reversed in the output, such that after
# transformation, each age strat is a container for all tests.

# In order to track the transformation of the hierarchy through the code, we
# name objects with the suffix "ta" (test>>age) when they express the input hierarchy,
# and we use the suffix "at" (age>>test) to label the output hierarchy, and "flat" to
# denote an absence of either hierarchy.
