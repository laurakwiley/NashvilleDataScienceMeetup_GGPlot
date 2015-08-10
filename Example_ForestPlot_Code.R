library(ggplot2)
library(readr)
library(magrittr)
library(dplyr)

data <- read_csv(file = "Example_ForestPlot_Data.csv")

## Let's try a generic forest plot with all of the data
## Note that geom_point range requires a mapping for the x-axis, a value for the point on y, then a ymin and y max value
ggplot(data) + 
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci))

## But in forest plots in genetics we have the predictor on the y-axis beta along the x-axis
## We need to change the coordinate space!
ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci)) +
  coord_flip()

## To be able to compare models I need to be able to see them in different colors
## Use the color mapping/aesthetic!
ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model)) +
  coord_flip()

## I really don't want each line layered on each other. 
## Can control using "position" argument which allows to set how wide apart they are
ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model), position = position_dodge(width = 1)) +
  coord_flip()

## Ok this makes it easier to see, but now looks lke a ton of tests rather than groups of 3 models for each SNP
## Make the width a bit less to see those discrete bins
ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model), position = position_dodge(width = 0.5)) +
  coord_flip()

## What if you are printing this graph on a b/w printer?
## Bring in shape scale!
ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model, shape = model), position = position_dodge(width = 0.5)) +
  coord_flip()

## What else would be helpful for us to know about these models? 
## In comparing these methods it would be helpful to check type 1/type 2 error
## Let's use pval as an alpha shading
ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model, shape = model, alpha = pval), position = position_dodge(width = 0.5)) +
  coord_flip()

## Well that's not quite what we want, we really want to show models that are significant and those that aren't
## Need a new variable!
data %<>% mutate(sig = ifelse(pval>(0.05/12), yes = FALSE, no = TRUE))

ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model, shape = model, alpha = sig), position = position_dodge(width = 0.5)) +
  coord_flip()

## I really would prefer the alphas to not be quite that transparent.
## Use scale options
ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model, shape = model, alpha = sig), position = position_dodge(width = 0.5)) +
  scale_alpha_manual(values = c(0.5,1)) +
  coord_flip()

## Now lets add a line to make it more clear those associations that have a risk vs protective odds ratio
## We can use geom_hline or geom_vline for horizontal and vertical lines respectively
## Rember that we have the coord_flip so although we want the vertical line, we need to use geom_hline
ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model, shape = model, alpha = sig), position = position_dodge(width = 0.5)) +
  geom_hline(y = 1, lty = 2) +
  scale_alpha_manual(values = c(0.5,1)) +
  coord_flip()

## That seems a bit stark, let's change it to a dashed line
ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model, shape = model, alpha = sig), position = position_dodge(width = 0.5)) +
  geom_hline(y = 1, lty = 2) +
  scale_alpha_manual(values = c(0.5,1)) +
  coord_flip()


## I want to make these even more clear to the viewer. Let's make the non-significant models have an open shape
## To do that we need to change the shapes to those that have a fill color and add the fill aesthetic
## Use the fill aesthetic
ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model, shape = model, alpha = sig, fill = sig), position = position_dodge(width = 0.5)) +
  geom_hline(y = 1, lty = 2) +
  scale_shape_manual(values = c(23,22,24)) +
  scale_alpha_manual(values = c(0.5,1)) +
  coord_flip()

## This gives us the option of two colors one for significant models and one for non-significant models
## But I really would like to have the fill in the significant models to be the same as color of the bar
## The only way to do this is change the underlying data structure to call them different variables in "sig"
## Will will need to change alpha to account for these values
data %<>% mutate(sig = factor(ifelse(sig == FALSE, yes = 0, no = ifelse(model=="ICD",yes = 1,no = ifelse(model=="NLP",yes = 2,no = 3))),levels =  c(0,1,2,3)))

ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model, shape = model, alpha = sig, fill = sig), position = position_dodge(width = 0.5)) +
  geom_hline(y = 1, lty = 2) +
  scale_alpha_manual(values = c(0.5,1,1,1)) +
  scale_shape_manual(values = c(23,22,24)) +
  scale_fill_manual(values = c("white","red","green","blue")) +
  coord_flip()

## Let's customize our colors to make this easier on color blind folks
ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model, shape = model, alpha = sig, fill = sig), position = position_dodge(width = 0.5)) +
  geom_hline(y = 1, lty = 2) +
  scale_alpha_manual(values = c(0.5,1,1,1)) +
  scale_shape_manual(values = c(23,22,24)) +
  scale_fill_manual(values = c("white","#000000","#e79f00","#0072B2")) +
  scale_color_manual(values = c("#000000","#e79f00","#0072B2")) +
  coord_flip()

## These are replications of known associations. It would be great to have a sense of the original OR of the significant models
## We can add a new layer to the figure
original_assocs <- tbl_df(data.frame(snp = c("rs1051730","rs931794","rs748404"), odds_ratio = c(1.35,1.39,0.87)))

ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model, shape = model, alpha = sig, fill = sig), position = position_dodge(width = 0.5)) +
  geom_hline(y = 1, lty = 2) +
  geom_point(data = original_assocs, aes(x=snp, y = odds_ratio),color = "red") +
  scale_alpha_manual(values = c(0.5,1,1,1)) +
  scale_shape_manual(values = c(23,22,24)) +
  scale_fill_manual(values = c("white","#000000","#e79f00","#0072B2")) +
  scale_color_manual(values = c("#000000","#e79f00","#0072B2")) +
  coord_flip()

## Ok the data part of our figure looks great. Let's fix the guide portion
## Conceptually we have two make types of information we are trying to convey:
## 1. Which adjustment method we used
## 2. Whether the model was significant
## Model looks good, thought I would like it to be in the same order as on the plot. We can fix that using the breaks option
## We can also clean up the labels using "labels" and "name"
ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model, shape = model, alpha = sig, fill = sig), position = position_dodge(width = 0.5)) +
  geom_hline(y = 1, lty = 2) +
  geom_point(data = original_assocs, aes(x=snp, y = odds_ratio),color = "red") +
  scale_alpha_manual(values = c(0.5,1,1,1)) +
  scale_fill_manual(values = c("white","#000000","#e79f00","#0072B2")) +
  scale_shape_manual(values = c(23,22,24), breaks = c("No Correction","NLP","ICD"), labels = c("Unadjusted","NLP","ICD"), name = "Adjustment Type") +
  scale_color_manual(values = c("#000000","#e79f00","#0072B2"), breaks = c("No Correction","NLP","ICD"),labels = c("Unadjusted","NLP","ICD"), name = "Adjustment Type") +
  coord_flip()

## Sig is kind of a train wreck with four levels, 3 of which are identical (except for fill color which does not appear in the guide)
## We can collapse them using breaks and do the same labeling as above
ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model, shape = model, alpha = sig, fill = sig), position = position_dodge(width = 0.5)) +
  geom_hline(y = 1, lty = 2) +
  geom_point(data = original_assocs, aes(x=snp, y = odds_ratio),color = "red") +
  scale_alpha_manual(values = c(0.5,1,1,1), breaks = c(0,1), labels = c("Not Significant","Significant (p<0.004)"), name = "Genetic Association") +
  scale_fill_manual(values = c("white","#000000","#e79f00","#0072B2"), breaks = c("white","#000000"), labels = c("Not Significant","Significant(p<0.004")) +
  scale_shape_manual(values = c(23,22,24), breaks = c("No Correction","NLP","ICD"), labels = c("Unadjusted","NLP","ICD"), name = "Adjustment Type") +
  scale_color_manual(values = c("#000000","#e79f00","#0072B2"), breaks = c("No Correction","NLP","ICD"),labels = c("Unadjusted","NLP","ICD"), name = "Adjustment Type") +
  coord_flip()


## Finally I want to force some of the aesthetics in the Guide
## I want all of the "Adjustment type" Shapes to be filled in
## I want the "Genetic Association" shapes to be filled/empty and be a different shape.
## We can use guides() to accomplish this
ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model, shape = model, alpha = sig, fill = sig), position = position_dodge(width = 0.5)) +
  geom_hline(y = 1, lty = 2) +
  geom_point(data = original_assocs, aes(x=snp, y = odds_ratio),color = "red", size = I(3)) +
  scale_alpha_manual(values = c(0.5,1,1,1), breaks = c(0,1), labels = c("Not Significant","Significant (p<0.004)"), name = "Genetic Association") +
  scale_fill_manual(values = c("white","#000000","#e79f00","#0072B2"), breaks = c("white","#000000"), labels = c("Not Significant","Significant(p<0.004")) +
  scale_shape_manual(values = c(23,22,24), breaks = c("No Correction","NLP","ICD"), labels = c("Unadjusted","NLP","ICD"), name = "Adjustment Type") +
  scale_color_manual(values = c("#000000","#e79f00","#0072B2"), breaks = c("No Correction","NLP","ICD"),labels = c("Unadjusted","NLP","ICD"), name = "Adjustment Type") +
  coord_flip() +
  guides(color = guide_legend(override.aes = list(fill = c("#0072B2","#e79f00","#000000"))),
         alpha = guide_legend(override.aes = list(shape = 22, fill = c("white","#000000"), alpha = c(0.5,1))))

# Now let's clean up axis labels and add a title
ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model, shape = model, alpha = sig, fill = sig), position = position_dodge(width = 0.5)) +
  geom_hline(y = 1, lty = 2) +
  geom_point(data = original_assocs, aes(x=snp, y = odds_ratio),color = "red", size = I(3)) +
  scale_alpha_manual(values = c(0.5,1,1,1), breaks = c(0,1), labels = c("Not Significant","Significant (p<0.004)"), name = "Genetic Association") +
  scale_fill_manual(values = c("white","#000000","#e79f00","#0072B2"), breaks = c("white","#000000"), labels = c("Not Significant","Significant(p<0.004")) +
  scale_shape_manual(values = c(23,22,24), breaks = c("No Correction","NLP","ICD"), labels = c("Unadjusted","NLP","ICD"), name = "Adjustment Type") +
  scale_color_manual(values = c("#000000","#e79f00","#0072B2"), breaks = c("No Correction","NLP","ICD"),labels = c("Unadjusted","NLP","ICD"), name = "Adjustment Type") +
  coord_flip() +
  guides(color = guide_legend(override.aes = list(fill = c("#0072B2","#e79f00","#000000"))),
         alpha = guide_legend(override.aes = list(shape = 22, fill = c("white","#000000"), alpha = c(0.5,1)))) +
  ylab("Odds Ratio") +
  xlab("") +
  ggtitle("Effect of Smoking Detection Algorithm on Lung Cancer Genetic Associations")

# Dislike the grey background?
# Now let's clean up axis labels and add a title
ggplot(data) +
  geom_pointrange(aes(x = snp, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = model, shape = model, alpha = sig, fill = sig), position = position_dodge(width = 0.5)) +
  geom_hline(y = 1, lty = 2) +
  geom_point(data = original_assocs, aes(x=snp, y = odds_ratio),color = "red", size = I(3)) +
  scale_alpha_manual(values = c(0.5,1,1,1), breaks = c(0,1), labels = c("Not Significant","Significant (p<0.004)"), name = "Genetic Association") +
  scale_fill_manual(values = c("white","#000000","#e79f00","#0072B2"), breaks = c("white","#000000"), labels = c("Not Significant","Significant(p<0.004")) +
  scale_shape_manual(values = c(23,22,24), breaks = c("No Correction","NLP","ICD"), labels = c("Unadjusted","NLP","ICD"), name = "Adjustment Type") +
  scale_color_manual(values = c("#000000","#e79f00","#0072B2"), breaks = c("No Correction","NLP","ICD"),labels = c("Unadjusted","NLP","ICD"), name = "Adjustment Type") +
  coord_flip() +
  guides(color = guide_legend(override.aes = list(fill = c("#0072B2","#e79f00","#000000"))),
         alpha = guide_legend(override.aes = list(shape = 22, fill = c("white","#000000"), alpha = c(0.5,1)))) +
  ylab("Odds Ratio") +
  xlab("") +
  ggtitle("Effect of Smoking Detection Algorithm on Lung Cancer Genetic Associations") +
  theme_bw()


