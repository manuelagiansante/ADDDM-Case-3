## Further investigation on `FA9_119`

We further confirm that the 9 factor model without `Im9` and `Im11` would be superfluous by
plotting the Scree graph and taking a closer look at the model eigenvalues:
  
  ```{r}
# Scree Plot 9_119
ggplot(mapping = aes(x = 1:length(FA9_119$values),
                     y = FA9_119$values,
                     color = (FA9_119$values >= 1))) +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_classic() +
  labs(title = "Scree plot of FA9_119",
       x = "Factor Number",
       y = "Eigenvalue",
       color = "Eigenvalue >= 1")
```

Above we have plotted out a Scree plot, which serves the purpose of
determining the number of factors to retain in the factor axis analysis.
Factors eigenvalues are ordered in a descending order, the method is
quite subjective but it might provide provide further insight.The scree
plot for the model `FA9_119` suggests, by the elbow rule to cut off at
factor number 8, which is coherent, as we saw how no variable would load on
the ninth factor. This means that factor 9 explains no variation in the
variables, it brings no further aid in explaining the structure of our
data.

```{r}
# Print Variances and Eigenvalues
FA9_119$Vaccounted
t(FA9_119$values)
t(cumsum(FA9_119$e.values/ncol(df2)))
```

We notice that the proportion explained by factor 9 is extremely low.
This reinforces how the best option seems to be only retain 8 factors.
We have also printed out eigenvalues, which explain how much variance in
the original variables is explained by the factors. If we follow the
Kaiser-Guttman criterion, the suggested amount of factors to maintain is
6 when observing the eigenvalues. Now, this opens new space for
experimenting with new models with a different number of factors.


We briefly recap before moving on with the investigation:
  
-   Model FA8_119 reaches the highest cumulative variance we have seen until now;
-   We need to pay attention to the variables `Im8`, `Im15` and `Im19`;
-   Eigenvalues from FA9_119 actually suggest a 6 factors model.




=======================================================
  
```{r}
# Print Variances and Eigenvalues
FA8_119$Vaccounted
t(FA8_119$values)
t(cumsum(FA8_119$e.values/ncol(df2)))
```

By considering less factors the "elbow rule" turns even more
conservative, suggesting a four factor model. We test some models with
less than 8 factors to see if we can outdo the 0.763 explained variance
of FA8_119.


=================================================================
  
  
```{r}
FA8_1191519 <- fa(df2[, -c(9,11,15,19)], nfactors = 8,rotate="varimax", fm = "pa")
print(FA8_1191519$loadings, cutoff = 0.3, sort = TRUE)
```

The cumulative explained variance has increased to 0.771 by running a 8
factor model excluding Im9, Im11, Im15, Im19. What happens here is that
now Im16 (Professional appearance towards customer) now has a very close
to cut off loading on factor 1 (with Im3, Im4, Im5) which models
decoration and arrangement. All of these have to do with appearence from
a client point of view.

What we observe now that we are back considering 8 factors models, is
that Im8 still has a quite low loading.

#### Excluding Im19

We try running a model without the Im19 because it was the variable with
the lowest loading value in FA8_119.

```{r}
FA8_11919 <- fa(df2[, -c(9,11,19)], nfactors = 8,rotate="varimax", fm = "pa")
print(FA8_11919$loadings, cutoff = 0.3, sort = TRUE)

```

The cumulative variance falls in between FA8_911 and FA8_9111519.This
model for now we set aside. Im15 is split between three factor with very
low loadings, the question regard professional selection of brand.
Thematically maybe it explores an aspect of client perception that
should be investigated further.

#### Excluding Im15

```{r}
FA8_11915 <- fa(df2[, -c(9,11,15)], nfactors = 8,rotate="varimax", fm = "pa")
print(FA8_11915$loadings, cutoff = 0.3, sort = TRUE)

```

We exclude Im15 the explained variance by the model is in parity with
the "slimmer" model FA8_9111519. Im19 loads on two different factors 4
and 8. Factor 8 would form at the start as well when simply observing
correlations, it deals with professionalism. Both loadings on both
factors are below 0.7 which is not necessarily good.

Now at this stage our choice is between a model that sacrifices more
information and one that includes one more variable, but the over all
explained variance is almost the same.

```{r}
FA8_1191519$Vaccounted
FA8_11915$Vaccounted
```

In FA8_11915 factor 8 has a higher Proportion Var.

Communalities are the shared variability with the factors.

```{r}
FA8_1191519$communality
FA8_11915$communality
```

In the case of Im19 0.5841990 of its variability is explained by the
factor it is loading on (PA4 and PA8) in FA8_11915. In the FA8_1191519
communalities the lowest communality is the one of Im5, its loading on
factor 1 in the model 0.64 both values are below acceptability
thresholds, but removing Im5 from factor one would not make much sense
in terms of themes covered.

In FA8_11915 PA4 -\> decoration and arrangement PA8 -\> professional
appearance

```{r}
descriptions[c(16,19),]
```

Thematically it would makes sense to retain Im19 as a variable, given
that PA8 in FA8_11915 has a higher SS loading, and the actual cumulative
variance of the model is slightly higher than FA8_1191519. The loadings
of Im19 are low, we would have Im16 with low loadings as well, and the
two thematically should go together.

### Considering Im8

One last thing we have not considered up until now variable Im8, which
shows low loadings both in FA8_1191519 and in FA8_11915. There are a few
reasons why we are considering this now, first for clarity of the
reader. We have first decided between 8 and 9 factors, we tried the 6
factors route and finally we digged deeper on how to best select between
Im15 and Im19.

Moreover, when we decided to retain Im19 was because otherwise the Im16
loadings would be too low. With FA8_11915 we reach an acceptable
explained variance, yet Im8 continues to have low loadings on PA2 and
PA7.

```{r}
descriptions[8,2]
```

The matter covered by Im8 seems to both touch the food theme and the
French lifestyle thematic. It seems like this question blurs factors
domains, we run a model without to see if it is worth it keeping it in
our model.

```{r}
FA8_119158 <- fa(df2[, -c(8,9,11,15)], nfactors = 8,rotate="varimax", fm = "pa")
print(FA8_119158$loadings, cutoff = 0.3, sort = TRUE)
```

The resulting Cumulative explained variance is higher than FA8_1191519
and FA8_11915, all loadings are above 0.6. We conclude, our final choice
of model is FA8_119158.