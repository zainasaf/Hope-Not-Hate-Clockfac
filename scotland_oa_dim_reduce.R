## Scotland Output Area PCA 

library(tidyverse)
library(FactoMineR)
library(corrr)
library(yaml)
library(readr)
library(here)

#source("src/theme_fd.R")

#### Load data ####


OA21CD_census_2021_csv <- read_csv(here("data", "OA21CD_census_2021.csv"))

top_variables <- read_csv(here("data", "top_variables.csv"))


  
raw_data <-  OA21CD_census_2021_csv %>%
  rename(output_area = OA21CD) |>
  print()
# raw_data <- read_csv("data/census_data/OA21CD_census_2021.csv.gz") |>  # N = 46,363 OA

#Predefined top variables
#These were made with a run where variables weren't filtered, then top 30 
#by contribution were picked - these lines are commented out below
#top_variables <- read_csv("data/top_variables.csv")

#N of variables
length(unique(names(raw_data |> select(-output_area))))

#NO NA values
map_dbl(raw_data, \(x) sum(is.na(x)))

#look at some distributions, if we fancy
raw_data |>
  select(contains("national_identity")) |>
  pivot_longer(everything()) |>
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = .2) +
  scale_x_log10()

#### PCA ####

#Select data
d_for_pca <- raw_data 

if (any(is.na(d_for_pca))) {
  warning("d_for_pca contains missing values!")
}

#Look at correlations, if desired
# d_for_pca |>
#   correlate() |>
#   rearrange() |>
#   #rplot()
#    autoplot(triangular = "full")
#   # rplot(colors = c("red", "white", "blue"))

#Select variables
pca_matrix <- d_for_pca |>
  select(where(is.numeric)) |>
  select(all_of(top_variables$variable)) #only the pre-defined top 30 list

#Fit model
m_pca_pre <- PCA(pca_matrix, ncp = 2, graph = F)

#Look at variable contributions
summary(m_pca_pre)
d_eig <- as_tibble(m_pca_pre$eig, rownames = "comp") |> print() # plot eigenvalues to viz how many dims to use

#Factor Loadings (write to file)
corr_table <- round(m_pca_pre$var$cor, digits = 2) |>
  as_tibble(rownames = "variable") |>
  #write_csv("data/factor_loadings.csv") |>
  print(n = Inf)

corr_dim_1 <- corr_table |> 
  arrange(desc(Dim.1)) |> 
  print(n = 50)

corr_dim_2 <- corr_table |> 
  arrange(desc(Dim.2)) |> 
  print(n = 50)

#Correlations, plot
## ask where function scale_fill_continuous_diverging" comes from 

m_pca_pre$var$cor |>
  as_tibble(rownames = "var") |>
  #filter(abs(Dim.1) > 0.4) |> # filter out low explanatory vars in Dim.1
  filter(abs(Dim.2) > 0.4) |> # filter out low explanatory vars in Dim.2
  mutate(var = fct_reorder(var, Dim.1)) |> 
  pivot_longer(-c(var)) |>
  ggplot(aes(x = name, y = var, fill = value)) +
  geom_tile() +
  scale_fill_continuous_diverging(palette = "tropic", rev = T, name = "Correlation")

m_pca_pre$var$cor  |>
  as_tibble(rownames = "var") |>
  write_csv("data/factor_loadings.csv")


#Contribution table
contrib_table <- as_tibble(round(m_pca_pre$var$contrib, digits = 1), rownames = "var") |> print()  # e.g. how much does each Q contribute to each dimension

contrib_dim_1 <- contrib_table |> 
  arrange(desc(Dim.1)) |> 
  print(n = 50)

contrib_dim_2 <- contrib_table |> 
  arrange(desc(Dim.2)) |> 
  print(n = 50)

## Cos2 Table

m_pca_pre$var$cos2 |>
  as_tibble(rownames = "var") |>
  arrange(desc(Dim.1)) |> 
  print(n = 50)

m_pca_pre$var$cos2 |>
  as_tibble(rownames = "var") |>
  arrange(desc(Dim.2)) |> 
  print(n = 50)
  

### Identify top variables - these can be fed back in at the top ####

#cors <- as_tibble(m_pca_pre$var$cor, rownames = "var")
### Get lists of top variables 
#top_dim1 <- cors |>
#  arrange(-abs(Dim.1)) |>
#  with(head(var, 15))
#top_dim2 <- cors |>
#  arrange(-abs(Dim.2)) |>
#  with(head(var, 15))
#
#enframe(c(top_dim1, top_dim2)) |>
#  select(variable = value) |>
#  write_csv("data/top_variables.csv") 

#### Write to file ####

# rename PCA dimensions
pca_dimensions <- tribble(~dim, ~ code, 
                          "Dim.1", "dimension_1", 
                          "Dim.2", "dimension_2")

#add into dataframe
cluster_data <- m_pca_pre |>
  with(as_tibble(ind$coord))  |>
  rename_with(\(x) pca_dimensions$code[x == pca_dimensions$dim]) |> 
  bind_cols(select(d_for_pca, output_area)) |> 
  left_join(d_for_pca) |>
  print()

cluster_data |>
  transmute(output_area, 
         low_diversity = dimension_2,
         security = -dimension_1) |>
  write_csv("data/Scotland_OA_2dim_170225.csv")
