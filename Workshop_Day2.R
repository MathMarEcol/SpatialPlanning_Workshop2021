# load packages
library(prioritizr)
library(prioritizrdata)
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(mapview)
library(units)
library(scales)
library(assertthat)
library(gridExtra)
library(dplyr)

## Some of this data is built in to the Prioritizr package, but it is lower resolution so we use that in the data/ folder.
# load planning unit data
# data(tas_pu) # SpatialPolygonsDataFrame # If raw, use readOGR(filename)

# load conservation feature data
# data(tas_features) # RasterStack # If raw, use stack(filename)

albers <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

tas_pu <- readOGR("data/pu.shp")

tas_features <- stack("data/vegetation.tif")

proj4string(tas_pu) <- albers # There is a problem with projection so we re-add it here
proj4string(tas_features) <- albers # There is a problem with projection so we re-add it here

tas_pu$locked_out[1:500] <- FALSE # There is a problem later on so we remove some of the locked out areas to improve chance of a solution


tas_pu$locked_in <- as.logical(tas_pu$locked_in) # Convert to logical
tas_pu$locked_out <- as.logical(tas_pu$locked_out) # Convert to logical

# A function to plot the solution. 
plot_solution <- function(s){
  s$solution_1 <- factor(s$solution_1)
  plot(st_as_sf(s[, "solution_1"]), pal = c("grey90", "darkgreen"), main = "Solution 1")
}

# print a short summary of the data
print(tas_pu)

plot(tas_pu)

# plot a map of the planning unit cost data
spplot(tas_pu, "cost")

# print a short summary of the data
print(tas_features)



# make prioritization problem
p1 <- problem(tas_pu, tas_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.05) %>% # 5% representation targets
  add_binary_decisions() %>%
  add_gurobi_solver(verbose = FALSE)
# add_lpsymphony_solver(verbose = FALSE)

# print problem
print(p1)

# solve problem
s1 <- solve(p1)

# print solution, the solution_1 column contains the solution values
# indicating if a planning unit is (1) selected or (0) not
print(s1)

# calculate total cost of the prioritization
sum(s1$solution_1 * s1$cost)

eval_cost_summary(p1, s1)
plot_solution(s1)

# make prioritization problem
p2 <- problem(tas_pu, tas_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.05) %>%
  add_locked_in_constraints("locked_in") %>%
  add_binary_decisions() %>%
  add_gurobi_solver(verbose = FALSE)
# add_lpsymphony_solver(verbose = FALSE)

# print problem
print(p2)

# solve problem
s2 <- solve(p2)

# plot solution
plot_solution(s2)

prob <- problem(tas_pu, tas_features, cost_column = "cost")

p2 <- prob %>%
  add_min_set_objective() %>%
  add_relative_targets(0.05) %>%
  add_locked_in_constraints("locked_in") %>%
  add_binary_decisions() %>%
  add_gurobi_solver(verbose = FALSE)

s2 <- solve(p2)
plot_solution(s2)

p3 <- p2 %>% 
  add_relative_targets(0.2)

print(p3)
s3 <- solve(p3)
print(s3)
plot_solution(s3)


p4 <- prob %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_locked_in_constraints("locked_in") %>%
  add_binary_decisions() %>%
  add_gurobi_solver(verbose = FALSE) %>% 
  add_locked_out_constraints("locked_out")

s4 <- solve(p4)

print(s4)
plot_solution(s4)


p5 <- prob %>%
  add_min_set_objective() %>%
  add_boundary_penalties(penalty = 0.0005) %>%
  add_relative_targets(0.2) %>%
  add_locked_in_constraints("locked_in") %>%
  add_locked_out_constraints("locked_out") %>%
  add_binary_decisions() %>%
  add_gurobi_solver(verbose = FALSE)
# add_lpsymphony_solver(verbose = FALSE)

# print problem
print(p5)

system.time(s5 <- solve(p5))
plot_solution(s5)

eval_cost_summary(p4, s4[, "solution_1"])
eval_cost_summary(p5, s5[, "solution_1"])


# funds for additional land acquisition (same units as cost data)
funds <- 100

# calculate the total budget for the prioritization
budget <- funds + sum(s4$cost * s4$locked_in)
print(budget)


# make prioritization problem
p6 <- prob %>%
  add_max_features_objective(budget) %>%
  add_relative_targets(0.2) %>%
  add_locked_in_constraints("locked_in") %>%
  add_locked_out_constraints("locked_out") %>%
  add_binary_decisions() %>%
  add_gurobi_solver(verbose = FALSE)
# add_lpsymphony_solver(verbose = FALSE)

# print problem
print(p6)


s6 <- solve(p6)
plot_solution(s6)

# calculate feature representation
r6 <- eval_feature_representation_summary(p6, s6[, "solution_1"])

# calculate number of features with targets met
sum(r6$relative_held >= 0.2, na.rm = TRUE)



# calculate weights as the log inverse number of grid cells that each vegetation
# class occupies, rescaled between 1 and 100
wts <- 1 / cellStats(tas_features, "sum")
wts <- rescale(wts, to = c(1, 10))

# print the name of the feature with smallest weight
names(tas_features)[which.min(wts)]

# print the name of the feature with greatest weight
names(tas_features)[which.max(wts)]

# plot histogram of weights
hist(wts, main = "feature weights")



# make prioritization problem with weights
p7 <- prob %>%
  add_max_features_objective(budget) %>%
  add_relative_targets(0.2) %>%
  add_feature_weights(wts) %>%
  add_locked_in_constraints("locked_in") %>%
  add_locked_out_constraints("locked_out") %>%
  add_binary_decisions() %>%
  add_gurobi_solver(verbose = FALSE)
# add_lpsymphony_solver(verbose = FALSE)

# print problem
print(p7)

# solve problem
s7 <- solve(p7)

# plot solution
plot_solution(s7)

# calculate feature representation
r7 <- eval_feature_representation_summary(p7, s7[, "solution_1"])

# calculate number of features with targets met
sum(r7$relative_held >= 0.2, na.rm = TRUE)





# make problem with a shuffle portfolio
p8 <- prob %>%
  add_max_features_objective(budget) %>%
  add_relative_targets(0.2) %>%
  add_feature_weights(wts) %>%
  add_binary_decisions() %>%
  add_shuffle_portfolio(number_solutions = 6,
                        remove_duplicates = FALSE) %>%
  add_gurobi_solver(verbose = TRUE, gap = 10)
# add_lpsymphony_solver(verbose = TRUE, gap = 10)
print(p8)

s8 <- solve(p8)
print(s8)



# plot all solutions
# s8_plots <- lapply(paste0("solution_", seq_len(6)), function(x) {
#   spplot(s8, x, main = x, col.regions = c("white", "darkgreen"))
# })
# do.call(grid.arrange, append(s8_plots, list(ncol = 3)))

s8$solution_1 <- factor(s8$solution_1)
s8$solution_2 <- factor(s8$solution_2)
s8$solution_3 <- factor(s8$solution_3)
s8$solution_4 <- factor(s8$solution_4)
s8$solution_5 <- factor(s8$solution_5)
s8$solution_6 <- factor(s8$solution_6)

plot(st_as_sf(s8[, c("solution_1", "solution_2", "solution_3", "solution_4", "solution_5", "solution_6")]),
     pal = c("grey90", "darkgreen"))
