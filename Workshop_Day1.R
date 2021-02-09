# load package
library(prioritizr)

# set default options for printing tabular data
options(tibble.width = Inf)

# load raster planning unit data
data(sim_pu_raster)

# print description of the data
print(sim_pu_raster)


# load polygon planning unit data
data(sim_pu_polygons)

# print first six rows of attribute table
head(sim_pu_polygons@data)

# load feature data
data(sim_features)

# plot the distribution of suitable habitat for each feature
plot(sim_features, main = paste("Feature", seq_len(nlayers(sim_features))),
     nr = 2, box = FALSE, axes = FALSE)


# create problem
p1 <- problem(sim_pu_raster, sim_features)

# print problem
print(p1)


# create problem with spatial vector data
# note that we have to specify which column in the attribute table contains
# the cost data
p2 <- problem(sim_pu_polygons, sim_features, cost_column = "cost")

# print problem
print(p2)


# create a new problem that has the minimum set objective
p3 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective()

p3 <- problem(sim_pu_raster, sim_features)

p3a <- p3 %>%
  add_min_set_objective()

# print the problem
print(p3a)


# create a new problem that has the maximum coverage objective and a budget
# of 5000
p4 <- problem(sim_pu_raster, sim_features) %>%
  add_max_cover_objective(5000)

# print the problem
print(p4)

# create a new problem that has the maximum features objective and a budget
# of 5000
p5 <- problem(sim_pu_raster, sim_features) %>%
  add_max_features_objective(budget = 5000)

# print the problem
print(p5)

p5a <- p4 %>% 
  add_max_features_objective(budget = 5000)

# create a problem with targets which specify that the solution must conserve
# a need a sum total of 3 units of suitable habitat for each feature
p10 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_absolute_targets(3)

# print problem
print(p10)


# create a problem with targets which specify that we need 10 % of the habitat
# for the first feature, 15 % for the second feature, 20 % for the third feature
# 25 % for the fourth feature and 30 % of the habitat for the fifth feature
targets <- c(0.1, 0.15, 0.2, 0.25, 0.3)
p12 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(targets)

# print problem
print(p12)




# create problem with constraints which specify that the first planning unit
# must be selected in the solution
p14 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_locked_in_constraints(c(1,5))

# print problem
print(p14)


# load data to lock in or lock out planning units
data(sim_locked_in_raster)
data(sim_locked_out_raster)

# plot the locked data
plot(stack(sim_locked_in_raster, sim_locked_out_raster),
     main = c("Locked In", "Locked Out"))


# create a problem using raster planning unit data and use the locked raster
# data to lock in some planning units and lock out some other planning units
p19 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_locked_in_constraints(sim_locked_in_raster) %>%
  add_locked_out_constraints(sim_locked_out_raster)

# print problem
print(p19)



# specify locked in data using the field name
p20 <- problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_locked_in_constraints("locked_in")

# print problem
print(p20)


p22 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_boundary_penalties(penalty = 0.01)
print(p22)


# create a problem and specify that Gurobi should be used to solve the problem
# and specify an optimality gap of zero to obtain the optimal solution
p28 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0)

# print problem
print(p28)



# formulate the problem
p37 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_boundary_penalties(penalty = 500, edge_factor = 0.5) %>%
  add_binary_decisions() %>% 
  add_gurobi_solver(gap = 0)

# solve the problem (using the default solver)
s37 <- solve(p37)

# plot solution
plot(s37, col = c("grey90", "darkgreen"), main = "Solution",
     xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))



p38 <- problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_boundary_penalties(penalty = 1000, edge_factor = 0.5) %>%
  add_binary_decisions() %>% 
  add_gurobi_solver(gap = 0)

# solve the problem (using the default solver)
s38 <- solve(p38)

# plot solution
plot(s38, col = c("grey90", "darkgreen"), main = "Solution",
     xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))


