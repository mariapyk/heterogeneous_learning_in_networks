

using Random 
using CSV, DataFrames 
using Dates

using StatsBase 
using BenchmarkTools

Random.seed!(123)

include("model.jl")
include("strategy.jl")
include("mutation.jl")
include("landscapes.jl")

#################### Simulation Parameters:

const i_simul = 100 # number of unique simulations
const env_change_value = 0.0
const mu_values = [0.01] # mutation
const random_starting = 1 # agents start from random location on the NK landscape==1 vs all from the same minimum location == 0
const n_groups = 30 # number of groups in a population competing
const t_step  = 25 # time steps each agent searches within a generation
const n_agents = 20 # number of agents per group
const generations = 20 # number of generations (t=25 time steps of search per generation)
const n_gen = generations
const n_strategies = 3 # number of learning strategies 
#Number of individuals by group
const n_pop = n_agents
save_gens = 2 # every nth generation saved


#### Paths pre-generated NK landscapes:
landscape_dirK0 = "NK_LandscapesN10K0"
landscape_dirK8 = "NK_LandscapesN10K8"

# strategy 1: explore(hill climb)
# strategy 2: success-bias
# strategy 3: frequence-bias

landscapes_K0, landscape_indices_K0 = load_landscapes_with_index(landscape_dirK0)
landscapes_K8, landscape_indices_K8 = load_landscapes_with_index(landscape_dirK8)

# Collect the keys (matrices) for later access
keys_K0 = collect(keys(landscapes_K0))
keys_K8 = collect(keys(landscapes_K8))

landscape_codes_K0 = Dict(key => randstring(8) for key in keys(landscapes_K0))
landscape_codes_K8 = Dict(key => randstring(8) for key in keys(landscapes_K8))

strategy_types = [1]  # Strategy types (1, 2, 3)



# ========================================
# REGULAR LATTICE NETWORKS
# ========================================

# function to get the neighbor with highest degree 
function max_degree_index(degree_of_neighbors::Vector{Vector{Int64}})
    highest_indices = []
    for degrees in degree_of_neighbors
        max_degree = maximum(degrees)
        max_indices = findall(x -> x == max_degree, degrees)
        if length(max_indices) > 1
            selected_index = rand(max_indices)
        else
            selected_index = max_indices[1]
        end
        push!(highest_indices, selected_index)
    end
    return highest_indices
end


## function to get the neighbors of an agent (identified by node/number)
function get_neighbors(network, node)
    return findall(x -> x != 0, network[node, :])
end

"""
Create a regular lattice network where each agent has exactly the same degree
"""

function create_regular_lattice(n_agents::Int, degree::Int)
    @assert degree < n_agents "Degree must be less than number of agents"
    @assert iseven(degree) "Degree must be even for undirected regular lattice"
    @assert degree >= 2 "Degree must be at least 2"
    
    adj_matrix = zeros(Int, n_agents, n_agents)
    
    # Each agent connects to degree/2 neighbors on each side
    k = degree ÷ 2
    
    for i in 1:n_agents
        for j in 1:k
            # Connect to j-th neighbor to the right
            right_neighbor = mod1(i + j, n_agents)
            adj_matrix[i, right_neighbor] = 1
            adj_matrix[right_neighbor, i] = 1  # Make undirected
            
            # Connect to j-th neighbor to the left  
            left_neighbor = mod1(i - j, n_agents)
            adj_matrix[i, left_neighbor] = 1
            adj_matrix[left_neighbor, i] = 1   # Make undirected
        end
    end
    
    return adj_matrix
end

#create_regular_lattice(10, 2)

# ========================================
# Random NETWORKS
# ========================================

function create_irregular_network(n_agents::Int, target_avg_degree::Int)
    @assert target_avg_degree < n_agents "Average degree must be less than number of agents"
    @assert target_avg_degree >= 2 "Average degree must be at least 2"
    @assert iseven(n_agents * target_avg_degree) "Total degree must be even"
    
    # Calculate total degree needed across all agents
    total_degree = n_agents * target_avg_degree
    
    # Create random degree sequence that sums to total_degree
    degrees = create_random_degree_sequence(n_agents, total_degree)
    
    # Build network using configuration model
    adj_matrix = build_network_from_degrees(degrees)
    
    return adj_matrix
end

"""
Create a random degree sequence that sums to target total
"""

function create_random_degree_sequence(n_agents::Int, total_degree::Int)
    # Start with minimum degree 1 for all agents (ensures connectivity)
    degrees = ones(Int, n_agents)
    remaining_degree = total_degree - n_agents
    
    # Randomly distribute the remaining degree
    while remaining_degree > 0
        # Pick a random agent
        agent = rand(1:n_agents)
        
        # Don't let any agent have degree >= n_agents
        if degrees[agent] < n_agents - 1
            degrees[agent] += 1
            remaining_degree -= 1
        end
    end
    
    return degrees
end

"""
Build network from degree sequence using configuration model with fallback
"""
function build_network_from_degrees(degrees::Vector{Int})
    n_agents = length(degrees)
    target_total_edges = sum(degrees) ÷ 2  # Expected number of edges
    adj_matrix = zeros(Int, n_agents, n_agents)
    
    # Try configuration model first
    stubs = Int[]
    for i in 1:n_agents
        for _ in 1:degrees[i]
            push!(stubs, i)
        end
    end
    
    shuffle!(stubs)
    
    # Pair stubs to create edges
    for i in 1:2:length(stubs)
        node1 = stubs[i]
        node2 = stubs[i + 1]
        
        # Add edge (avoid self-loops and multiple edges)
        if node1 != node2 && adj_matrix[node1, node2] == 0
            adj_matrix[node1, node2] = 1
            adj_matrix[node2, node1] = 1
        end
    end
    
    # Check if we got the right number of edges
    actual_edges = sum(adj_matrix) ÷ 2
    
    # If configuration model failed, add remaining edges randomly
    if actual_edges < target_total_edges
        missing_edges = target_total_edges - actual_edges
        attempts = 0
        max_attempts = missing_edges * 50  # Generous attempt limit
        
        while missing_edges > 0 && attempts < max_attempts
            i, j = rand(1:n_agents), rand(1:n_agents)
            
            if i != j && adj_matrix[i, j] == 0
                adj_matrix[i, j] = 1
                adj_matrix[j, i] = 1
                missing_edges -= 1
            end
            attempts += 1
        end
        
        if missing_edges > 0
            @warn "Could not place all edges. Missing $missing_edges edges."
        end
    end
    
    return adj_matrix
end



# ========================================
# UTILITY FUNCTIONS
# ========================================

"""
Calculate actual network density
"""

function calculate_network_density(adj_matrix)
    n = size(adj_matrix, 1)
    total_possible_edges = n * (n - 1)  # For directed graph
    actual_edges = sum(adj_matrix)
    return actual_edges / total_possible_edges
end

"""
Verify each agent has at least one connection (outgoing or incoming)
"""
function verify_connectivity(adj_matrix)
    n = size(adj_matrix, 1)
    for i in 1:n
        outgoing = sum(adj_matrix[i, :])
        incoming = sum(adj_matrix[:, i])
        if outgoing == 0 && incoming == 0
            return false, i
        end
    end
    return true, -1
end


# Run simulation for each agent count

function calculate_network_density(adjacency_matrix)
    n = size(adjacency_matrix, 1)
    total_possible_edges = n * (n - 1)  # For directed graph
    actual_edges = sum(adjacency_matrix)
    return actual_edges / total_possible_edges
end

all_results = []


# aggregation function
function aggregate_strategy_proportions_fixed(result_df)
    # Ensure key columns are the right type
    if "gen" in names(result_df)
        result_df.gen = convert(Vector{Int}, result_df.gen)
    end
    if "strategy" in names(result_df)
        result_df.strategy = convert(Vector{Int}, result_df.strategy)
    end
    if "group" in names(result_df)
        result_df.group = convert(Vector{Int}, result_df.group)
    end
    
    # Step 1: Calculate strategy proportions per group per generation
    aggregated = combine(groupby(result_df, [:gen, :group, :strategy]), nrow => :count)
    
    # Step 2: Calculate total agents per group per generation
    group_totals = combine(groupby(aggregated, [:gen, :group]), :count => sum => :total_agents)
    
    # Step 3: Merge and calculate proportions
    aggregated = leftjoin(aggregated, group_totals, on = [:gen, :group])
    aggregated.proportion = aggregated.count ./ aggregated.total_agents
    
    # Step 4: Calculate mean proportion across groups for each generation and strategy
    final_aggregated = combine(
        groupby(aggregated, [:gen, :strategy]),
        :proportion => mean => :mean_proportion,
        :proportion => std => :sd_proportion,
        :proportion => length => :n_groups
    )
    
    # Get metadata from first row (all rows have same metadata for a given simulation)
    first_row = result_df[1, :]
    
    # Add metadata columns
    final_aggregated[!, :landscape_name] .= first_row.landscape_name
    final_aggregated[!, :landscape_type] .= first_row.landscape_type  
    final_aggregated[!, :network_name] .= first_row.network_name
    final_aggregated[!, :mu_value] .= first_row.mu_value
    final_aggregated[!, :simul] .= first_row.simul
    final_aggregated[!, :landscape_code] .= first_row.landscape_code
    
    return final_aggregated
end


# Updated network types and simulation loop
network_types = ["regular", "irregular"]

n_agents_list = [10, 19]

degrees_for_agents = Dict(
    # Low connectivity (0.222 density)
    # High connectivity (0.889 density)
    10 => [2, 8],     # Densities: 0.222, 0.889
    19 => [4, 16]     # Densities: 0.222, 0.889
)

# Add labels for analysis
connectivity_labels = Dict(
    (10, 2) => "Low",   (19, 4) => "Low",     # Both 0.222 density
    (10, 8) => "High",  (19, 16) => "High"   # Both 0.889 density
)

# Initialize both results arrays before the loop
all_results = []           # For aggregated data
all_raw_results = []       # For raw individual-level data


for n_agents in n_agents_list
    for network_type in network_types
        
        # Get the appropriate degrees for this agent count
        degrees_list = degrees_for_agents[n_agents]
        
        for degree in degrees_list
            if degree < n_agents
                println("Running simulations with $n_agents agents, degree $degree, network type $network_type...")
          
                for simul_run in 1:i_simul  # Loop over individual simulation runs
                    
                    # Generate NEW network for each simulation run
                    if network_type == "regular"
                        network_matrix = create_regular_lattice(n_agents, degree)
                    elseif network_type == "irregular"
                        network_matrix = create_irregular_network(n_agents, degree)
                    end
                    network_matrix = convert(Matrix{Bool}, network_matrix)

                    network_name = "$(network_type)_num_agents$(n_agents)_degree$(degree)_sim$(simul_run)"
                    local networks = Dict(network_matrix => network_name)
                    actual_density = calculate_network_density(network_matrix)
                    
                    # Get connectivity label
                    connectivity_label = connectivity_labels[(n_agents, degree)]
                    
                    is_connected, isolated_agent = verify_connectivity(network_matrix)
                    
                    if !is_connected
                        println("Warning: Network has isolated agent: $isolated_agent in simulation $simul_run")
                        continue
                    end
                    
                    # Run simulation
                    temp_results = run_simulations(1,
                        keys_K0, keys_K8, landscapes_K0, landscapes_K8, 
                        landscape_codes_K0, landscape_codes_K8, landscape_indices_K0, landscape_indices_K8, 
                        networks, n_groups, t_step, n_agents, n_strategies, random_starting, n_gen, 
                        strategy_types, mu_values, [], save_gens)
                    
                    # Process the results and save BOTH raw and aggregated data
                    for result_df in temp_results
                        # Add the network-specific metadata to raw data
                        result_df[!, :n_agents] .= n_agents
                        result_df[!, :target_degree] .= degree
                        result_df[!, :actual_density] .= actual_density
                        result_df[!, :network_type] .= network_type
                        result_df[!, :network_instance] .= simul_run
                        result_df[!, :connectivity_label] .= connectivity_label  
                        result_df[!, :degree] .= degree  
                        result_df[!, :exact_connectivity] .= actual_density  
                        
                        # Aggregate to strategy proportions
                        aggregated_result = aggregate_strategy_proportions_fixed(result_df)
                        
                        # Add the network-specific metadata to aggregated result
                        aggregated_result[!, :n_agents] .= n_agents
                        aggregated_result[!, :target_degree] .= degree
                        aggregated_result[!, :actual_density] .= actual_density
                        aggregated_result[!, :network_type] .= network_type
                        aggregated_result[!, :network_instance] .= simul_run
                        aggregated_result[!, :connectivity_label] .= connectivity_label  
                        aggregated_result[!, :degree] .= degree  
                        aggregated_result[!, :exact_connectivity] .= actual_density  
                        
                        # Save AGGREGATED data (strategy proportions)
                        push!(all_results, aggregated_result)
                    end
                end
            end
        end
    end
end

println("Completed $(length(all_results)) aggregated results")


if length(all_results) > 0
    # Save aggregated data (strategy proportions)
    combined_aggregated = vcat(all_results...)
    CSV.write("results.csv", combined_aggregated)
else
end
