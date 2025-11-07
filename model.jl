#################################################################
#############  Model functions
#################################################################

# function to randomly select starting positions for each Agent in the NK landscape:
function random_NK_starting_position(data_matrix,
    n_agents::Int,
    n_groups::Int)
    # Generate random row indices with replacement separately for each group
    col_indices = [rand(1:size(data_matrix, 2), n_agents) for _ in 1:n_groups]
    # Extract the selected rows for each group
    selected_cols = Array{Float64}(undef, 11, n_agents, n_groups)
    for g in 1:n_groups
        for a in 1:n_agents
            selected_cols[:, a, g] = data_matrix[:, col_indices[g][a]]
        end
    end

    return selected_cols
 end
 

function run_model(
    n_groups::Int64,
    landscape_matrix::Matrix{Float64},
    n_gen::Int64,
    population_strategies,
    all_neighbors,
    t_step::Int64,
    n_agents::Int64,
    mu::Float64,
    n_strategies::Int64,
    random_starting,
    landscape_precomputed_index::Dict{Vector{Float64}, Int64}, 
    selection_level,
    save_every_n::Int64)

    saved_generations = []
    push!(saved_generations, 1)  # First generation
    push!(saved_generations, n_gen)  # Last generation
    
    # Add intermediate generations
    for gen in save_every_n:save_every_n:(n_gen-1)
        if gen != 1 && gen != n_gen
            push!(saved_generations, gen)
        end
    end
    
    total_saved_gens = length(saved_generations)
    total_entries = total_saved_gens * n_groups * n_agents

    # Pre-allocate arrays
    gen = Vector{Int}(undef, total_entries)
    group = Vector{Int}(undef, total_entries)
    ind = Vector{Int}(undef, total_entries)
    cumulative_payoff = Vector{Float64}(undef, total_entries)
    strategy = Vector{Int}(undef, total_entries)
    ind_payoff = Vector{Float64}(undef, total_entries)

    pop_degree = length.(all_neighbors)
    degree_of_neighbors = getindex.(Ref(pop_degree), all_neighbors)
    selected_indices = max_degree_index(degree_of_neighbors)
    highest_degree_neighbor = [all_neighbors[i][selected_indices[i]] for i in 1:length(all_neighbors)]

    idx = 1

    # Start evolutionary generations
    for i_gen in 1:n_gen
        # Set starting positions
        if random_starting == 1
            pop_solution = random_NK_starting_position(landscape_matrix, n_agents, n_groups)
        else
            pop_solution = lowest_NK_starting_position(landscape_matrix, landscape_precomputed_index, n_agents, n_groups)
        end

        # Prepare for this generation
        n_flip = length(pop_solution[:, 1, 1]) - 1
        cumulative_payoffs = zeros(n_groups)
        ind_payoffs = Array{Float64}(undef, n_agents, n_groups)

        # Run all time steps
        for i_step in 1:t_step
            for g in 1:n_groups
                for a in 1:n_agents  
                    agent_strategy = population_strategies[g][a]
                    if agent_strategy == 1
                        pop_solution[:, a, g] = hill_climb(pop_solution[:, a, g], 1, landscape_matrix, landscape_precomputed_index, n_flip)
                    elseif agent_strategy == 2
                        pop_solution[:, a, g] = payoff_biased_social_learning_no_hill_climbing!(pop_solution[:, a, g], pop_solution[:,:,g], all_neighbors[a], landscape_matrix, 1, landscape_precomputed_index, n_flip)
                    elseif agent_strategy == 3
                        pop_solution[:, a, g] = frequency_biased_learning_no_hill_climb(pop_solution[:, a, g], pop_solution[:,:,g], all_neighbors[a], landscape_matrix, 1, landscape_precomputed_index, n_flip)
                    end
                end
            end
        end

        # Calculate final payoffs after all timesteps
        for g in 1:n_groups
            for a in 1:n_agents
                indiv_payoff = pop_solution[end, a, g]
                ind_payoffs[a, g] = indiv_payoff
                cumulative_payoffs[g] += indiv_payoff
            end
        end

        
        if i_gen in saved_generations
            if idx + (n_groups * n_agents - 1) <= total_entries  # Check bounds
                for g in 1:n_groups
                    for a in 1:n_agents
                        gen[idx] = i_gen
                        group[idx] = g
                        ind[idx] = a
                        cumulative_payoff[idx] = cumulative_payoffs[g]
                        strategy[idx] = population_strategies[g][a]
                        ind_payoff[idx] = ind_payoffs[a, g]
                        idx += 1
                    end
                end
            else
                println("Warning: Not enough space allocated. idx=$idx, total_entries=$total_entries")
                break
            end
        end

        # Evolve strategies
        population_strategies = mutate_selection_entire_pop(
            population_strategies,
            mu,
            n_strategies,
            cumulative_payoffs,
            n_groups,
            0.2
        )
    end

    actual_entries = idx - 1
    df_res = DataFrame(
        gen = gen[1:actual_entries],
        group = group[1:actual_entries],
        ind = ind[1:actual_entries],
        cumulative_payoff = cumulative_payoff[1:actual_entries],
        strategy = strategy[1:actual_entries],
        ind_payoff = ind_payoff[1:actual_entries]
    )

    return df_res
end



function run_simulations(
    i_simul::Int,
    keys_K0,
    keys_K8,
    landscapes_K0,
    landscapes_K8,
    landscape_codes_K0, 
    landscape_codes_K8,
    landscape_indices_K0,
    landscape_indices_K8,
    networks,
    n_groups::Int,
    t_step::Int,
    n_agents::Int,
    n_strategies::Int,
    random_starting, 
    n_gen::Int,
    strategy_types,
    mu_values,
    all_results,  
    save_every_n)

    local_results = []
    
    # Create all unique parameter combinations
    param_combinations = []
    for (network_matrix, network_name) in networks
        for mu_value in mu_values
            push!(param_combinations, (network_matrix, network_name, mu_value))
        end
    end
    
    run_count = 0
    total_rows_added = 0

    # Outer loop over simulations 
    for simul in 1:i_simul
        
        # For each unique parameter combination, sample one landscape from each type
        for (i, (network_matrix, network_name, mu_value)) in enumerate(param_combinations)
            
            # Randomly sample one landscape from each type for this parameter combination
            sampled_K0_key = rand(keys_K0)
            sampled_K8_key = rand(keys_K8)
            
            # Run simulation for K0 landscape
            landscape_matrix = sampled_K0_key
            landscape_name = landscapes_K0[landscape_matrix]
            landscape_code = landscape_codes_K0[landscape_matrix]
            landscape_precomputed_index = landscape_indices_K0[landscape_matrix]
            
            # Generate population strategies randomly for this run
            population_strategies = [rand(strategy_types, n_agents) for _ in 1:n_groups]
            all_neighbors = get_neighbors.(Ref(network_matrix), 1:n_agents)
            
            println("Running K0 simulation: simul=$simul, network=$network_name, mu=$mu_value")
            
            result_K0 = run_model(
                n_groups, 
                landscape_matrix, 
                n_gen, 
                population_strategies, 
                all_neighbors, 
                t_step, 
                n_agents, 
                mu_value,
                n_strategies, 
                random_starting, 
                landscape_precomputed_index, 
                "individual",
                save_every_n
            )

            
            # Run simulation for K8 landscape
            landscape_matrix = sampled_K8_key
            landscape_name = landscapes_K8[landscape_matrix]
            landscape_code = landscape_codes_K8[landscape_matrix]
            landscape_precomputed_index = landscape_indices_K8[landscape_matrix]
            
            # Generate new population strategies for K8 run
            population_strategies = [rand(strategy_types, n_agents) for _ in 1:n_groups]
            
            println("Running K8 simulation: simul=$simul, network=$network_name, mu=$mu_value")
            
            result_K8 = run_model(
                n_groups, 
                landscape_matrix, 
                n_gen, 
                population_strategies, 
                all_neighbors, 
                t_step, 
                n_agents, 
                mu_value,
                n_strategies, 
                random_starting, 
                landscape_precomputed_index, 
                "individual",
                save_every_n
            )
            
            # Validate the result immediately after creation
            if validate_simulation_result(result_K8, n_gen, n_strategies, n_groups)
                
                run_count += 1
                rows_in_result = nrow(result_K8)
                total_rows_added += rows_in_result
                
                
                result_K8_with_metadata = hcat(result_K8, DataFrame(
                landscape_name = fill(landscape_name, nrow(result_K8)),
                landscape_type = fill("K8", nrow(result_K8)),
                network_name = fill(network_name, nrow(result_K8)),
                mu_value = fill(mu_value, nrow(result_K8)),
                simul = fill(simul, nrow(result_K8)),
                landscape_code = fill(landscape_code, nrow(result_K8))
                ))
                
                # Push to LOCAL results array
                push!(local_results, result_K8_with_metadata)
                
            else
            end
        end
    end
    
    println("Completed $run_count simulation runs, total rows: $total_rows_added")
    
    # Return the LOCAL results array
    return local_results
end



#  Validation function to catch corrupted data early
function validate_simulation_result(result_df, n_gen, n_strategies, n_groups)
    if nrow(result_df) == 0
       println("  ERROR: Empty result dataframe")
        return false
    end
    
    # Check for reasonable ranges
    if "gen" in names(result_df)
        max_gen = maximum(result_df.gen)
        if max_gen > n_gen + 5  
            println("  ERROR: Invalid generation values (max: $max_gen, expected ≤ $n_gen)")
            return false
        end
    end
    
    if "strategy" in names(result_df)
        max_strategy = maximum(result_df.strategy)
        if max_strategy > n_strategies + 5  # Allow some buffer
            println("  ERROR: Invalid strategy values (max: $max_strategy, expected ≤ $n_strategies)")
            return false
        end
    end
    
    if "group" in names(result_df)
        max_group = maximum(result_df.group)
        if max_group > n_groups + 5  # Allow some buffer
            println("  ERROR: Invalid group values (max: $max_group, expected ≤ $n_groups)")
            return false
         end
    end
    
    println("  Data validation passed")
    return true
end

