

function mutate_selection_entire_pop(
    population::Vector{Vector{Int}},
    mu::Float64,
    n_strategies::Int,
    cumulative_payoffs,
    n_groups::Int,
    max_proportion_change::Float64
)
    
    ranked_indices = sortperm(cumulative_payoffs, rev=true)
    top_indices = ranked_indices[1:2]
    top_groups = population[top_indices]
    
    replicated_population = []

    while length(replicated_population) < n_groups
        append!(replicated_population, deepcopy(top_groups))
    end
    replicated_population = replicated_population[1:n_groups]
    
    replicated_population_for_mutation = [copy(group) for group in replicated_population]
    
    for (i, group) in enumerate(replicated_population_for_mutation)
        if rand() < mu
            n_individuals = length(group)
            
            # Randomly select which strategy to increase
            strategy_to_increase = rand(1:n_strategies)
            
            # Calculate number of agents to change based on max_proportion_change
            n_agents_to_change = round(Int, max_proportion_change * n_individuals)
            
            # If at least one agent should change
            if n_agents_to_change > 0
                # Randomly select positions to change
                positions_to_change = randperm(n_individuals)[1:n_agents_to_change]
                # Change those positions to the strategy_to_increase
                for pos in positions_to_change
                    replicated_population_for_mutation[i][pos] = strategy_to_increase
                end
            end
        end
    end
    return replicated_population_for_mutation
end
