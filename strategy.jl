# Strategy 1, Hill Climbing:
function hill_climb(solution, n_change::Int, landscape_matrix, landscape_precomputed_index, n_flip::Int)
    # Select a random position to flip
    position_flip = randperm(n_flip)[1]
    new_solution_vector = solution[:]
    new_solution_vector[position_flip] = 1.0 - new_solution_vector[position_flip]

    # Ensure the new solution vector matches the format of landscape_precomputed_index keys
    key_format = new_solution_vector[1:end-1]  # Exclude the fitness value at the end

    # Debugging: Print the key format
   # println("Checking key: ", key_format)

    # Access the column index from precomputed index
    col_index = get(landscape_precomputed_index, key_format, nothing)
    
    # Handle case where the flipped solution is not found
    if col_index === nothing
        return solution
    end

    # Ensure col_index is valid
    if col_index < 1 || col_index > size(landscape_matrix, 2)
      #  println("Invalid column index: ", col_index)
        return solution
    end

    # Retrieve the new fitness value
    new_fitness = landscape_matrix[end, col_index]

    # Compare new fitness with current solution's fitness
    if solution[end] < new_fitness
      #  println("Improved fitness: ", new_fitness, " > ", solution[end])
        new_solution_vector[end] = new_fitness
        return new_solution_vector
    else
      #  println("No improvement: ", new_fitness, " <= ", solution[end])
        return solution
    end
end


# payoff bias

function payoff_biased_social_learning_no_hill_climbing!(
    pop_solution_focal, 
    pop_solution_ref,
    neighbors,
    landscape_matrix::Matrix{Float64},
    n_change::Int,
    landscape_precomputed_index::Dict{Vector{Float64}, Int64},
    n_flip::Int)
    
    # Sample neighbors' solutions from the relevant group/population
    sampled_vectors = [pop_solution_ref[:, i] for i in neighbors]
    
    # Find the index of the neighbor with the highest fitness (last element)
    max_index = neighbors[argmax([x[end] for x in sampled_vectors])]
    
    # Select the best neighbor solution
    new_solution = pop_solution_ref[:, max_index]
    
    # Compare the fitness of the best neighbor with the focal agent's solution
    if new_solution[end] > pop_solution_focal[end]
        return new_solution
    else
        # If the focal agent's own solution is better, keep current solution
        return pop_solution_focal
    end
end


# Strategy 3, Frequency-bias:
function frequency_biased_learning_no_hill_climb(
    pop_solution_focal, 
    pop_solution, 
    neighbors,
    landscape_matrix::Matrix{Float64}, 
    n_change::Int, 
    landscape_precomputed_index::Dict{Vector{Float64}, Int64},
    n_flip::Int
)
    # Sample the neighbors
    sampled_vectors = [pop_solution[:,n] for n in neighbors]

    # Calculate frequencies using countmap
    freq_map = countmap(sampled_vectors)
    
    # Find the maximum frequency
    max_freq = maximum(values(freq_map))
    
    # If there's a solution that appears more than once, consider adopting it
    if max_freq > 1
        # Find all solutions with the maximum frequency
        most_frequent_solutions = [sol for (sol, freq) in freq_map if freq == max_freq]
        
        # Randomly choose one of the most frequent solutions
        new_vector = rand(most_frequent_solutions)
        
        # Find out the new solution's fitness
        new_solution_and_fitness = @views extract_NKsolution_score(landscape_matrix, Vector(new_vector[1:end-1]), landscape_precomputed_index)
        
        # Adopt only if it's better than current solution
        if new_solution_and_fitness[end] > pop_solution_focal[end]
            return new_solution_and_fitness
        else
            return pop_solution_focal  # Keep current solution if not better
        end
    else
        # If no clear frequent solution exists, keep current solution
        return pop_solution_focal
    end
end


# Extract the score/payoff of a particular bit string location: 
function extract_NKsolution_score(landscape_matrix::Matrix{Float64},
    new_solution::Vector{Float64},
    landscape_precomputed_index::Dict{Vector{Float64}, Int64})
    # Use precomputed index to find the row
    row_index = get(landscape_precomputed_index, new_solution, nothing)
    if isnothing(row_index)
        error("No matching row found in the matrix.")
    end
    new_solution_and_fitness = vcat(new_solution, [landscape_matrix[end, row_index]])

    return new_solution_and_fitness
    #end
end
