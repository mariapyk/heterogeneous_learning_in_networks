# List and load all landscape CSV files from the directory
#### pre-compute landscape indices for faster processing: 

function create_landscape_index(landscape_matrix::Matrix{Float64})
    landscape_index = Dict{Vector{Float64}, Int}()
    for col in 1:size(landscape_matrix, 2)
        # Use the first columns as the key (excluding the last column, which holds fitness)
        key = landscape_matrix[1:end-1, col]  # Exclude the fitness (last column)
        landscape_index[key] = col  # Store the row index
    end
    return landscape_index
end

function load_landscapes_with_index(directory)
    landscapes = Dict{Matrix{Float64}, String}()
    landscape_indices = Dict{Matrix{Float64}, Dict{Vector{Float64}, Int}}()

    
    files = readdir(directory, join=true)
    for file in files
        if occursin(r"landscape_N10_K\d+_\d+\.csv", file)
            #landscape_name = "matrix_N10_" * split(split(basename(file), '_')[3], '.')[1]  # Extract K value and index
            landscape_name = split(basename(file), '.')[1]
            landscape_matrix = Matrix(CSV.read(file, DataFrame, transpose=true, header=false)[:, 2:end])

            # Store the landscape in the dictionary
            landscapes[landscape_matrix] = landscape_name

            # Precompute the index for fast lookups
            landscape_indices[landscape_matrix] = create_landscape_index(landscape_matrix)
        end
    end
    return landscapes, landscape_indices
end

