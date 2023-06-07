from ortools.sat.python import cp_model
import time
import glob

def numberlink_f(filename):
    f = open(filename, 'r')
    puzzle_str = f.read()
    f.close()
    start_time = time.time()
    solution = numberlink(puzzle_str)
    elapsed_time = time.time() - start_time
    return solution, elapsed_time

def numberlink(puzzle_str):
    puzzle = read_puz(puzzle_str)
    max = get_max_puz(puzzle)
    
    model = cp_model.CpModel()

    solution = [[parse_cell(cell, max, model, i, j) for j, cell in enumerate(row)] for i, row in enumerate(puzzle)]

    maxi = len(solution) - 1
    maxj = len(solution[0]) - 1
    for i, row in enumerate(solution):
        for j, cell in enumerate(row):
            neighbors_idx = [(i-1, j), (i, j-1), (i, j+1), (i+1, j)]
            neighbors = [solution[ni][nj] for ni, nj in neighbors_idx if ni>=0 and ni<=maxi and nj>=0 and nj<=maxj]
            if isinstance(cell, int):
                add_count_eq(neighbors, cell, 1, model)
            else:
                add_count_eq(neighbors, cell, 2, model)
    
    solver = cp_model.CpSolver()
    status = solver.Solve(model)

    if status == cp_model.OPTIMAL or status == cp_model.FEASIBLE:
        print('yaaaaaaassss, found a solution')
        return [[solver.Value(cell) for cell in row] for row in solution]

    return 'no solutions, sorry'

def read_puz(puzzle_str):
    puzzle_str = ''.join(puzzle_str.split()) # remove whitespaces
    puzzle_str = puzzle_str.strip('[].') # strip beginning and end
    return [line.split(',') for line in puzzle_str.split('],[')] # parse into a matrix

def read_sol(solution_str):
    return [list(map(int, line)) for line in read_puz(solution_str)]

def get_max_puz(puzzle):
    puzzle = [0 if cell == '_' else int(cell) for row in puzzle for cell in row]
    return max(puzzle)

def parse_cell(cell, max, model, i, j):
    return model.NewIntVar(1, max, f'Var{i+1}-{j+1}') if cell == '_' else int(cell)

def add_count_eq(vars, value, count, model):
    boolvars = []
    for var in vars:
        boolvar = model.NewBoolVar('')
        model.Add(var == value).OnlyEnforceIf(boolvar)
        model.Add(var != value).OnlyEnforceIf(boolvar.Not())
        boolvars.append(boolvar)
    model.Add(count == sum(boolvars))

def print_m(M):
    for row in M:
        print(row)

for filename in glob.glob("puzzles/*/*"):
    solution, elapsed_time = numberlink_f(filename)
    print(f'Elapsed time until solution for {filename} (in seconds):')
    print(elapsed_time)
