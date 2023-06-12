# numberlink-solver

A solver for the Numberlink puzzle game.

The Numberlink problem presents players with a grid of cells, each containing a number. The objective is to connect each pair of numbers with a path, ensuring that the paths do not cross or intersect each other. The goal is to fill the entire grid with lines connecting the numbers while meeting the above constraints.

## Installation

This project requires the Sicstus Prolog 4.7 and Python 3.6 or later.
To install the required dependencies, run the following command:

```bash
pip3 install ortools
```

## Usage

For the prolog solver, run the following predicate:

numberlink_test(<PUZZLE_FOLDER>, [impact,enum,median], <TIMEOUT>, <OUTPUT_FILE>).

Replace <PUZZLE_FOLDER> with the path to the puzzles sub-folder containing the puzzle files; <TIMEOUT> with the timeout in milliseconds and <OUTPUT_FILE> with the path to the output results file.

Example:

```bash
numberlink_test('1tiny', [impact,enum,median], 3600000, 'results_pl/1tiny.txt').
```

For the python solver, run the following command:

python3 numberlink_or.py <PUZZLE_FOLDER> <TIMEOUT> <OUTPUT_FILE>

Example:

```bash
python3 numberlink_or.py "1tiny" 3600000 "results_or/1tiny.txt"
```
