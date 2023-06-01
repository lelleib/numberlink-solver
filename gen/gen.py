import sys
import random
import collections
import argparse
import collections
import string

from mitm import Mitm
from grid import Grid
import os

# Number of tries at adding loops to the grid before redrawing the side paths.
LOOP_TRIES = 1000


parser = argparse.ArgumentParser(description='Generate Numberlink Puzzles')
parser.add_argument('--min', type=int, default=-1,
                    help='Minimum number of pairs')
parser.add_argument('--max', type=int, default=-1,
                    help='Maximum number of pairs')
parser.add_argument('--verbose', action='store_true',
                    help='Print progress information')
parser.add_argument('--solve', action='store_true',
                    help='Print solution as well as puzzle')
parser.add_argument('--zero', action='store_true',
                    help='Print puzzle in zero format')
parser.add_argument('--no-colors', action='store_true',
                    help='Print puzzles without colors')
parser.add_argument('--no-pipes', action='store_true',
                    help='When printing solutions, don\'t use pipes')
parser.add_argument('--terminal-only', action='store_true',
                    help='Don\'t show the puzzle in matplotlib')


def has_loops(grid, uf):
    """ Check whether the puzzle has loops not attached to an endpoint. """
    groups = len({uf.find((x, y)) for y in range(grid.h)
                 for x in range(grid.w)})
    ends = sum(bool(grid[x, y] in 'v^<>')
               for y in range(grid.h) for x in range(grid.w))
    return ends != 2 * groups


def has_pair(tg, uf):
    """ Check for a pair of endpoints next to each other. """
    for y in range(tg.h):
        for x in range(tg.w):
            for dx, dy in ((1, 0), (0, 1)):
                x1, y1 = x + dx, y + dy
                if x1 < tg.w and y1 < tg.h:
                    if tg[x, y] == tg[x1, y1] == 'x' \
                            and uf.find((x, y)) == uf.find((x1, y1)):
                        return True
    return False


def has_tripple(tg, uf):
    """ Check whether a path has a point with three same-colored neighbours.
        This would mean a path is touching itself, which is generally not
        allowed in pseudo-unique puzzles.
        (Note, this also captures squares.) """
    for y in range(tg.h):
        for x in range(tg.w):
            r = uf.find((x, y))
            nbs = 0
            for dx, dy in ((1, 0), (0, 1), (-1, 0), (0, -1)):
                x1, y1 = x + dx, y + dy
                if 0 <= x1 < tg.w and 0 <= y1 < tg.h and uf.find((x1, y1)) == r:
                    nbs += 1
            if nbs >= 3:
                return True
    return False


def color_tubes(grid):
    colors = ['']
    tube_grid, uf = grid.make_tubes()
    raw_grid = Grid(tube_grid.w, tube_grid.h)
    letters = string.digits[1:] + string.ascii_letters
    letter_to_number = {ch: str(i + 1) for i, ch in enumerate(letters)}
    char = collections.defaultdict(lambda: letters[len(char)])
    col = collections.defaultdict(lambda: colors[len(col) % len(colors)])
    for x in range(tube_grid.w):
        for y in range(tube_grid.h):
            if tube_grid[x, y] == 'x':
                raw_grid[x, y] = letter_to_number[char[uf.find((x, y))]]
    return raw_grid, char


def make(w, h, mitm, min_numbers=0, max_numbers=1000):
    """ Creates a grid of size  w x h  without any loops or squares.
        The mitm table should be genearted outside of make() to give
        the best performance.
        """

    def test_ready(grid):
        # Test if grid is ready to be returned.
        sg = grid.shrink()
        stg, uf = sg.make_tubes()
        numbers = list(stg.values()).count('x') // 2
        return min_numbers <= numbers <= max_numbers \
            and not has_loops(sg, uf) \
            and not has_pair(stg, uf) \
            and not has_tripple(stg, uf) \

    # Internally we work on a double size grid to handle crossings
    grid = Grid(2 * w + 1, 2 * h + 1)

    gtries = 0
    while True:
        # Previous tries may have drawn stuff on the grid
        grid.clear()

        # Add left side path
        path = mitm.rand_path2(h, h, 0, -1)
        if not grid.test_path(path, 0, 0):
            continue
        grid.draw_path(path, 0, 0)
        # Draw_path doesn't know what to put in the first and last squares
        grid[0, 0], grid[0, 2 * h] = '\\', '/'

        # Add right side path
        path2 = mitm.rand_path2(h, h, 0, -1)
        if not grid.test_path(path2, 2 * w, 2 * h, 0, -1):
            continue
        grid.draw_path(path2, 2 * w, 2 * h, 0, -1)
        grid[2 * w, 0], grid[2 * w, 2 * h] = '/', '\\'

        # The puzzle might already be ready to return
        if test_ready(grid):
            return grid.shrink()

        # Add loops in the middle
        # Tube version of full grid, using for tracking orientations.
        # This doesn't make so much sense in terms of normal numberlink tubes.
        tg, _ = grid.make_tubes()
        # Maximum number of tries before retrying main loop
        for tries in range(LOOP_TRIES):
            x, y = 2 * random.randrange(w), 2 * random.randrange(h)

            # If the square square doen't have an orientation, it's a corner
            # or endpoint, so there's no point trying to add a loop there.
            if tg[x, y] not in '-|':
                continue

            path = mitm.rand_loop(clock=1 if tg[x, y] == '-' else -1)
            if grid.test_path(path, x, y):
                # A loop may not overlap with anything, and may even have
                # the right orientation, but if it 'traps' something inside it, that
                # might now have the wrong orientation.
                # Hence we clear the insides.
                grid.clear_path(path, x, y)

                # Add path and recompute orientations
                grid.draw_path(path, x, y, loop=True)
                tg, _ = grid.make_tubes()

                # Run tests to see if the puzzle is nice
                sg = grid.shrink()
                stg, uf = sg.make_tubes()
                numbers = list(stg.values()).count('x') // 2
                if numbers > max_numbers:
                    debug('Exceeded maximum number of number pairs.')
                    break
                if test_ready(grid):
                    debug(f'Finished in {tries} tries.')
                    debug(f'{numbers} numbers')
                    return sg

        debug(grid)
        debug(f'Gave up after {tries} tries')


def debug(s):
    try:
        if args.verbose:
            print(s, file=sys.stderr)
    except NameError:
        pass


def main(w, h, filePath):
    global args
    args = parser.parse_args()
    filePath = os.path.join(os.getcwd(), filePath)

    file = open(file=filePath, mode="w+")

    # w, h = args.width, args.height
    if w < 4 or h < 4:
        print('Please choose width and height at least 4.')
        return

    n = int((w * h)**.5)
    min_numbers = n * 2 // 3 if args.min < 0 else args.min
    max_numbers = n * 3 // 2 if args.max < 0 else args.max

    debug('Preprocessing...')
    mitm = Mitm(lr_price=2, t_price=1)
    # Using a larger path length in mitm might increase puzzle complexity, but
    # 8 or 10 appears to be the sweet spot if we want small sizes like 4x4 to
    # work.
    mitm.prepare(min(20, max(h, 6)))
    print('Generating puzzle...')

    grid = make(w, h, mitm, min_numbers, max_numbers)
    print('Made puzzle.')
    raw_grid, mapping,  = color_tubes(grid)

    lines = '['
    for y in range(raw_grid.h):
        line = '['
        for x in range(raw_grid.w):
            if grid[x, y] in 'v^<>':
                line += raw_grid[x, y] + ','
            else:
                line += '_,'
        # replace the last ',' with ''
        line = line[:-1] + ']'
        lines += line + ',\n'

    # replace the last ',\n' with ''
    lines = lines[:-2] + '].'
    file.write(lines)
    file.close()


if __name__ == '__main__':
    # generate 5 tiny puzzles
    for i in range(5):
        h = random.randint(5, 9)
        w = random.randint(5, 9)
        print("Generating tiny puzzle " + str(h) + "x" + str(w))
        main(w, h, "puzzles/tiny/"+str(h) + "x" + str(w) + ".txt")

    # generate 5 small puzzles
    for i in range(5):
        h = random.randint(10, 20)
        w = random.randint(10, 20)
        print("Generating small puzzle " + str(h) + "x" + str(w))
        main(w, h, "puzzles/small/"+str(h) + "x" + str(w) + ".txt")

    # generate 5 medium puzzles
    for i in range(5):
        h = random.randint(21, 40)
        w = random.randint(21, 40)
        print("Generating medium puzzle " + str(h) + "x" + str(w))
        main(w, h, "puzzles/medium/"+str(h) + "x" + str(w) + ".txt")

    # generate 5 large puzzles
    for i in range(5):
        h = random.randint(41, 60)
        w = random.randint(41, 60)
        print("Generating large puzzle " + str(h) + "x" + str(w))
        main(w, h, "puzzles/large/"+str(h) + "x" + str(w) + ".txt")

    # generate 5 very-large puzzles
    for i in range(5):
        h = random.randint(61, 80)
        w = random.randint(61, 80)
        print("Generating very-large puzzle " + str(h) + "x" + str(w))
        main(w, h, "puzzles/very-large/"+str(h) + "x" + str(w) + ".txt")
