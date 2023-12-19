#!/usr/bin/env python3

def distance(galaxy1: (int, int), galaxy2: (int, int), expand_rows: [int], expand_columns: [int], expand_factor: int) -> int:
    crossed_rows = [row for row in expand_rows if min(y1, y2) < row < max(y1, y2)]
    crossed_columns = [col for col in expand_columns if min(x1, x2) < col < max(x1, x2)]
    crossed_total = len(crossed_rows) + len(crossed_columns)

    distance = abs(x1 - x2) + abs(y1 - y2) - crossed_total
    distance += expand_factor * crossed_total
    return distance

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        galaxies = []
        height = 0
        width = 0
        for y, line in enumerate(f.readlines()):
            height += 1
            width = len(line.strip())
            for x, char in enumerate(line.strip()):
                if char == '#':
                    galaxies.append((x, y))

        expand_rows = set(range(0, height)) - {y for _, y in galaxies}
        expand_columns = set(range(0, width)) - {x for x, _ in galaxies}

        # part 1
        result = 0
        for i, (x1, y1) in enumerate(galaxies):
            for x2, y2 in galaxies[i+1:]:
                result += distance((x1, y1), (x2, y2), expand_rows, expand_columns, 2)
        print(result)

        # part 2
        result = 0
        for i, (x1, y1) in enumerate(galaxies):
            for x2, y2 in galaxies[i+1:]:
                result += distance((x1, y1), (x2, y2), expand_rows, expand_columns, 1000000)
        print(result)
