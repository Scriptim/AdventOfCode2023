#!/usr/bin/env python3

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

        # part 1
        expand_rows = set(range(0, height)) - {y for _, y in galaxies}
        expand_columns = set(range(0, width)) - {x for x, _ in galaxies}
        result = 0
        for i, (x1, y1) in enumerate(galaxies):
            for x2, y2 in galaxies[i+1:]:
                distance = abs(x1 - x2) + abs(y1 - y2)
                distance += len([row for row in expand_rows if min(y1, y2) < row < max(y1, y2)])
                distance += len([col for col in expand_columns if min(x1, x2) < col < max(x1, x2)])
                result += distance
        print(result)
