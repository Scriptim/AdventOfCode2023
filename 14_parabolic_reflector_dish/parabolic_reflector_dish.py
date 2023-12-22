#!/usr/bin/env python3

def tilt_north(platform: list[list[str]]) -> list[list[str]]:
    stable = True
    for row in range(len(platform)):
        for col in range(len(platform[row])):
            if platform[row][col] == 'O':
                new_row = row
                while new_row > 0 and platform[new_row - 1][col] == '.':
                    new_row -= 1
                if new_row != row:
                    platform[row][col], platform[new_row][col] = '.', 'O'
                    stable = False
    return platform if stable else tilt_north(platform)

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        platform = list(map(lambda line: list(line.strip()), f.readlines()))

        # part 1
        result = 0
        for i, row in enumerate(tilt_north(platform)):
            for col in row:
                if col == 'O':
                    result += len(platform) - i
        print(result)
