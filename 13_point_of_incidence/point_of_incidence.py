#!/usr/bin/env python3

def reflection_delta(row: str, column: int) -> int:
    reflection_length = min(len(row[:column]), len(row[column:]))
    return sum(row[:column][::-1][:reflection_length][i] != row[column:][:reflection_length][i] for i in range(reflection_length))

def reflection_columns(pattern: list[str], delta=0) -> int:
    for col in range(1, len(pattern[0])):
        if sum(reflection_delta(pattern[row], col) for row in range(len(pattern))) == delta:
            return col
    return 0

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        patterns = [[]]
        for line in f.readlines():
            line = line.strip()
            if line:
                patterns[-1].append(line)
            else:
                patterns.append([])

        def transpose(pattern): return list(map(''.join, zip(*pattern)))

        # part 1
        result = 0
        for pattern in patterns:
            result += reflection_columns(pattern) or 100 * reflection_columns(transpose(pattern))
        print(result)

        # part 2
        result = 0
        for pattern in patterns:
            result += reflection_columns(pattern, 1) or 100 * reflection_columns(transpose(pattern), 1)
        print(result)
