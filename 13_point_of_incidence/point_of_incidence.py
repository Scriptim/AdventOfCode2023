#!/usr/bin/env python3

def is_reflection(row: str, column: int) -> bool:
    reflection_length = min(len(row[:column]), len(row[column:]))
    return row[:column][::-1][:reflection_length] == row[column:][:reflection_length]

def reflection_columns(pattern: list[str]) -> int:
    for col in range(1, len(pattern[0])):
        if all(is_reflection(pattern[row], col) for row in range(len(pattern))):
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

        # part 1
        result = 0
        for pattern in patterns:
            result += reflection_columns(pattern) or 100 * reflection_columns(list(map(''.join, zip(*pattern))))
        print(result)
