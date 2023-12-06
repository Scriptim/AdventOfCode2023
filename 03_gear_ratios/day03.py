#!/usr/bin/env python3

def adjacent(number: tuple[int, int, int], symbol: tuple[str, int, int]) -> bool:
    (num_val, num_row, num_col) = number
    num_width = len(str(num_val))
    (_, sym_row, sym_col) = symbol
    return num_col - 1 <= sym_col <= num_col + num_width and abs(sym_row - num_row) <= 1

if __name__ == '__main__':
    with open('input.txt', 'r') as f:

        # parse input
        symbols = []  # (symbol, row, col)
        numbers = []  # (value, row, col)
        row = 0
        while line := f.readline().strip():
            reading_number = False
            for col, char in enumerate(line):
                if char == '.':
                    # blank
                    reading_number = False
                elif char.isdigit():
                    # number
                    if reading_number:
                        num_val, num_row, num_col = numbers[-1]
                        numbers[-1] = (num_val * 10 + int(char), num_row, num_col)
                    else:
                        numbers.append((int(char), row, col))
                        reading_number = True
                else:
                    # symbol
                    symbols.append((char, row, col))
                    reading_number = False
            row += 1

        # part 1
        result = 0
        for number in numbers:
            if any(adjacent(number, symbol) for symbol in symbols):
                result += number[0]
        print(result)

        # part 2
        result = 0
        for gear in filter(lambda symbol: symbol[0] == '*', symbols):
            adjacent_numbers = [number[0] for number in numbers if adjacent(number, gear)]
            if len(adjacent_numbers) == 2:
                result += adjacent_numbers[0] * adjacent_numbers[1]
        print(result)
