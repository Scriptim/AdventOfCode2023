#!/usr/bin/env python3

import re

def extract_digits(string: str) -> [str]:
    return [char for char in string if char.isdigit()]

def line_value(digits: [str]) -> int:
    return int(digits[0] + digits[-1])

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        lines = f.readlines()

        # part 1
        digits_lines = (extract_digits(line) for line in lines)
        result = sum(line_value(digits) for digits in digits_lines)
        print(result)
