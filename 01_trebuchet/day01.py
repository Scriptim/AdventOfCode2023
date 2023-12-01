#!/usr/bin/env python3

import re

DIGIT_NAMES = ['_', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine']
DIGIT_WITH_NAMES_REGEX = '(?=(' + '|'.join([r'\d'] + DIGIT_NAMES) + '))'

def extract_digits(string: str) -> [str]:
    return [char for char in string if char.isdigit()]

def extract_digits_with_names(string: str) -> [str]:
    return re.findall(DIGIT_WITH_NAMES_REGEX, string)

def digit_names_to_digits(digits: [str]) -> [str]:
    return [digit if digit.isdigit() else str(DIGIT_NAMES.index(digit)) for digit in digits]

def line_value(digits: [str]) -> int:
    return int(digits[0] + digits[-1])

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        lines = f.readlines()

        # part 1
        digits_lines = (extract_digits(line) for line in lines)
        result = sum(line_value(digits) for digits in digits_lines)
        print(result)

        # part 2
        digits_lines = (digit_names_to_digits(extract_digits_with_names(line)) for line in lines)
        result = sum(line_value(digits) for digits in digits_lines)
        print(result)
