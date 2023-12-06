#!/usr/bin/env python3

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        lines = f.readlines()

        # parse input
        cards = []
        for line in lines:
            [card_str, all_numbers_str] = line.split(':')
            card_number = int(card_str[5:])
            [winning_numbers_str, my_numbers_str] = all_numbers_str.split('|')
            winning_numbers = set(int(x) for x in winning_numbers_str.split())
            my_numbers = set(int(x) for x in my_numbers_str.split())
            cards.append((card_number, winning_numbers, my_numbers))
        num_cards = len(cards)

        # part 1
        result = 0
        for _, winning_numbers, my_numbers in cards:
            matches = len(winning_numbers.intersection(my_numbers))
            if matches:
                result += 2 ** (matches - 1)
        print(result)
