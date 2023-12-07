#!/usr/bin/env python3

def hand_type(hand: list[int]) -> int:
    hand_no_joker = [card for card in hand if card != 0]
    num_jokers = len(hand) - len(hand_no_joker)
    num_threes = len(set(card for card in hand_no_joker if hand_no_joker.count(card) == 3))
    num_pairs = len(set(card for card in hand_no_joker if hand_no_joker.count(card) == 2))

    if num_jokers == 5 or hand_no_joker.count(hand_no_joker[0]) == len(hand_no_joker):
        return 6  # five of a kind
    elif any(hand_no_joker.count(card) == len(hand_no_joker) - 1 for card in hand_no_joker):
        return 5  # four of a kind
    elif (num_jokers, num_threes, num_pairs) in [(2, 0, 1), (1, 1, 0), (1, 0, 2), (0, 1, 1)]:
        return 4  # full house
    elif any(hand_no_joker.count(card) + num_jokers == 3 for card in hand_no_joker):
        return 3  # three of a kind
    elif num_pairs + num_jokers == 2:
        return 2  # two pair
    elif num_pairs + num_jokers == 1:
        return 1  # one pair
    else:
        return 0

def total_winnings(hands: list[tuple[list[int], int]]) -> int:
    result = 0
    for i, hand in enumerate(sorted(hands, key=lambda hand: (hand_type(hand[0]), hand[0]))):
        result += hand[1] * (i + 1)
    return result

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        hands = []
        for line in f:
            hand, bid = line.split()
            hand = ['23456789TJQKA'.index(card) + 2 for card in hand]
            bid = int(bid)
            hands.append((hand, bid))

        # part 1
        result = total_winnings(hands)
        print(result)

        # part 2
        result = total_winnings(([0 if card == 11 else card for card in hand], bid) for hand, bid in hands)
        print(result)
