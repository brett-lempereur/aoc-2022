from __future__ import annotations

from copy import deepcopy
from dataclasses import dataclass
from operator import attrgetter
from typing import Callable, List

# The relieve decay factor.
RELIEF_DECAY = 3


@dataclass
class Monkey:
    items: List[int]
    update: Callable[[int], int]
    target: Callable[[int], int]
    inspected: int = 0

    def turn(self, monkeys: List[Monkey], relief: int = RELIEF_DECAY) -> None:
        """
        Play a turn for this monkey.
        """
        self.inspected += len(self.items)
        while self.items:
            item = (self.update(self.items.pop()) % 9699690) // relief
            monkeys[self.target(item)].items.append(item)


def play_game(monkeys: List[Monkey], rounds=20, relief: int=RELIEF_DECAY) -> List[Monkey]:
    """
    Play a game and return a list sorted by most active monkey.
    """
    for i in range(rounds):
        for monkey in monkeys:
            monkey.turn(monkeys, relief)
    return sorted(monkeys, key=attrgetter("inspected"))

def monkey_business(monkeys: List[Monkey], rounds=20, relief: int=RELIEF_DECAY) -> int:
    """
    Compute the level of monkey business.
    """
    results = play_game(monkeys, rounds, relief)
    return results[-1].inspected * results[-2].inspected


# Input dataset.
EXAMPLE_MONKEYS = [
    Monkey([79, 98], lambda v: v * 19, lambda v: 2 if v % 23 == 0 else 3),
    Monkey([54, 65, 75, 74], lambda v: v + 6, lambda v: 2 if v % 19 == 0 else 0),
    Monkey([79, 60, 97], lambda v: v * v, lambda v: 1 if v % 13 == 0 else 3),
    Monkey([74], lambda v: v + 3, lambda v: 0 if v % 17 == 0 else 1)
]
MONKEYS = [
    Monkey([98, 97, 98, 55, 56, 72], lambda v: v * 13, lambda v: 4 if v % 11 == 0 else 7),
    Monkey([73, 99, 55, 54, 88, 50, 55], lambda v: v + 4, lambda v: 2 if v % 17 == 0 else 6),
    Monkey([67, 98], lambda v: v * 11, lambda v: 6 if v % 5 == 0 else 5),
    Monkey([82, 91, 92, 53, 99], lambda v: v + 8, lambda v: 1 if v % 13 == 0 else 2),
    Monkey([52, 62, 94, 96, 52, 87, 53, 60], lambda v: v * v, lambda v: 3 if v % 19 == 0 else 1),
    Monkey([94, 80, 84, 79], lambda v: v + 5, lambda v: 7 if v % 2 == 0 else 0),
    Monkey([89], lambda v: v + 1, lambda v: 0 if v % 3 == 0 else 5),
    Monkey([70, 59, 63], lambda v: v + 3, lambda v: 4 if v % 7 == 0 else 3),
]

if __name__ == "__main__":
    print(f"Solution 1: {monkey_business(deepcopy(MONKEYS), 20, 3)}")
    print(f"Solution 2: {monkey_business(deepcopy(MONKEYS), 10000, 1)}")
