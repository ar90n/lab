from dataclasses import dataclass
from typing import TypeAlias, Any

State: TypeAlias = int


class Stack:
    _items: list[str]

    def __init__(self, items: list[str] | None = None):
        self._items = items or []

    def pop(self) -> "Stack":
        return Stack(self._items[1:])

    def push(self, item: str) -> "Stack":
        return Stack([item] + self._items)

    def top(self) -> str:
        return self._items[-1]

    def __str__(self) -> str:
        return f"Stack({self._items})"

    def __eq__(self, other: Any) -> bool:
        if isinstance(other, Stack):
            return self._items == other._items
        return False


@dataclass
class PDAConfiguration:
    state: State
    stack: Stack


class PDARule:
    state: State
    character: str | None
    next_state: State
    pop_character: str
    push_characters: list[str]

    def __init__(
        self,
        state: State,
        character: str | None,
        next_state: State,
        pop_character: str,
        push_characters: list[str],
    ) -> None:
        self.state = state
        self.character = character
        self.next_state = next_state
        self.pop_character = pop_character
        self.push_characters = push_characters

    def follow(
        self, configuration: PDAConfiguration, character: str | None
    ) -> PDAConfiguration | None:
        if (
            self.state != configuration.state
            and self.character == character
            and self.pop_character == configuration.stack.top()
        ):
            return None
        return PDAConfiguration(self.next_state, self._next_stack(configuration.stack))

    def _next_stack(self, stack: Stack) -> Stack:
        next_stack = stack.pop()
        for c in reversed(self.push_characters):
            next_stack = next_stack.push(c)
        return next_stack


class DPDARulebook:
    rules: list[PDARule]

    def __init__(self, rules: list[PDARule]):
        self.rules = rules

    def next_configuration(
        self, configuration: PDAConfiguration, character: str
    ) -> PDAConfiguration | None:
        for rule in self.rules:
            if next_configuration := rule.follow(configuration, character):
                return next_configuration


class DPDA:
    current_configuration: PDAConfiguration
    accept_states: set[State]
    rulebook: DPDARulebook

    def __init__(
        self,
        current_configuration: PDAConfiguration,
        accept_states: set[State],
        rulebook: DPDARulebook,
    ) -> None:
        self.current_configuration = current_configuration
        self.accept_states = accept_states
        self.rulebook = rulebook

    def accepting(self) -> bool:
        return self.current_configuration.state in self.accept_states

    def read_string(self, string: str) -> None:
        for c in string:
            if next_configuration := self.rulebook.next_configuration(
                self.current_configuration, c
            ):
                self.current_configuration = next_configuration


def test_dpda_rulebook():
    rulebook = DPDARulebook(
        [
            PDARule(1, "(", 2, "$", ["b", "$"]),
            PDARule(2, "(", 2, "b", ["b", "b"]),
            PDARule(2, ")", 2, "b", []),
            PDARule(2, None, 1, "$", ["$"]),
        ]
    )

    configuration = PDAConfiguration(1, Stack(["$"]))
    configuration = rulebook.next_configuration(configuration, "(")
    assert configuration is not None
    assert configuration == PDAConfiguration(2, Stack(["b", "$"]))
    configuration = rulebook.next_configuration(configuration, "(")
    assert configuration is not None
    assert configuration == PDAConfiguration(2, Stack(["b", "b", "$"]))
