from typing import TypeAlias

State: TypeAlias = int
Input: TypeAlias = str


class FARule:
    state: State
    input: Input | None
    next_state: State

    def __init__(self, state: State, input: Input | None, next_state: State):
        self.state = state
        self.input = input
        self.next_state = next_state

    def follow(self, state: State, input: Input | None) -> State | None:
        if self.state == state and self.input == input:
            return self.next_state
        return None

    def __repr__(self) -> str:
        return f"{self.state} --{self.input}--> {self.next_state}"


class DFARulebook:
    rules: list[FARule]

    def __init__(self, rules: list[FARule]):
        self.rules = rules

    def next_state(self, state: State, input: Input) -> State:
        for rule in self.rules:
            if next_state := rule.follow(state, input):
                return next_state
        raise Exception(f"No rule for {state} --{input}-->")


class DFA:
    current_state: State
    accept_states: set[State]
    rulebook: DFARulebook

    def __init__(
        self, current_state: State, accept_states: set[State], rulebook: DFARulebook
    ):
        self.current_state = current_state
        self.accept_states = accept_states
        self.rulebook = rulebook

    def input(self, input_: Input) -> None:
        for c in input_:
            self.current_state = self.rulebook.next_state(self.current_state, c)

    def accepting(self) -> bool:
        return self.current_state in self.accept_states


class DFADesign:
    start_state: State
    accept_states: set[State]
    rulebook: DFARulebook

    def __init__(
        self, start_state: State, accept_states: set[State], rulebook: DFARulebook
    ):
        self.start_state = start_state
        self.accept_states = accept_states
        self.rulebook = rulebook

    def to_dfa(self) -> DFA:
        return DFA(self.start_state, self.accept_states, self.rulebook)

    def accepts(self, string: str) -> bool:
        dfa = self.to_dfa()
        dfa.input(string)
        return dfa.accepting()

    def __repr__(self) -> str:
        return f"{self.start_state} {self.accept_states} {self.rulebook}"


class NFARulebook:
    rules: list[FARule]

    def __init__(self, rules: list[FARule]):
        self.rules = rules

    def next_states(self, states: set[State], input: Input | None) -> set[State]:
        ret: list[State] = []
        for rule in self.rules:
            for state in states:
                if next_state := rule.follow(state, input):
                    ret.append(next_state)
        return set(ret)


class NFA:
    _current_states: set[State]
    accept_states: set[State]
    rulebook: NFARulebook

    def __init__(
        self,
        current_states: set[State],
        accept_states: set[State],
        rulebook: NFARulebook,
    ):
        self._current_states = current_states
        self.accept_states = accept_states
        self.rulebook = rulebook

    def input(self, input_: Input) -> None:
        for c in input_:
            self._current_states = self.rulebook.next_states(self.current_states, c)

    def accepting(self) -> bool:
        return 0 < len(self.current_states & self.accept_states)

    def _follow_free_moves(self) -> None:
        extra_states = self.rulebook.next_states(self._current_states, None)
        if  0 < len(extra_states - self._current_states):
            self._current_states |= extra_states
            self._follow_free_moves()

    @property
    def current_states(self) -> set[State]:
        self._follow_free_moves()
        return self._current_states


class NFADesign:
    start_state: State
    accept_states: set[State]
    rulebook: NFARulebook

    def __init__(
        self, start_state: State, accept_states: set[State], rulebook: NFARulebook
    ):
        self.start_state = start_state
        self.accept_states = accept_states
        self.rulebook = rulebook

    def to_nfa(self) -> NFA:
        return NFA(set([self.start_state]), self.accept_states, self.rulebook)

    def accepts(self, string: str) -> bool:
        nfa = self.to_nfa()
        nfa.input(string)
        return nfa.accepting()


def test_dfa_rulebook():
    rulebook = DFARulebook(
        [
            FARule(1, "a", 2),
            FARule(1, "b", 1),
            FARule(2, "a", 2),
            FARule(2, "b", 3),
            FARule(3, "a", 3),
            FARule(3, "b", 3),
        ]
    )

    assert rulebook.next_state(1, "a") == 2
    assert rulebook.next_state(1, "b") == 1
    assert rulebook.next_state(2, "a") == 2


def test_dfa_design():
    rulebook = DFARulebook(
        [
            FARule(1, "a", 2),
            FARule(1, "b", 1),
            FARule(2, "a", 2),
            FARule(2, "b", 3),
            FARule(3, "a", 3),
            FARule(3, "b", 3),
        ]
    )

    dfa_design = DFADesign(1, set([3]), rulebook)
    assert not dfa_design.accepts("a")
    assert not dfa_design.accepts("ba")
    assert dfa_design.accepts("baba")


def test_nfa_rulebook():
    rulebook = NFARulebook(
        [
            FARule(1, "a", 1),
            FARule(1, "b", 1),
            FARule(1, "b", 2),
            FARule(2, "a", 3),
            FARule(2, "b", 3),
            FARule(3, "a", 4),
            FARule(3, "b", 4),
            FARule(5, None, 1),
            FARule(5, "b", 3),
        ]
    )

    assert rulebook.next_states(set([1]), "b") == set([1, 2])
    assert rulebook.next_states(set([1, 2]), "a") == set([1, 3])
    assert rulebook.next_states(set([1, 3]), "b") == set([1, 2, 4])
    assert rulebook.next_states(set([5]), None) == set([1])


def test_nfa_design():
    rulebook = NFARulebook(
        [
            FARule(1, "a", 1),
            FARule(1, "b", 1),
            FARule(1, "b", 2),
            FARule(2, "a", 3),
            FARule(2, "b", 3),
            FARule(3, "a", 4),
            FARule(3, "b", 4),
        ]
    )

    nfa_design = NFADesign(1, set([4]), rulebook)
    assert nfa_design.accepts("bab")
    assert nfa_design.accepts("bbbbb")
    assert not nfa_design.accepts("bbabb")

def test_free_move():
    rulebook = NFARulebook(
        [
            FARule(1, None, 2),
            FARule(1, None, 4),
            FARule(2, "a", 3),
            FARule(3, "a", 2),
            FARule(4, "a", 5),
            FARule(5, "a", 6),
            FARule(6, "a", 4),
        ]
    )

    nfa_design = NFADesign(1, set([2, 4]), rulebook)
    assert nfa_design.accepts("aa")
    assert nfa_design.accepts("aaa")
    assert not nfa_design.accepts("aaaaa")
    assert nfa_design.accepts("aaaaaa")
