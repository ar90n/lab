fn main() {
    let mut fsm = StateMachineRunner::new();
    while !fsm.is_done() {
        fsm.step();
    }
}

trait State {
    fn step(&self) -> StateMachine;
}

// The following states can be the 'S' in StateMachine<S>
struct Waiting {}

impl State for Waiting {
    fn step(&self) -> StateMachine {
        println!("in Waiting");
        StateMachine::Filling(Filling {})
    }
}

struct Filling {}
impl State for Filling {
    fn step(&self) -> StateMachine {
        println!("in Filling");
        StateMachine::Done(Done {})
    }
}

struct Done;

impl State for Done {
    fn step(&self) -> StateMachine {
        println!("in Done");
        StateMachine::Done(Done {})
    }
}

impl Waiting {
    fn new() -> Self {
        Self {}
    }
}

// Here is we're building an enum so we can contain this state machine in a parent.
enum StateMachine {
    Waiting(Waiting),
    Filling(Filling),
    Done(Done),
}

// Defining a function which shifts the state along.
impl StateMachine {
    fn step(&self) -> Self {
        match self {
            Self::Waiting(state) => state.step(),
            Self::Filling(state) => state.step(),
            Self::Done(state) => state.step(),
        }
    }

    fn new() -> Self {
        Self::Waiting(Waiting::new())
    }

    fn is_done(&self) -> bool {
        match self {
            Self::Done(_) => true,
            _ => false,
        }
    }
}

struct StateMachineRunner {
    state: StateMachine,
}

impl StateMachineRunner {
    pub fn new() -> Self {
        Self {
            state: StateMachine::new(),
        }
    }

    pub fn step(&mut self) {
        self.state = self.state.step()
    }

    pub fn is_done(&self) -> bool {
        self.state.is_done()
    }
}
