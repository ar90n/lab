use std::fmt;

#[derive(PartialEq, fmt::Debug)]
pub struct Clock {
    hours: i32,
    minutes: i32,
}

fn calc_carry_and_minutes(minutes: i32) -> (i32, i32) {
    let mut carry = 0;
    let mut minutes = minutes;
    loop {
        if minutes < 0 {
            carry -= 1;
            minutes += 60;
        } else if 60 <= minutes {
            carry += 1;
            minutes -= 60;
        } else {
            break;
        }
    }
    (carry, minutes)
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        let (carry, minutes) = calc_carry_and_minutes(minutes);
        let hours = (((hours + carry) % 24) + 24) % 24;
        Clock {
            hours: hours,
            minutes: minutes,
        }
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        Clock::new(self.hours, self.minutes + minutes)
    }
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:02}:{:02}", self.hours, self.minutes)
    }
}
