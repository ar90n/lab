type planet =
  | Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Neptune
  | Uranus;

let ageOn = (p, s) => {
    let ratio = 
        switch(p) {
        | Earth => 1.0
        | Mercury => 0.2408467
        | Venus => 0.61519726
        | Mars => 1.8808158
        | Jupiter => 11.862615
        | Saturn => 29.447498
        | Uranus => 84.016846
        | Neptune => 164.79132
        };
    s /. (ratio *. 31557600.0 )
};
