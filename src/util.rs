use serde::{Serialize, Deserialize};
use std::ops;

pub mod rnd {
    use std::iter;
    use num::ToPrimitive;

    static mut STATE: u32 = 0;

    pub fn reseed(seed : u32) { unsafe { STATE = seed } }

    pub fn rnd() -> u32 {
        unsafe { STATE = STATE.wrapping_mul(1_103_515_245).wrapping_add(12_345); STATE %= 1 << 31; STATE }
    }

    pub fn rand<T : Copy>(from : T, unto : T) -> T
    where T : std::ops::Sub + num::cast::FromPrimitive + ToPrimitive,
          <T as std::ops::Sub>::Output : ToPrimitive {
        let r = (rnd() & 0xffff) as f32 / 65535.;
        T::from_f32(from.to_f32().unwrap() + (unto - from).to_f32().unwrap() * r + r).unwrap()
    }

    pub fn pick<T : iter::ExactSizeIterator>(mut items : T) -> T::Item {
        let n = rand(0, items.len()-1);
        items.nth(n).unwrap()
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn range() {
            reseed(1234);
            let from = 0;
            let unto = 11;
            let n = unto-from+1;
            let mut tots : Vec<u32> = Vec::with_capacity(n);
            tots.resize(n, 0);
            for _i in 0..n*100 {
                tots[rand(from, unto)-from] += 1;
            }
            let mut tot = 0;
            for i in 0..n {
                assert!(tots[i] > 80 && tots[i] < 120, "Hits for {} were {}", i+from, tots[i]);
                println!("Result {} hit {} times", i+from, tots[i]);
                tot += tots[i];
            }
            println!("Total hits = {}", tot);
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Dist(pub i32, pub i32);

impl Dist {
    pub fn north() -> Dist { Dist(-1, 0) }
    pub fn south() -> Dist { Dist(1, 0) }
    pub fn east() -> Dist { Dist(0, 1) }
    pub fn west() -> Dist { Dist(0, -1) }
    pub fn len(&self) -> usize { (self.0*self.0 + self.1*self.1) as usize }
    pub fn sgn(&self) -> Self { Dist(num::signum(self.0), num::signum(self.1)) }
}

impl ops::Add<Dist> for Dist { type Output = Self; fn add(self, other: Dist) -> Self { Self(self.0 + other.0, self.1 + other.1) } }

#[derive(Debug, Default, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Pos(pub u32, pub u32);

impl ops::Add<Dist> for Pos { type Output = Self; fn add(self, other: Dist) -> Self { Self(self.0.wrapping_add(other.0 as u32), self.1.wrapping_add(other.1 as u32)) } }
impl ops::AddAssign<Dist> for Pos { fn add_assign(&mut self, other: Dist) { self.0 = self.0.wrapping_add(other.0 as u32); self.1 = self.1.wrapping_add(other.1 as u32) } }
impl ops::Sub<Pos> for Pos { type Output = Dist; fn sub(self, other: Pos) -> Dist { Dist(self.0 as i32 - other.0 as i32, self.1 as i32 - other.1 as i32) } }

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Level { pub depth : u8, pub shaft : u8 }

impl Level {
    pub fn up(&self)    -> Level { Level {depth: self.depth-1, shaft:self.shaft} }
    pub fn down(&self)  -> Level { Level {depth: self.depth+1, shaft:self.shaft} }
    pub fn fore(&self)  -> Level { Level {depth: self.depth,   shaft:self.shaft+1} }
    pub fn back(&self)  -> Level { Level {depth: self.depth,   shaft:self.shaft-1} }
}

#[derive(Clone, Copy, Serialize, Deserialize)]
pub struct Location { pub lev : Level, pub pos : Pos }

impl Location { pub fn repos(&mut self, np : Pos) { self.pos = np; } }
