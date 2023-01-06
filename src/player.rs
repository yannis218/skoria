use std::ops;
use serde::{Serialize, Deserialize};
use std::marker::PhantomData;
use crate::{util, ui, Then};
use crate::util::rnd;
use num::FromPrimitive;

#[derive(Clone, Serialize, Deserialize)]
pub struct Scores<T, V> {
    scores_for: PhantomData<T>,
    val: [V; 4]
}

impl<T, V> Scores<T, V> {
    pub fn new<F : std::convert::Into<V> + Copy>(v : [F; 4]) -> Scores<T, V> { Scores { val:[v[0].into(), v[1].into(), v[2].into(), v[3].into()], scores_for:PhantomData } }
}
impl<T, V> ops::Index<usize> for Scores<T, V> { type Output = V; fn index(&self, i: usize) -> &Self::Output { &self.val[i] } }
impl<T, V> ops::IndexMut<usize> for Scores<T, V> { fn index_mut(&mut self, i: usize) -> &mut Self::Output { &mut self.val[i] } }


#[derive(Clone, Copy, enum_map::Enum)] pub enum Race { Dwarf, Gnome, Hobbit, Pixie }
#[derive(Clone, Copy, enum_map::Enum)] pub enum Stat { Brawn=0, Speed=1, Craft=2, Sense=3 }
#[derive(Clone, Copy, enum_map::Enum)] pub enum Fuss { Bashed=0, Winded, Hungry, Sleepy }
#[derive(Clone, Copy, enum_map::Enum)] pub enum Good { Food, Meds, Gold, Gems }
#[derive(Clone, Copy, enum_map::Enum)] pub enum Eqpt { Spear, Shield, Byrnie, Boots,  Knife, Bow, Coat, Hat,  Staff, Wand, Robe, Book,  Dagger, Sling, Cape, Mask, }

pub static RACE_NAMES: [&str; 4] = [ "Dwarf", "Gnome", "Hobbit", "Pixie" ];
pub static JOB_NAMES:  [&str; 4] = [ "Guard", "Hunter", "Scholar", "Sneak" ];
pub static STAT_NAMES: [&str; 4] = [ "Brawn", "Speed", "Craft", "Sense" ];
pub static FUSS_NAMES: [&str; 4] = [ "Bashed", "Winded", "Hungry", "Sleepy" ];
pub static GOOD_NAMES: [&str; 4] = [ "Rations", "Potions", "Florins", "Crystal" ];
pub static EQUI_NAMES: [&str;16] = [
    "Spear", "Shield", "Byrnie", "Boots",   // Guard
    "Knife", "Bow", "Coat", "Hat",          // Hunter
    "Staff", "Wand", "Robe", "Book",        // Scholar
    "Dagger", "Sling", "Cape", "Mask"       // Sneak
];

static RACIAL_MODS: [[u8; 4]; 4] = [
    [ 5, 1, 2, 4], // Dwarf
    [ 4, 2, 4, 2], // Gnome
    [ 2, 4, 2, 4], // Hobbit
    [ 1, 5, 4, 2], // Pixie
];

static TRAINING_MODS : [[u8; 4]; 4] = [
    [ 6, 1, 3, 2], // Guard
    [ 4, 4, 1, 3], // Hunter
    [ 1, 2, 5, 4], // Scholar
    [ 1, 5, 3, 3], // Sneak
];

pub fn gen_stats(race : usize, job : usize) -> Stats { Scores::new([
    RACIAL_MODS[race][0] + TRAINING_MODS[job][0],	RACIAL_MODS[race][1] + TRAINING_MODS[job][1],
    RACIAL_MODS[race][2] + TRAINING_MODS[job][2],	RACIAL_MODS[race][3] + TRAINING_MODS[job][3],
])}

pub type Stats = Scores<Stat, u8>;
pub type Needs = Scores<Fuss, f32>;
pub type Goods = Scores<Good, u16>;
pub type Equal = Scores<Eqpt, u8>;

enum_from_primitive! {
#[derive(Clone, Copy, Serialize, Deserialize)]
enum Motion { Sneak, Creep, March, Run, Dash }
}
static MOT_NAMES: [&str; 5] = [ "Sneaking", "Creeping", "Marching", "Running", "Dashing" ];
static SPEED : [f32; 5] = [ 1.,   2.,   5.,   10., 20. ]; // in feet per sec
static FATIGUE: [f32;5] = [ 0.01, 0.02, 0.04, 0.3,  1. ];
static STEALTH: [f32;5] = [ 9.,   3.,   1.,   0.3,  0.1];

#[derive(Clone, Copy, Serialize, Deserialize)]
pub enum Action { Aiming, Sleeping }
static ACT_NAMES: [&str; 2] = [ "Aiming", "Sleeping" ];

#[derive(Serialize, Deserialize)]
pub struct Player {
    pub name : String, pub race : usize, pub job : usize,
    stats : Stats, equal : Equal, goods : Goods, fuss : Needs,
    loc : Option<util::Location>,
    pub turn: f32,
    mot : Motion,
    act : Option<Action>, act_info : String,
}

impl Player {
    pub fn new(name : String, race : usize, job : usize, stats : Stats, goods : [u16; 4]) -> Player {
        Player { name, race, job, stats, equal: Scores::new([1,1,1,1]), fuss: Scores::new([0.,0.,0.,0.]), goods: Scores::new(goods), loc:None, turn:0., mot:Motion::Creep, act: None, act_info: String::new() }
    }

    pub fn test() -> Player {
        let mut t = Player::new(String::from("Pelegrin from Archet"), 2, 1, Scores::new([8, 11, 5, 8]), [55, 7, 1503, 40]);
        t.loc = Some(util::Location{lev:util::Level{depth:1, shaft:0}, pos:util::Pos(10,22)});
        t
    }

    fn statbase(&self, s : usize) -> u8 { self.stats[s] }
    fn stat<T : enum_map::Enum>(&self, s : T) -> u8 {
        let i = s.into_usize();
        self.stats[i] - ((self.stats[i] - 1) as f32 * num::clamp(self.fuss[i] - 12., 0., 12.) / 12.) as u8
    }

    // LOCATION/MOTION
    pub fn dunglev(&self) -> util::Level { self.loc.unwrap().lev }
    pub fn mappos(&self) -> util::Pos { self.loc.unwrap().pos }
    pub fn location(&self) -> util::Location { self.loc.unwrap() }
    pub fn goto(&mut self, lev : util::Level, pos : util::Pos) { self.loc = Some(util::Location{lev, pos})}
    pub fn repos(&mut self, pos : util::Pos) { self.loc.as_mut().unwrap().repos(pos) }
    pub fn exit(&mut self) { self.loc = None }
    pub fn outside(&self) -> bool { self.loc.is_none() }
    pub fn faster(&mut self) { match Motion::from_i8(self.mot as i8 + 1) { Some(mot) => { self.mot = mot }, None => {} } }
    pub fn sneakier(&mut self) { match Motion::from_i8(self.mot as i8 - 1) { Some(mot) => { self.mot = mot }, None => {} } }

    // DERIVED STATS
    pub fn speed_fps(&self) -> f32 { SPEED[self.mot as usize] * self.stat(Stat::Speed) as f32 / 6. }
    pub fn move_fatigue(&self) -> f32 { FATIGUE[self.mot as usize] * (20. - self.equip(Eqpt::Boots) as f32) / 20. }
    pub fn attack_time(&self, _ranged : bool) -> f32 { // TODO implement ranged
        10. - 0.1 * (self.stat(Stat::Speed) + self.equip(Eqpt::Boots) + self.equip(Eqpt::Cape)) as f32
    }
    pub fn dmg_potential(&self, ranged : bool) -> u16 {
        if ranged {
            let bow = (self.equip(Eqpt::Bow) as f32).sqrt() * self.stat(Stat::Brawn) as f32;
            let wand = (self.equip(Eqpt::Wand) as f32).sqrt() * self.stat(Stat::Craft) as f32;
            let sling = (self.equip(Eqpt::Sling) as f32).sqrt() * self.stat(Stat::Sense) as f32;
            (bow+wand+sling) as u16
        } else {
            let brawn = self.stat(Stat::Brawn) + 2*self.equip(Eqpt::Spear) + self.equip(Eqpt::Knife) + self.equip(Eqpt::Staff) + self.equip(Eqpt::Dagger);
            brawn as u16
        }
    }
    pub fn vulnerability(&self) -> f32 {
        let spe = self.stat(Stat::Speed) as f32;
        let def = (self.stat(Stat::Brawn) + self.equip(Eqpt::Byrnie) + self.equip(Eqpt::Shield) + self.equip(Eqpt::Coat) + self.equip(Eqpt::Cape)) as f32;
        0.5 / (spe * spe / 6. + def)
    }
    pub fn absorption(&self) -> f32 {
        0.1 * (self.stat(Stat::Brawn) + 2*self.equip(Eqpt::Byrnie) + self.equip(Eqpt::Coat) + self.equip(Eqpt::Robe) + self.equip(Eqpt::Cape)) as f32
    }

    pub fn eqbase(&self) -> u8 { self.job as u8 * 4 }
    pub fn equip<T : enum_map::Enum>(&self, e : T) -> u8 { *self.equal.val.get(e.into_usize().wrapping_sub(self.eqbase() as usize)).unwrap_or(&0) }

    pub fn goods<T : enum_map::Enum>(&self, g : T) -> u16 { self.goods[g.into_usize()] }
    pub fn loot<T : enum_map::Enum + Copy>(&mut self, g : T, d : i16) {
        self.goods[g.into_usize()] = if d < 0 { self.goods[g.into_usize()].saturating_sub((-d) as u16) } else { self.goods[g.into_usize()] + d as u16 }
    }

    pub fn fuss<T : enum_map::Enum>(&self, f : T) -> f32 { self.fuss[f.into_usize()] }
    pub fn inflict<T : enum_map::Enum + Copy>(&mut self, f : T, d : f32) {
        self.fuss[f.into_usize()] += d;
        if self.fuss[f.into_usize()] < 0. { self.fuss[f.into_usize()] = 0. }
    }

    // ACTIONS
    pub fn doing(&self) -> Option<Action> { self.act }
    pub fn stop_action(&mut self) { self.act = None }
    fn activity_str(&self) -> String {
        match self.act {
            Some(a) => { String::from(ACT_NAMES[a as usize]) + " " + &self.act_info }
            None => { String::from(MOT_NAMES[self.mot as usize]) }
        }
    }

    pub fn aiming_at(&mut self, t : &str) { self.act = Some(Action::Aiming); self.act_info = String::from("at ") + t; }

    pub fn rest(&mut self) -> Then {
        let tired = self.fuss(Fuss::Winded);
        let recovery = rnd::rand(tired, 30.*tired);
        self.inflict(Fuss::Winded, -recovery/100.);
        if tired > 1. { ui::message((8,8,12), "You take a breather") }
        let hurt = self.fuss(Fuss::Bashed);
        let healed = rnd::rand(hurt, 8.*hurt);
        self.inflict(Fuss::Bashed, -healed/100.);
        if hurt > 1. { ui::message((8,8,12), "You tend to your wounds") }
        self.inflict(Fuss::Hungry, 0.2);
        Then::CarryOn(50.0) // TODO revise
    }

    pub fn sleep_start(&mut self) -> Then {
        let sleepy = self.fuss(Fuss::Sleepy);
        if sleepy < 2. { ui::message((8,8,12), "Not particularly sleepy"); return Then::CarryOn(0.) }
        self.act = Some(Action::Sleeping);
        ui::message((8,8,12), "You take a nap...");
        Then::CarryOn(10.0)
    }

    pub fn sleep_continue(&mut self) -> Then {
        let sleepy = self.fuss(Fuss::Sleepy);
        if sleepy < 2. { ui::message((8,8,12), "You wake up"); self.stop_action(); return Then::CarryOn(0.) }
        let slept = rnd::rand(sleepy, 50.*sleepy);
        self.inflict(Fuss::Sleepy, -slept/10000.);
        self.inflict(Fuss::Winded, -0.1);
        let hurt = self.fuss(Fuss::Bashed);
        let healed = rnd::rand(hurt, 25.*hurt);
        self.inflict(Fuss::Bashed, -healed/20000.);
        Then::CarryOn(10.0)
    }

    pub fn eat(&mut self) -> Then {
        if self.goods(Good::Food) == 0 { ui::message((8,8,12), "You have no food!"); return Then::CarryOn(0.) }
        let hungry = self.fuss(Fuss::Hungry);
        if hungry < 4. { ui::message((8,8,12), "Not particularly hungry"); return Then::CarryOn(0.) }
        self.inflict(Fuss::Hungry, -4.);
        self.loot(Good::Food, -1);
        ui::message((8,8,12), "You take a bite");
        Then::CarryOn(20.0) // TODO revise
    }

    pub fn drink(&mut self) -> Then {
        if self.goods(Good::Meds) == 0 { ui::message((8,8,12), "You have no potions!"); return Then::CarryOn(0.) }
        let hurt = self.fuss(Fuss::Bashed);
        let healed = rnd::rand(hurt, 5.*hurt) + rnd::rand(hurt, 5.*hurt) + rnd::rand(hurt, 5.*hurt);
        self.inflict(Fuss::Bashed, -healed/12.);
        self.loot(Good::Meds, -1);
        ui::message((8,8,12), "You take a potion");
        Then::CarryOn(5.0) // TODO revise
    }

    pub fn get_loot(&mut self, mob : &crate::mobs::Mob) {
        fn items(n : i16) -> i16 {
            if n <= 0 {
                rnd::rand(0, -n)
            } else {
                if rnd::rand(1, n as usize) == 1 { 1 } else { 0 }
            }
        }
        fn dice(n : i16) -> i16 {
            let mut result = 0;
            for _i in 0..n { result += rnd::rand(1, 6); }
            result
        }
        fn gain(pl : &mut Player, which : Good, gain : i16) -> String {
            pl.loot(which, gain);
            if gain > 0 { format!("{}: {}", GOOD_NAMES[which as usize], gain ) } else { String::new() }
        }
        let mut gains = String::new();
        for s in &[gain(self, Good::Food, items(mob.loot()[0])), gain(self, Good::Meds, items(mob.loot()[1])),
            gain(self, Good::Gold, dice(mob.loot()[2])),  gain(self, Good::Gems, items(mob.loot()[3]))] {
            if !gains.is_empty() && !s.is_empty() { gains += ", "; }
            gains += s;
        }
        let msg = String::from("You gained ") + &gains;
        ui::message((15,7,14), &msg);
    }

    pub fn stat_display(&self) {
        fn stat_col(temp : u8, stat : u8) -> ui::Col { if temp < stat { (14,9,9) } else if temp > stat { (8,15,10) } else { (13,13,12) } }
        fn fuss_col(fuss : f32) -> ui::Col { (num::clamp(3. + fuss*0.66, 0., 15.) as u8, num::clamp(17. - fuss*0.66, 0., 15.) as u8, 5) }
        ui::foreground((12,12,12));
        ui::background((0,0,0));
        ui::print(0, 0, &format!("Name: {} - Race: {} - Job: {}", &self.name, RACE_NAMES[self.race], JOB_NAMES[self.job]));
        ui::print(0, 1, &format!("Mineshaft {} - Depth {} - {:<30}", self.dunglev().shaft + 1, self.dunglev().depth, self.activity_str()));
        ui::foreground((9,12,14));
        ui::print(0, 22, "===Stats===  Eqmt Qual  ==Supplies==  Condition");
        for i in 0u8..=3 {
            ui::foreground(stat_col(self.stat(i).into(), self.statbase(i.into()).into()));
            ui::print(0, (23+i).into(), &format!("{} {:2}/{:<2}", STAT_NAMES[i as usize], self.stat(i), self.statbase(i.into())));
            ui::foreground((12,12,12));
            let eq = self.eqbase() + i;
            ui::print(13, (23+i).into(), &format!("{:6} {:+2}", EQUI_NAMES[eq as usize], self.equip(eq)));
            ui::print(24, (23+i).into(), &format!("{} {:4}", GOOD_NAMES[i as usize], self.goods(i)));
            ui::foreground(fuss_col(self.fuss(i)));
            ui::print(38, (23+i).into(), &format!("{} {:2.0}", FUSS_NAMES[i as usize], self.fuss(i)));
        }
    }
}

pub fn char_create() -> Result<Player, ui::Para> {

    fn calc_stats(race : usize, job : usize) -> Result<Stats, ui::Para> {
        'calc: loop {
            let mut stats = gen_stats(race, job);
            let mut bpt = 2;
            let bonus = 0;
            ui::base_colour();
            while bpt > 0 {
                match ui::menu(0, 0, &format!("Stat to add +{}", bpt), bonus, &[
                    &format!("{:6} {:2}", STAT_NAMES[0], &stats[0]),
                    &format!("{:6} {:2}", STAT_NAMES[1], &stats[1]),
                    &format!("{:6} {:2}", STAT_NAMES[2], &stats[2]),
                    &format!("{:6} {:2}", STAT_NAMES[3], &stats[3]),
                ]) {
                    Err(ui::Para::Quit) => { return Err(ui::Para::Quit) }
                    Err(ui::Para::Back) => { if bpt == 2 { return Err(ui::Para::Back) } else { continue 'calc } }
                    Ok(bonus) => { stats[bonus as usize] += bpt; bpt -= 1; }
                }
            }
            ui::print(8, 2 + bonus, &format!("{:2}", stats[bonus as usize]));
            ui::flush();
            return Ok(stats)
        }
    }

    fn create_stats(race: usize, job: usize) -> Result<Player, ui::Para> {
        'stats: loop {
            ui::cls();
            let stats = calc_stats(race, job)?;
            loop {
                match ui::query(0, 21, "Name:", 20) {
                    None => { continue 'stats }
                    Some(name) => {
                        if name.trim().is_empty() { continue }
                        let pl = Player::new(name, race, job, stats, [20, 1, 20, 1]);
                        return Ok(pl);
                    }
                }
            }
        }
    }

    let mut race : usize = 0;
    'pick_race: loop {
        ui::cls();
        race = ui::menu(0, 0, "Choose Race", race, &RACE_NAMES)?;
        let job = 0;
        loop {
            ui::cls();
            match ui::menu(0, 0, "Choose Job", job, &JOB_NAMES) {
                Err(ui::Para::Quit) => { return Err(ui::Para::Quit) }
                Err(ui::Para::Back) => { continue 'pick_race }
                Ok(job) => {
                    match create_stats(race as usize, job as usize) {
                        Err(ui::Para::Quit) => { return Err(ui::Para::Quit); }
                        Err(ui::Para::Back) => { continue }
                        Ok(pl) => { return Ok(pl); }
                    }
                }
            }
        }
    }
}
