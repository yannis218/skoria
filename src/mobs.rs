use crate::{ui, dungeon};
use crate::util::{Pos, Level, Location};
use lazy_static::lazy_static;
use serde::{Serialize, Deserialize};
use enum_map;
use std::collections::HashMap;

#[derive(Serialize, Deserialize)]
pub struct PopRec {
    mobs : Vec<Mob>,
    current_popn : usize,
    target_popn : usize,
}

type PopMapBase = HashMap<Level, PopRec>;
#[derive(Serialize, Deserialize)]
pub struct PopMap(PopMapBase);

impl PopMap {
    pub fn new() -> PopMap { PopMap(PopMapBase::new()) }
    pub fn popn_add(&mut self, lev : Level, d : i32) { self.0.get_mut(&lev).unwrap().current_popn = self.0.get(&lev).unwrap().current_popn.wrapping_add(d as usize) }
    pub fn get_all(&self, lev : Level) -> &Vec<Mob> { &self.0.get(&lev).unwrap().mobs }
    pub fn get_all_mut(&mut self, lev : Level) -> &mut Vec<Mob> { &mut self.0.get_mut(&lev).unwrap().mobs }
    pub fn get(&self, loc : Location) -> Option<&Mob> { self.0.get(&loc.lev).unwrap().mobs.iter().find(|m| { m.pos == loc.pos }) }
    pub fn get_idx(&self, loc : Location) -> Option<usize> { self.0.get(&loc.lev).unwrap().mobs.iter().enumerate().find(|(_, m)| { m.pos == loc.pos }).map(|(i, _)| {i}) }
    pub fn occupancy(&self, lev : Level, p0 : Pos, p1 : Pos) -> Vec<Pos> {
        self.0.get(&lev).unwrap().mobs.iter().filter_map(|m|{
            if (p0.0..=p1.0).contains(&m.pos.0) && (p0.1..=p1.1).contains(&m.pos.1) { Some(m.pos) } else { None }
        }).collect()
    }
}

pub struct Creature {
    pub name : &'static str, pub glyph : char, pub col : ui::Col, pub peril : u8, pub speed : u8, life : u16, loot: [i16; 4]
}

#[derive(Clone, Copy, enum_map::Enum, Serialize, Deserialize)]
pub enum Cr {
    Nemo, Rat, Kobold, Goblin, Orc, Troll,
}

lazy_static! {
    pub static ref CREATURES : enum_map::EnumMap<Cr, Creature> = enum_map::enum_map! {
    //                                                                              max 12
        Cr::Nemo   => Creature{ name: "nobody", glyph: ' ', col: (0,0,0), peril:0,  speed:0,  life:0,   loot:[ 0, 0, 0, 0]},
        Cr::Rat    => Creature{ name: "rat",    glyph: 'r', col: (8,5,1), peril:1,  speed:11, life:4,   loot:[ 1, 0, 0, 0]},
        Cr::Kobold => Creature{ name: "kobold", glyph: 'k', col: (9,7,5), peril:2,  speed:9,  life:10,  loot:[ 0, 2, 1, 0]},
        Cr::Goblin => Creature{ name: "goblin", glyph: 'g', col: (6,9,4), peril:4,  speed:7,  life:35,  loot:[-1, 0, 2,-1]},
        Cr::Orc    => Creature{ name: "orc",    glyph: 'o', col: (6,8,8), peril:6,  speed:5,  life:90,  loot:[-2, 3, 2, 0]},
        Cr::Troll  => Creature{ name: "troll",  glyph: 'T', col: (8,9,4), peril:8,  speed:3,  life:230, loot:[-5,-1, 8,-3]},
    };
}

#[derive(Serialize, Deserialize)]
pub struct Mob {
    pub typ : Cr,
    pub pos : Pos,
    pub turn: f32,
    pub life : u16,
}

impl Mob {
    pub fn new(typ : Cr, pos : Pos) -> Mob { Mob { typ, pos, turn:0., life: CREATURES[typ].life } }
    pub fn name(&self) -> &str {
        let typ = &CREATURES[self.typ];
        return typ.name;
    }
    pub fn loot(&self) -> [i16; 4] {
        let typ = &CREATURES[self.typ];
        return typ.loot;
    }
}

use crate::util::rnd;
pub fn remove_some(rec : &mut Vec<Mob>) {
    let popn = rec.len();
    let to_remove = (popn + 5)/6;
    print!("remove_some: removing {} of {}; ", to_remove, popn);
    for _i in 0..to_remove {
        rec.remove(rnd::rand(0, rec.len()-1));
    }
}

fn popn_rejig(state: &mut crate::State) {
    state.mobs.0.retain(|lev, rec| {
        print!("popn_rejig for level {:?}; ", lev);
        remove_some(&mut rec.mobs); // Some mobs depart
        print!("popn {}, target {}; ", rec.current_popn, rec.target_popn);
        if rec.current_popn < rec.target_popn {
            println!("added 1 to popn");
            rec.current_popn += 1;
            true
        } else {
            let retain = !rec.mobs.is_empty();
            println!("popn up to level, {} retained mobs", rec.mobs.len() );
            retain
        }
    });
}

fn record_for_here(mobs: &mut PopMap, lev : Level, target_popn : usize) -> &mut PopRec {
    mobs.0.entry(lev).or_insert(PopRec{mobs:Vec::new(), current_popn:target_popn, target_popn})
}

pub fn find_empty_space(dung : &dungeon::Map, popn : &PopRec) -> Option<Pos>{
    let dungh = dung.tiles.height();
    let dungw = dung.tiles.width();
    for _i in 0..1000 {
        let p = crate::Pos(rnd::rand(1, dungh - 2), rnd::rand(1, dungw - 2));
        if !dung.tiles[p].passable() { continue }
        if dung.tiles[p].fea != dungeon::TF::Nil { continue }
        if popn.mobs.iter().find(|m|{m.pos == p}).is_some() { continue }
        return Some(p)
    }
    return None
}

pub fn repopulate(state: &mut crate::State, lev : Level) {
    let target_popn : usize = rnd::rand(7, 11); // This is the last time that the rands are predictable.  Coincidence oriented programming!
    popn_rejig(state);
    let here = record_for_here(&mut state.mobs, lev, target_popn);

    let to_gen = here.current_popn - here.mobs.len();
    print!("Already here: {} {} {}", target_popn, here.current_popn, here.mobs.len());
    for m in &here.mobs {
        print!(" - {} at {:?}", CREATURES[m.typ].name, m.pos);
    }
    println!();
    print!("About to gen {} new mobs:", to_gen);
    let candidates : Vec<Cr> = CREATURES.iter().skip(1).filter(|c|{c.1.peril<=lev.depth}).map(|c|{c.0}).collect();
    assert!(! candidates.is_empty());
    for _i in 0..to_gen {
        if let Some(pos) = find_empty_space(&state.dung, &here) {
            let m = Mob::new(*rnd::pick(candidates.iter()), pos);
            print!(" - {} at {:?}", m.name(), m.pos);
            here.mobs.push(m);
        }
    }
    println!();
}
