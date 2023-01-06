use std::{ops, cmp};
use crate::ui;
use crate::{rnd, player, State, Entrypoint, Then};
use crate::util::{Pos, Level};
use lazy_static::lazy_static;
use serde::{Serialize, Deserialize};
use enum_map;

type Func = fn(&mut State) -> crate::Then;

pub struct Base {
    pub name : &'static str, pub back : ui::Col, pub glyph : char, pub col : ui::Col, pass : bool,
}

#[derive(Clone, Copy, enum_map::Enum, Serialize, Deserialize)]
pub enum TB {
    Void, Rock, Floor
}

lazy_static! {
    pub static ref TILE_BASES : enum_map::EnumMap<TB, Base> = enum_map::enum_map! {
        TB::Void  => Base{ name: "void",  back: (0,0,0), glyph: ' ', col: (0,0,0), pass: false},
        TB::Rock  => Base{ name: "rock",  back: (1,1,1), glyph: '#', col: (3,3,3), pass: false},
        TB::Floor => Base{ name: "floor", back: (3,1,0), glyph: '·', col: (0,0,0), pass: true},
    };
}

pub struct Feature {
    pub name : &'static str, pub glyph : char, pub col : ui::Col, pass : bool, act : Func
}

impl Feature {
    pub fn new(name : &'static str, glyph : char, col: ui::Col, pass: bool, act : Option<Func>) -> Feature {
        fn nop(_ : &mut State) -> crate::Then { crate::Then::CarryOn(0.01) }
        Feature { name, glyph, col, pass, act:act.unwrap_or(nop) }}
}

#[derive(Clone, Copy, cmp::PartialEq, enum_map::Enum, Serialize, Deserialize)]
pub enum TF {
    Nil, StairDn, StairUp, TunnelFw, TunnelBk, TownDwr, TownGnm, TownHob, TownPix
}

fn visit_inn(game: &mut State) -> Then {
    let depth= game.pc.dunglev().depth as u16;
    let price = depth * depth + depth * 5;
    match game.menu(&format!("That will be ﬂ{}", price), 0, &["Stay", "Leave"]) {
        Err(ui::Para::Quit) => { game.save_game(); return Then::Quit }
        Err(ui::Para::Back) | Ok(1) => { return Then::CarryOn(0.) }
        _ => ()
    }
    let player = &mut game.pc;
    if player.goods(player::Good::Gold) < price { ui::message((8,8,12), "You lack the funds!"); return Then::CarryOn(0.) }
    player.loot(player::Good::Gold, -(price as i16));

    player.inflict(player::Fuss::Sleepy, -99.);
    player.inflict(player::Fuss::Hungry, -99.);
    let hurt = player.fuss(player::Fuss::Bashed);
    let healed = rnd::rand(hurt, 50.*hurt);
    player.inflict(player::Fuss::Bashed, -healed/100.);
    ui::message((8,8,12), "You sleep on a comfortable bed and eat a hearty breakfast");
    Then::CarryOn(0.)
}

fn visit_town(game: &mut State, race : player::Race) -> Then {
    game.pc.inflict(player::Fuss::Winded, -99.);
    ui::message((12,12,12), &format!("The {}s welcome you!", player::RACE_NAMES[race as usize]));
    game.render_msgs();
    let shops = ["trader", "supplier", "trainer", "wizard"];
    loop {
        game.pc.stat_display();
        match game.menu(&format!("{} town", player::RACE_NAMES[race as usize]),
                        0, &["Visit inn", &format!("Visit {}", shops[race as usize]), "Leave Town"]) {
            Err(ui::Para::Quit) => { game.save_game(); return Then::Quit }
            Ok(2) | Err(ui::Para::Back) => { game.render_msgs(); return Then::CarryOn(10.0) } // TODO revise
            Ok(0) => { match visit_inn(game) { Then::Quit => return Then::Quit, _ => () } }
            Ok(1) => {}
            _ => { panic!("Coding error") }
        }
    }
}

lazy_static! {
    pub static ref TILE_FEATURES : enum_map::EnumMap<TF, Feature> = enum_map::enum_map! {
        TF::Nil =>     Feature::new("nothing", ' ', (0,0,0), false, None),
        TF::StairDn => Feature::new("stair down", '>', (9,7,6), true, Some(|s:&mut State| { s.enter_level(s.pc.dunglev().down(), Entrypoint::FromAbove); crate::Then::CarryOn(0.) })),
        TF::StairUp => Feature::new("stair up", '<', (9,7,6), true, Some(|s:&mut State| { match s.pc.dunglev() {
                Level{depth:1,shaft:_} => { s.pc.exit(); crate::Then::Start }
                l => { s.enter_level(l.up(), Entrypoint::FromBelow); crate::Then::CarryOn(0.) }
        }})),
        TF::TunnelFw => Feature::new("tunnel", 'O', (9,7,6), true, Some(|s:&mut State| { s.enter_level(s.pc.dunglev().fore(), Entrypoint::FromBack); crate::Then::CarryOn(0.) })),
        TF::TunnelBk => Feature::new("tunnel", 'O', (9,7,6), true, Some(|s:&mut State| { s.enter_level(s.pc.dunglev().back(), Entrypoint::FromFore); crate::Then::CarryOn(0.) })),
        TF::TownDwr => Feature::new("dwarf town gate",  'Π', (8,8,8),  true, Some(|s:&mut State| { visit_town(s, player::Race::Dwarf) })),
        TF::TownHob => Feature::new("hobbit town gate", 'Ω', (12,6,0), true, Some(|s:&mut State| { visit_town(s, player::Race::Hobbit) })),
        TF::TownGnm => Feature::new("gnome town gate",  'Λ', (8,2,15), true, Some(|s:&mut State| { visit_town(s, player::Race::Gnome) })),
        TF::TownPix => Feature::new("pixie town gate",  'Л', (6,10,0), true, Some(|s:&mut State| { visit_town(s, player::Race::Pixie) })),
    };
}

#[derive(Clone, Copy, Serialize, Deserialize)]
pub struct Tile {
    pub typ: TB,
    pub fea: TF,
}

impl Tile {
    pub fn passable(&self) -> bool { TILE_FEATURES[self.fea].pass || TILE_BASES[self.typ].pass }
    pub fn activate(&self, s: &mut State) -> crate::Then { (TILE_FEATURES[self.fea].act)(s) }
    pub fn desc(&self) -> String {
        let fea_desc = if self.fea == TF::Nil { String::new() } else { String::from(TILE_FEATURES[self.fea].name) + ", " };
        fea_desc + TILE_BASES[self.typ].name
    }
}

#[derive(Serialize, Deserialize)]
pub struct Field {
    tiles : Vec<Tile>
}

impl Field {
    pub fn width(&self) -> usize { 40 }
    pub fn height(&self) -> usize { 20 }
}

impl Default for Field {
    fn default() -> Self { Self { tiles: vec![Tile { typ:TB::Void, fea:TF::Nil}; 800] } }
}

impl ops::Index<Pos> for Field {
    type Output = Tile;
    fn index(&self, p: Pos) -> &Self::Output { &self.tiles[(p.0*self.width()+p.1) as usize] }
}

impl ops::IndexMut<Pos> for Field {
    fn index_mut(&mut self, p: Pos) -> &mut Self::Output { let w = self.width(); &mut self.tiles[(p.0*w+p.1) as usize] }
}

#[derive(Serialize, Deserialize)]
pub struct Room {
    pub lx : usize, pub rx : usize, pub ty : usize, pub by : usize
}

#[derive(Default, Serialize, Deserialize)]
pub struct Map {
    pub tiles: Field,
    pub rooms: Vec<Room>,
    pub stup: Pos,
    pub stdn: Pos,
    pub tnbk: Option<Pos>,
    pub tnfw: Option<Pos>,
    pub town: Option<Pos>,
}
