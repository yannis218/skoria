mod ui;
mod util;
mod player;
mod dungeon;
mod mapgen;
mod mobs;
#[macro_use] extern crate enum_primitive;


use std::{fs, io, path};
use serde::{Serialize, Deserialize};
use util::{Level, Pos, rnd};
use crate::util::Location;

//use std::time::Instant;

#[derive(Serialize, Deserialize)]
pub struct State {
    pc: player::Player,
    dung : dungeon::Map,
    mobs : mobs::PopMap,
    needs_timer : f32,
    time : u64, rem : f32, // in deciseconds
}

pub enum Then { CarryOn(f32), Start, Quit }

pub enum Entrypoint { FromAbove, FromBelow, FromBack, FromFore }

impl State {
    pub fn new(pc: player::Player) -> State { State { pc, dung: Default::default(), mobs: mobs::PopMap::new(), needs_timer:0., time:0, rem:0. } }
    pub fn test() -> State {
        let mut state = State::new(player::Player::test());
        let lev = state.pc.dunglev();
        mobs::repopulate(&mut state, lev);
        state
    }


    fn folder() -> io::Result<path::PathBuf> {
        let mut path = dirs::home_dir().unwrap_or_default();
        path.push(".skoria");
        fs::create_dir_all(&path)?;
        Ok(path)
    }

    fn save(&self) -> io::Result<()> {
        let mut path = State::folder()?;
        path.push(base64::encode_config(&self.pc.name, base64::URL_SAFE_NO_PAD));
        fs::write(&path, bincode::serialize(&self).unwrap())?;
        Ok(())
    }

    fn get_saves() -> io::Result<Vec<String>> {
        fn decode(coded: &str) -> Option<String> {
            let dec = base64::decode_config(coded, base64::URL_SAFE_NO_PAD);
            if dec.is_err() { return None };
            let name = String::from_utf8(dec.unwrap());
            name.ok()
        }
        let mut results : Vec<String> = vec![];
        let mut proc = |e : fs::DirEntry| {
            e.file_name().to_str().and_then(|f| {
                if let Some(nm) = decode(f) { results.push(nm) }
                Some(())
            })
        };
        let path = State::folder()?;
        for entry in fs::read_dir(&path)? { entry.and_then(|e| { proc(e); Ok(())}).ok(); }
        Ok(results)
    }

    fn load(name : &str) -> io::Result<State> {
        let mut path = State::folder()?;
        path.push(base64::encode_config(name, base64::URL_SAFE_NO_PAD));
        let data = fs::read(&path)?;
        bincode::deserialize::<State>(data.as_slice()).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e)) //?;
    }

    pub fn save_game(&self) {
        self.save().unwrap_or_else(|e|{println!("Error Saving Game!\n{}", e); });
    }

    pub fn gen_map(&mut self) {
        let lev = self.pc.dunglev();
        mapgen::run(&mut self.dung, lev);
    }

    pub fn enter_level(&mut self, lev : Level, from : Entrypoint) {
        let msg = match from {
            Entrypoint::FromAbove => { "You descend the stair" }
            Entrypoint::FromBelow => { "You ascend the stair" }
            Entrypoint::FromBack | Entrypoint::FromFore => { "You follow the tunnel" }
        };
        ui::message((10,10,10), msg);
        println!("====== Generating level {:?} ======", lev);
        mapgen::run(&mut self.dung, lev);
        mobs::repopulate(self, lev);
        let pos = match from { Entrypoint::FromAbove => { self.dung.stup }, Entrypoint::FromBelow => { self.dung.stdn },
            Entrypoint::FromBack => { self.dung.tnbk.unwrap() }, Entrypoint::FromFore => { self.dung.tnfw.unwrap() } };
        self.pc.goto(lev, pos);
    }

    fn render_tile(&self, rc: Pos, clear : bool) {
        if clear { ui::clear(rc.1, 2+rc.0, 1, 1); ui::transparent(); }
        let typ = &dungeon::TILE_BASES[self.dung.tiles[rc].typ];
        ui::background(typ.back);
        ui::foreground(typ.col);
        ui::putc(rc.1, 2+rc.0, typ.glyph);
        let fea = self.dung.tiles[rc].fea;
        if fea == dungeon::TF::Nil { return }
        let fea = &dungeon::TILE_FEATURES[fea];
        ui::foreground(fea.col);
        ui::putc(rc.1, 2+rc.0, fea.glyph);
    }

    fn render_view(&self) {
        let dungh = self.dung.tiles.height() as u32;
        let dungw = self.dung.tiles.width() as u32;
        ui::clear(0, 2, dungw, dungh);
        ui::transparent();
        for r in 0..dungh { for c in 0..dungw { self.render_tile(Pos(r, c), false); } }
        ui::opaque();
        ui::base_colour();
    }

    fn render_mobs(&self) {
        let lev = self.pc.dunglev();
        for mob in self.mobs.get_all(lev) {
            let floor = &dungeon::TILE_BASES[self.dung.tiles[mob.pos].typ];
            ui::background(floor.back);
            let typ = &mobs::CREATURES[mob.typ];
            ui::foreground(typ.col);
            ui::putc(mob.pos.1, 2 + mob.pos.0, typ.glyph);
        }
        let p = self.pc.mappos();
        let floor = &dungeon::TILE_BASES[self.dung.tiles[p].typ];
        ui::background(floor.back);
        ui::foreground((10, 6, 14));
        ui::putc(p.1, 2 + p.0, '@');
        ui::base_colour();
    }

    fn render_msgs(&self) {
        let dungh = self.dung.tiles.height() as u32;
        let dungw = self.dung.tiles.width() as u32;
        ui::display_messages(dungw+1, 2, dungh);
    }

    fn render_all(&self) {
        self.pc.stat_display();
        self.render_view();
        self.render_mobs();
        self.render_msgs();
        //let before = Instant::now();
        ui::flush();
        //println!("Flush took: {:.2?}", before.elapsed());
    }

    fn action_mob(&mut self, mobi : usize) -> bool { // true = refresh display
        let player = &mut self.pc;
        let mob = self.mobs.get_all_mut(player.dunglev()).get_mut(mobi).unwrap();
        let typ = &mobs::CREATURES[mob.typ];
        let pl_pos = player.mappos();
        mob.turn += 36.0 / typ.speed as f32; // TODO revise

        // Attack if pc adjacent
        let dd = mob.pos - pl_pos;
        if dd.len() <= 2 {
            ui::message((14,10,6), &format!("The {} strikes you", typ.name));
            let ave_dmg = typ.peril as u32 * typ.peril as u32 + typ.peril as u32 * 10;
            let base_dmg = rnd::rand(0, ave_dmg) + rnd::rand(0, ave_dmg);
            let dmg = num::clamp(base_dmg as f32 * player.vulnerability() - player.absorption(), 0., 30.);
            player.inflict(player::Fuss::Bashed, dmg);
            true
        } else { false }
    }

    fn attack(&mut self, loc : util::Location, mi : usize, ranged : bool) -> Then {
        {
            let m = &mut self.mobs.get_all_mut(loc.lev)[mi];
            ui::message((10, 13, 8), &format!("You strike the {}", m.name()));
            self.pc.inflict(player::Fuss::Winded, 0.5); // TODO this should depend of ranged
            let dmgmax = self.pc.dmg_potential(ranged);
            let dmg = rnd::rand(1, dmgmax) + rnd::rand(1, dmgmax);
            m.life = m.life.saturating_sub(dmg);
            if m.life > 0 { return Then::CarryOn(self.pc.attack_time(ranged)); }
            ui::message((8, 14, 5), &format!("You have killed the {}", m.name()));
            self.pc.get_loot(m);
        }
        self.mobs.get_all_mut(loc.lev).retain(|m| { m.pos != loc.pos });
        self.mobs.popn_add(loc.lev, -1);
        Then::CarryOn(10.0) // TODO revise
    }

    fn try_move(&mut self, dr : i32, dc : i32) -> Then {
        let d = util::Dist(dr, dc);
        let pc = &mut self.pc;
        let pos = pc.mappos() + d;
        if ! self.dung.tiles[pos].passable() { return Then::CarryOn(0.1) }
        let lev = pc.dunglev();
        let mi = {
            let mob = self.mobs.get_all(lev).iter().enumerate().find(|m| { m.1.pos == pos });
            if mob.is_none() {
                pc.repos(pos);
                pc.inflict(player::Fuss::Winded, pc.move_fatigue());
                let fps = pc.speed_fps();
                let dist = if d.len() == 2 { 7. } else { 5. };
                return Then::CarryOn(dist / fps);
            }
            mob.unwrap().0
        };
        self.attack(util::Location{ lev, pos }, mi, false)
    }

    fn activate(&mut self) -> Then {
        let pos= self.pc.mappos();
        let tile : dungeon::Tile = self.dung.tiles[pos];
        tile.activate(self)
    }

    pub fn menu<T: AsRef<str>>(&self, title : &str, init : u32, options : &[T]) -> Result<u32, ui::Para> {
        let dungw = self.dung.tiles.width();
        ui::base_colour();
        let result = ui::menu(2+dungw as u32, 2, title, init, options);
        result
    }

    fn game_menu(&mut self) -> Then {
        let then = match self.menu("Actions", 0, &["Eat a ration", "Sleep a bit", "Drink a potion", "Save & Quit"]) {
            Err(ui::Para::Quit) => { self.save_game(); return Then::Quit }
            Ok(2) => { self.pc.drink() }, Ok(1) => { self.pc.sleep_start() }, Ok(0) => { self.pc.eat() }
            Ok(3) => { self.save_game(); return Then::Start }
            _ => Then::CarryOn(0.)
        };
        then
    }

    fn done_for(&self, msg : &str) -> Then {
        ui::single_message((15,3,2), msg);
        self.render_all();
        ui::sleepms(5000);
        return Then::Start
    }

    fn check_fuss(&mut self) -> Then {
        let player = &self.pc;
        let mut msg = String::new();

        let hurt = player.fuss(player::Fuss::Bashed);
        if hurt > 24. { return self.done_for("You succumb to your wounds!"); }
        if hurt > 12. { msg += "You are bleeding! "; }

        let winded = player.fuss(player::Fuss::Winded);
        if winded > 24. { return self.done_for("You lose consciousness from fatigue!"); }
        if winded > 12. { msg += if winded > 20. { "You are faint from fatigue!" } else { "You need some rest! " }; }

        let hungry = player.fuss(player::Fuss::Hungry);
        if hungry > 24. { return self.done_for("You lose consciousness from hunger!"); }
        if hungry > 12. { msg += if hungry > 20. { "You are faint from hunger!" } else { "You need some food! " }; }

        let sleepy = player.fuss(player::Fuss::Sleepy);
        if hungry > 24. { return self.done_for("You wander around in a delirium!"); }
        if sleepy > 12. { msg += "You need some sleep!"; }

        if ! msg.is_empty() { ui::single_message((13,13,0), &msg); self.render_msgs(); }
        Then::CarryOn(0.)
    }

    fn bres2(p0 : Pos, p1 : Pos) -> impl std::iter::Iterator<Item = Pos> {
        let dx = num::abs(p1.1 as i32 - p0.1 as i32);
        let dy = num::abs(p1.0 as i32 - p0.0 as i32);
        let sx = num::signum(p1.1 as i32 - p0.1 as i32);
        let sy = num::signum(p1.0 as i32 - p0.0 as i32);
        let mut err = dx-dy;
        let mut p = p0;
        let mut done = false;
        std::iter::from_fn(move || {
            if done { return None }
            let e2 = 2*err;
            if e2 > -dy { err -= dy; p += util::Dist(0, sx) }
            if e2 < dx { err += dx; p += util::Dist(sy, 0) }
            if p == p1 { done = true }
            Some(p)
        })
    }

    fn los(&self, from : Location, unto : Location) -> Result<Pos, Pos> {
        assert!(from.lev == unto.lev);
        let occ = self.mobs.occupancy(from.lev, from.pos, unto.pos);
        for pos in State::bres2(from.pos, unto.pos) {
            if ! self.dung.tiles[pos].passable() { return Err(pos) }
            if occ.contains(&pos) && pos != unto.pos { return Err(pos) }
        }
        Ok(unto.pos)
    }

    fn shoot(&mut self, pos: Pos) -> Then {
        let lev = self.pc.dunglev();
        let loc = Location{ lev, pos };
        let mi = match self.los(self.pc.location(), loc) {
            Ok(_) => { match self.mobs.get_idx(loc) { Some(mi) => { mi }  None => { return Then::CarryOn(1.) } } }
            Err(pos) => { match self.mobs.get_idx(Location{lev, pos}) {
                    Some(mi) => { mi }  None => { ui::message((13, 10, 8), &format!("You hit {}", self.dung.tiles[pos].desc())); return Then::CarryOn(1.) }
            } }
        };
        self.attack(loc, mi, true)
    }

    fn target(&mut self) -> Result<Pos, ui::Para> { // TODO: Messy; refactor.
        let dungh = self.dung.tiles.height() as u32;
        let dungw = self.dung.tiles.width() as u32;
        let render = |p:Pos, typ : dungeon::TB, c : ui::Col| {
            let typ = &dungeon::TILE_BASES[typ];
            ui::background(typ.back);
            ui::foreground(c);
            ui::putc(p.1, 2 + p.0, '*');
        };
        let upup = |p:Pos| { if p.0 > 0 { util::Dist::north() } else { util::Dist(0, 0) } };
        let down = |p:Pos| { if p.0 < dungh-1 { util::Dist::south() } else { util::Dist(0, 0) } };
        let left = |p:Pos| { if p.1 > 0 { util::Dist::west() } else { util::Dist(0, 0) } };
        let rght = |p:Pos| { if p.1 < dungw-1 { util::Dist::east() } else { util::Dist(0, 0) } };

        let mut loc = self.pc.location();
        let mut phase = true;
        loop {
            let desc = match self.mobs.get(loc) { Some(m) => { String::from(m.name()) + ", " }, None => { String::new() } } + &self.dung.tiles[loc.pos].desc();
            self.pc.aiming_at(&desc);
            self.pc.stat_display();
            let disp_tile = |p:Pos| { self.render_tile(p, false); self.render_mobs(); };
            let hit = self.los(self.pc.location(), loc); println!("To hit {:?}", hit);
            if phase {
                let c = match hit { Err(pos) => { render(pos, self.dung.tiles[pos].typ, (10, 15, 10)); (15, 9, 9) }, _ => { (10, 15, 10) } };
                render(loc.pos, self.dung.tiles[loc.pos].typ, c);
            } else {
                disp_tile(loc.pos);
                match hit { Err(pos) => { disp_tile(pos) }, _ => {} }
            }
            ui::flush();
            let new_pos = match ui::keyms(150) {
                Some(ui::Key::Quit) => { self.pc.stop_action(); return Err(ui::Para::Quit) }, Some(ui::Key::Esc) => { self.pc.stop_action(); return Err(ui::Para::Back) }
                Some(ui::Key::Up) => { loc.pos + upup(loc.pos) }, Some(ui::Key::Dn) => { loc.pos + down(loc.pos) }
                Some(ui::Key::Lf) => { loc.pos + left(loc.pos) }, Some(ui::Key::Rg) => { loc.pos + rght(loc.pos) }
                Some(ui::Key::UL) => { loc.pos + upup(loc.pos) + left(loc.pos) }, Some(ui::Key::DL) => { loc.pos + down(loc.pos) + left(loc.pos) }
                Some(ui::Key::UR) => { loc.pos + upup(loc.pos) + rght(loc.pos) }, Some(ui::Key::DR) => { loc.pos + down(loc.pos) + rght(loc.pos) }
                Some(ui::Key::Ret) => { self.pc.stop_action(); return Ok(loc.pos) }
                _ => { loc.pos }
            };
            if new_pos != loc.pos {
                disp_tile(loc.pos);
                match hit { Err(pos) => { disp_tile(pos) }, _ => {} }
                loc.pos = new_pos
            }
            phase = ! phase;
        }
    }

    fn player_action(&mut self) -> Then {
        match self.pc.doing() {
            Some(player::Action::Sleeping) => {
                self.pc.sleep_continue();
                match ui::inkey() {
                    None => { Then::CarryOn(10.) }
                    Some(ui::Key::Quit) => { Then::Quit }
                    Some(_) => { self.pc.stop_action(); Then::CarryOn(0.) }
                }
            }
            Some(player::Action::Aiming) => { panic!("Should not happen!") }
            None => {
                match ui::key() {
                    ui::Key::Quit => { self.save_game(); return Then::Quit }
                    ui::Key::Esc => { self.game_menu() }
                    ui::Key::Up => { self.try_move(-1, 0) }
                    ui::Key::Dn => { self.try_move(1, 0) }
                    ui::Key::Lf => { self.try_move(0, -1) }
                    ui::Key::Rg => { self.try_move(0, 1) }
                    ui::Key::UL => { self.try_move(-1, -1) }
                    ui::Key::DL => { self.try_move(1, -1) }
                    ui::Key::UR => { self.try_move(-1, 1) }
                    ui::Key::DR => { self.try_move(1, 1) }
                    ui::Key::Ret => { self.activate() }
                    ui::Key::Mid => { self.pc.rest() }
                    ui::Key::Pls => { self.pc.faster(); Then::CarryOn(0.) }
                    ui::Key::Min => { self.pc.sneakier(); Then::CarryOn(0.) }
                    ui::Key::Slash => { match self.target() { Ok(p) => { self.shoot(p) }, Err(ui::Para::Quit) => { self.save_game(); Then::Quit }, _ => { Then::CarryOn(0.) } } }
                    //_ => {}
                }
            }
        }
    }

    //
    // SEQUENCING
    //

    fn seed_action(&mut self) {
        self.pc.turn = 0.;
        let lev = self.pc.dunglev();
        for mob in self.mobs.get_all_mut(lev) {
            let typ = &mobs::CREATURES[mob.typ];
            mob.turn = rnd::rand(13u16.saturating_sub(typ.speed.into()), 16u16.saturating_sub(typ.speed.into())*5) as f32
        }
    }

    fn apply_needs(&mut self) {
        match self.pc.doing() {
            Some(player::Action::Sleeping) => {
                self.pc.inflict(player::Fuss::Hungry, 0.01);
            }
            _ => {
                self.pc.inflict(player::Fuss::Hungry, 0.02);
                self.pc.inflict(player::Fuss::Sleepy, 0.01);
                if self.pc.fuss(player::Fuss::Hungry) > 12. { self.pc.inflict(player::Fuss::Winded, 0.03); }
            }
        }
    }

    fn pass_time(&mut self, delta : f32) {
        self.pc.turn -= delta;
        let lev = self.pc.dunglev();
        for mob in self.mobs.get_all_mut(lev) { mob.turn -= delta }
        self.needs_timer += delta;
        while self.needs_timer > 10.0 { self.apply_needs(); self.needs_timer -= 10.0 }

        self.rem += delta * 10.;
        if self.rem >= 1. {
            let dcs = self.rem.floor() as u32;
            self.time += dcs as u64;
            self.rem -= dcs as f32;
        }
    }

    fn next_mob_to_act(&mut self) -> Option<(usize, f32)> {
        let lev = self.pc.dunglev();
        let vec = self.mobs.get_all(lev);
        if let Some((i,mob)) = vec.iter().enumerate().min_by_key(|(_,m)|{ (1000.*m.turn).round() as i32 }) {
            if mob.turn < self.pc.turn { return Some((i, mob.turn)) }
        }
        None
    }

    pub fn play(&mut self) -> Then {
        ui::message((7,7,9), &format!("Welcome, {}, to the mines of Skoria!",self.pc.name));
        self.seed_action();
        self.needs_timer = 0.;
        self.render_all();
        loop {
            match self.check_fuss() { Then::CarryOn(_) => {} x => { return x } }
            if let Some((mob_idx, mob_act)) = self.next_mob_to_act() {
                self.pass_time(mob_act);
                if self.action_mob(mob_idx) {
                    self.render_all();
                }
            } else {
                self.pass_time(self.pc.turn);
                match self.player_action() {
                    Then::CarryOn(passed) => { self.pc.turn += passed; /*println!("passed {}", passed);*/ },
                    then => { self.save_game(); return then } }

                self.render_all();
            }
        }
    }
}

fn start_play(play : Result<State, ui::Para>) -> bool { // true == quit
    ui::purge_messages();
    ui::base_colour();
    ui::cls();
    return match play {
        Ok(mut state) => {
            if state.pc.outside() { state.enter_level(Level{depth:1, shaft:0}, Entrypoint::FromAbove) }
            else { state.gen_map() }
            match state.play() {
                Then::Quit => { true }
                _ => { false }
            }
        }
        Err(ui::Para::Quit) => { true }
        Err(_) => { false }
    }
}

fn game_load() -> Result<State, ui::Para> {
    ui::cls();
    match State::get_saves() {
        Ok(saves) => {
            if saves.is_empty() {
                let state = State::test();
                return Ok(state);
                // popup("No saved games found");
                // return false
            }
            let ch = ui::menu(0, 0, "Select Character", 0, saves.as_slice())?;
            State::load(&saves[ch as usize]).map_err(|e| {
                println!("Error loading {}!\n{}", saves[ch as usize], e);
                ui::popup("Failed to load - corrupted save?");
                ui::Para::Back
            })
        }
        Err(e) => { println!("Error while scanning saves!\n{}", e); Err(ui::Para::Back) }
    }
}

fn start_menu() {
    loop {
        ui::base_colour();
        ui::cls();
        ui::banner();
        match ui::menu(0, 9, "Start Menu", 0, &["Continue An Adventure", "Start A New Character", "Quit"]) {
            Ok(0) => { if start_play(game_load()) { return } }
            Ok(1) => { if start_play(player::char_create().map(|pc|{ State::new(pc) })) { return } }
            Ok(2) | Err(ui::Para::Quit) => { return }
            _ => ()
        }
    }
}

fn main() {
    println!("Skoria running.");
    ui::initialise();
    start_menu();
    ui::terminate();
}