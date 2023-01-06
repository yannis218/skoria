use num;
use crate::util::{rnd, Level, Pos, Dist};
use crate::dungeon::{Room, Map, TB, TF};

//fn inside(r : &Room, x : usize, y : usize) -> bool { (r.lx ..= r.rx).contains(&x) && (r.ty ..= r.by).contains(&y) }

fn overlap(a : &Room, b : &Room) -> bool {
    if a.lx > b.rx || a.rx < b.lx { return false };
    if a.ty > b.by || a.by < b.ty { return false };
    true
}

fn fill(dung: &mut Map, ty : usize, by : usize, lx : usize, rx : usize, typ : TB) {
    for r in ty..=by { for c in lx..=rx {
        dung.tiles[crate::Pos(r, c)].typ = typ;
        dung.tiles[crate::Pos(r, c)].fea = TF::Nil;
    } }
}

fn rand_room(nrooms : usize, dungw : usize, dungh : usize) -> Room {
    let width = rnd::rand(9-nrooms, 15-nrooms);
    let height = rnd::rand(9-nrooms, 12-nrooms);
    let lx = rnd::rand(1, dungw-width-1);
    let ty = rnd::rand(1, dungh-height-1);
    //println!("Genned room at {},{} with dimensions {}x{}", lx, ty, width, height);
    Room{ lx, ty, rx:lx+width-1, by:ty+height-1 }
}

fn gen_rooms(dung : &mut Map, nrooms : usize) {
    let dungh = dung.tiles.height();
    let dungw = dung.tiles.width();
    for room in 0..nrooms {
        'roomgen: loop {
            let rm = rand_room(nrooms, dungw, dungh);
            for prev in 0..room { if overlap(&dung.rooms[prev as usize], &rm) { continue 'roomgen } }
            fill(dung, rm.ty, rm.by, rm.lx, rm.rx, TB::Floor);
            dung.rooms.push(rm);
            break
        }
    }
}

fn room_join(dung : &mut Map, a : usize, b : usize) {
    let rooma = &dung.rooms[a];
    let roomb = &dung.rooms[b];
    let cca = (rooma.lx + rooma.rx) / 2;
    let cra = (rooma.ty + rooma.by) / 2;
    let ccb = (roomb.lx + roomb.rx) / 2;
    let crb = (roomb.ty + roomb.by) / 2;
    let mut c = cca;
    let mut r = cra;
    while c != ccb || r != crb {
        let dc : i32 = ccb as i32 - c as i32;
        let dr : i32 = crb as i32 - r as i32;
        dung.tiles[crate::Pos(r, c)].typ = TB::Floor;
        let dist = dc*dc + dr*dr;
        if rnd::rand(1, dist) <= (dc*dc) { c = c.wrapping_add(num::signum(dc) as usize)
        } else { r = r.wrapping_add(num::signum(dr) as usize) }
    }
}

fn gen_passages(dung : &mut Map) {
    for room in 1..dung.rooms.len() {
        let other = rnd::rand(0, room-1);
        room_join(dung, room, other);
    }
}

fn place_stairs(dung : &mut Map) {
    // Stairs only appear in rooms.  Tile assumed passable.
    dung.stup = crate::Pos( rnd::rand(dung.rooms[0].ty, dung.rooms[0].by), rnd::rand(dung.rooms[0].lx, dung.rooms[0].rx));
    dung.tiles[dung.stup].fea = TF::StairUp;
    dung.stdn = crate::Pos( rnd::rand(dung.rooms[1].ty, dung.rooms[1].by), rnd::rand(dung.rooms[1].lx, dung.rooms[1].rx));
    dung.tiles[dung.stdn].fea = TF::StairDn;
}

fn tunnel_place(dung: &mut Map) -> Pos {
    let dungh = dung.tiles.height();
    let dungw = dung.tiles.width();
    loop {
        let dir = rnd::rand(0, 3); // NSEW
        let mut scan = match dir {
            0 => crate::Pos(1, rnd::rand(1, dungh - 1)),
            1 => crate::Pos(dungh - 2, rnd::rand(1, dungh - 1)),
            2 => crate::Pos(dungw - 2, rnd::rand(1, dungw - 1)),
            _ => crate::Pos(1, rnd::rand(1, dungw - 1)),
        };
        while scan.0 > 0 && scan.0 < dungh-1 && scan.1 > 0 && scan.1 < dungw-1 {
            let next = match dir {
                0 => { scan + Dist::south() }
                1 => { scan + Dist::north() }
                2 => { scan + Dist::west() }
                _ => { scan + Dist::east() }
            };
            if dung.tiles[next].passable() { return scan } // Only impassable returned so cannot clash with stairs.
            scan = next;
        }
    }
}

fn place_tunnels(dung : &mut Map, back : usize, fore : usize) {
    if back == 1 {
        let pos = tunnel_place(dung);
        dung.tiles[pos].fea = TF::TunnelBk;
        dung.tnbk = Some(pos);
    }
    if fore == 1 {
        fn other_place(dung : &mut Map) -> Pos {
            loop {
                let pos = tunnel_place(dung);
                //println!("Tunnel fore at {}?", pos);
                match dung.tnbk {
                    Some(p) => { if p != pos { return pos } }
                    None => { return pos }
                };
                //println!("Second tunnel coincided!");
            }
        }
        let pos = other_place(dung);
        dung.tiles[pos].typ = TB::Floor;
        dung.tiles[pos].fea = TF::TunnelFw;
        dung.tnfw = Some(pos);
    }
}

fn place_town(dung: &mut Map, lev : Level) {
    if rnd::rand(1, 7 + lev.depth + lev.shaft) > 3 { return }
    loop {
        let pos = tunnel_place(dung);
        if dung.tiles[pos].fea == TF::Nil {
            dung.tiles[pos].fea = [TF::TownDwr, TF::TownGnm, TF::TownHob, TF::TownPix][rnd::rand(0, 3)];
            dung.town = Some(pos);
            return
        }
    }
}

fn reseed(lev : Level) { rnd::reseed(lev.depth as u32 + (lev.shaft as u32) * 20) }

pub fn run(dung: &mut Map, lev : Level) {
    let tunnelfore = if lev.shaft < 19 { reseed(lev.fore()); rnd::rand(1, 4) } else { 100 };
    reseed(lev);
    let tunnelback = if lev.shaft > 0 { rnd::rand(1, 4) } else { 100 };
    let dungh = dung.tiles.height();
    let dungw = dung.tiles.width();
    fill(dung, 0, dungh-1, 0, dungw-1, TB::Rock);
    dung.rooms.clear();
    let nrooms = rnd::rand(3, 7);
    gen_rooms(dung, nrooms);
    gen_passages(dung);
    place_stairs(dung);
    place_tunnels(dung, tunnelback, tunnelfore);
    place_town(dung, lev);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stuff() {
        for depth in 1..=20 {
            print!("{:2} ", depth);
            for shaft in 0..20 {
                let mut map = Map::default();
                run(&mut map, Level{ depth, shaft });
                let bk = if map.tnbk.is_some() { "=" } else { " " };
                let fr = if map.tnfw.is_some() { "=" } else { " " };
                let hr = if let Some(t) = map.town { ["d","g","h","p"].get(map.tiles[t].fea as usize-5).unwrap() } else { "." };
                print!("{}|{}|{}", bk, hr, fr);
            }
            println!();
        }
    }
}