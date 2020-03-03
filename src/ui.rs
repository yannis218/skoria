use std::cmp;
use bear_lib_terminal::Color;
use bear_lib_terminal::geometry::{Point, Rect};
use bear_lib_terminal::terminal::{self, Event, KeyCode};

pub enum Key { Quit, Esc, Ret, Up, Dn, Lf, Rg, UL, UR, DL, DR, Mid, Pls, Min, Slash }

fn ev2key(e : Event) -> Option<Key> {
    match e {
        Event::KeyPressed{key:KeyCode::Escape, ctrl:true, shift:true}
        | Event::Close => { Some(Key::Quit) },
        Event::KeyPressed{key:KeyCode::Escape, ctrl:false, shift:false} => { Some(Key::Esc) }
        Event::KeyPressed{key:KeyCode::NumEnter, ctrl:false, shift:false} |
        Event::KeyPressed{key:KeyCode::Enter, ctrl:false, shift:false} => { Some(Key::Ret) }
        Event::KeyPressed{key:KeyCode::Num8, ctrl:false, shift:false} |
        Event::KeyPressed{key:KeyCode::Up, ctrl:false, shift:false} => { Some(Key::Up) }
        Event::KeyPressed{key:KeyCode::Num2, ctrl:false, shift:false} |
        Event::KeyPressed{key:KeyCode::Down, ctrl:false, shift:false} => { Some(Key::Dn) }
        Event::KeyPressed{key:KeyCode::Num6, ctrl:false, shift:false} |
        Event::KeyPressed{key:KeyCode::Right, ctrl:false, shift:false} => { Some(Key::Rg) }
        Event::KeyPressed{key:KeyCode::Num4, ctrl:false, shift:false} |
        Event::KeyPressed{key:KeyCode::Left, ctrl:false, shift:false} => { Some(Key::Lf) }
        Event::KeyPressed{key:KeyCode::Num1, ctrl:false, shift:false} => { Some(Key::DL) }
        Event::KeyPressed{key:KeyCode::Num3, ctrl:false, shift:false} => { Some(Key::DR) }
        Event::KeyPressed{key:KeyCode::Num7, ctrl:false, shift:false} => { Some(Key::UL) }
        Event::KeyPressed{key:KeyCode::Num9, ctrl:false, shift:false} => { Some(Key::UR) }
        Event::KeyPressed{key:KeyCode::Num5, ctrl:false, shift:false} => { Some(Key::Mid) }
        Event::KeyPressed{key:KeyCode::NumPlus, ctrl:false, shift:false} => { Some(Key::Pls) }
        Event::KeyPressed{key:KeyCode::NumMinus, ctrl:false, shift:false} => { Some(Key::Min) }
        Event::KeyPressed{key:KeyCode::NumDivide, ctrl:false, shift:false} |
        Event::KeyPressed{key:KeyCode::Slash, ctrl:false, shift:false} => { Some(Key::Slash) }
        _ => None
    }
}

pub fn key() -> Key {
    for e in terminal::events() {
        match ev2key(e) {
            Some(k) => { return k }
            None => { }
        }
    };
    Key::Quit
}

pub fn inkey() -> Option<Key> {
    loop {
        let e = terminal::read_event();
        if e.is_none() { return None }
        let key = ev2key(e.unwrap());
        if key.is_some() { return key }
    }
}

pub fn keyms(ms : u32) -> Option<Key> {
    for _m in 1..ms {
        let e = terminal::read_event();
        if e.is_some() {
            let key = ev2key(e.unwrap());
            if key.is_some() { return key }
        }
        terminal::delay(1);
    }
    None
}

pub fn query(x : u32, y : u32, prompt : &str, maxlen : u32) -> Option<String> {
    terminal::print_xy(x as i32, y as i32, prompt);
    let startx = x + prompt.chars().count() as u32 + 1;
    let sz = terminal::state::size();
    let max = cmp::min(maxlen, sz.width as u32- startx);
    terminal::read_str(Point::new(startx as i32, y as i32), max as i32)
}

pub type Col = (u8, u8, u8);
pub fn foreground((r, g, b) : Col) { terminal::set_foreground(Color::from_rgb(17*r, 17*g, 17*b)); }
pub fn background((r, g, b) : Col) { terminal::set_background(Color::from_rgb(17*r, 17*g, 17*b)); }

pub fn print(x : u32, y : u32, s : &str) { terminal::print_xy(x as i32, y as i32, s); }
pub fn putc(x : u32, y : u32, c : char) { terminal::put_xy(x as i32, y as i32, c); }

pub fn hilist(x : u32, y : u32, items : &[&str], hl : u32) {
    let mut i = 0;
    let bk = Color::from_rgb(0, 0, 68);
    let hi = Color::from_rgb(0, 0, 255);
    for it in items {
        terminal::with_background(if i==hl { hi } else { bk },
                                  || terminal::print_xy(x as i32, (y + i) as i32, it));
        i += 1;
    }
    terminal::refresh();
}

pub fn initialise() { terminal::open("Skoria", 68, 30); }
pub fn terminate() { terminal::close(); }
pub fn flush() { terminal::refresh(); }
pub fn base_colour() { foreground((13, 13, 10)); background((0, 0, 4)); }
pub fn cls() { terminal::clear(None); }
pub fn transparent() { /*terminal::composition(true);*/ }
pub fn opaque() { /*terminal::composition(false);*/ }
pub fn sleepms(ms : u32) { terminal::delay(ms as i32); }
pub fn clear(x : u32, y : u32, w : u32, h : u32) { terminal::clear(Some(Rect::from_values(x as i32, y as i32, w as i32, h as i32))); }

pub fn banner() {
    base_colour();
    let sz = terminal::state::size();
    let w = sz.width;
    terminal::print_xy((w - 6)/2, 0, "SKORIA");
    terminal::print_xy((w - 27)/2, 1, "A roguelike written it Rust");
    terminal::print_xy((w - 22)/2, 2, "by Yannis Irvine, 2020");
    flush();
}

pub fn popup(msg : &str) {
    cls();
    print(0, 0, msg);
    flush();
    sleepms(2000);
}

pub enum Para { Back, Quit }

pub fn menu<T: AsRef<str>>(x : u32, y : u32, title : &str, init : u32, options : &[T]) -> Result<u32, Para> {
    let w = cmp::max(options.iter().map(|o| o.as_ref().chars().count()).max().unwrap(), title.chars().count());
    let tit = format!("{:^1$}", title, w);
    let dashes = format!("{:-^1$}", "", w);
    let mut items : Vec<&str> = vec![&tit, &dashes];
    for o in options { items.push(o.as_ref()) };

    let mut sel = init;
    let n = options.len() as u32;
    terminal::clear(Some(Rect::from_values(x as i32, y as i32, 2+w as i32, 3+options.len() as i32)));
    loop {
        hilist(x+1, y, items.as_slice(), 2+sel);
        match key() {
            Key::Quit => { return Err(Para::Quit) }
            Key::Esc => { return Err(Para::Back) }
            Key::Up => { sel = (sel + n - 1) % n; }
            Key::Dn => { sel = (sel + 1) % n; }
            Key::Ret => { return Ok(sel) }
            _ => ()
        }
    }
}

use std::cell::RefCell;

thread_local! {
    static MESSAGES: RefCell<Vec<(Col,String)>> = RefCell::new(Vec::new());
    //static MSGS_ADDED : RefCell<bool> = RefCell::new(false);
}
static MAX_NUM_MSGS : usize = 20;

pub fn message(c : Col, m: &str) {
    MESSAGES.with(|msg| {
        if msg.borrow().len() < MAX_NUM_MSGS {
            msg.borrow_mut().push((c, String::from(m)));
        } else {
            msg.borrow_mut()[0] = (c, String::from(m));
            msg.borrow_mut().rotate_left(1);
        }
    });
    //MSGS_ADDED.with(|a| { *a.borrow_mut() = true; });
}

pub fn single_message(c : Col, m: &str) {
    MESSAGES.with(|msg| {
        if let Some((cc,mm)) = msg.borrow().last() {
            if cc.0 == c.0 && cc.1 == c.1 && cc.2 == c.2 && mm == m { return }
        }
        message(c, m);
    });
}

pub fn purge_messages() {
    MESSAGES.with(|msg| {
        msg.borrow_mut().clear();
    });
}

pub fn display_messages(x : u32, y : u32, h : u32) {
    //if ! MSGS_ADDED.with(|a| { if *a.borrow() { *a.borrow_mut() = false; true } else { false } }) { return }
    let sz = terminal::state::size();
    let w = sz.width as u32 - x;
    foreground((12, 14, 12));
    background((0, 0, 0));
    clear(x, y, w, h);

    MESSAGES.with(|msgs| {
        let mut lines = Vec::new();
        let msgs = &*msgs.borrow();
        for m in msgs {
            for c in m.1.as_bytes().chunks(w as usize) {
                lines.push((m.0, std::str::from_utf8(c).unwrap()))
            }
        }

        for (i, m) in lines.iter().rev().enumerate() {
            if i as u32 == h { break }
            foreground(m.0);
            print(x, y+h-1-i as u32, m.1);
        }
    })
}