use speedy2d::color::Color;
use speedy2d::font;
use speedy2d::font::Font;
use speedy2d::font::TextLayout;
use speedy2d::font::TextOptions;
use speedy2d::shape::{Rectangle};
use speedy2d::window::{KeyScancode, UserEventSender, WindowCreationOptions};
use speedy2d::window::VirtualKeyCode;
use speedy2d::window::WindowHandler;
use speedy2d::window::WindowHelper;
use speedy2d::window::WindowSize;
use speedy2d::Graphics2D;
use speedy2d::Window;

use std::cell::RefCell;
use std::cmp::{min, max};
use std::sync::mpsc::channel;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::Sender;
use std::thread;
use std::time::Duration;
use speedy2d::dimen::UVec2;

pub enum SpecialKey { Esc, Ret, Up, Dn, Lf, Rg, UL, UR, DL, DR, Mid, Pls, Min, Slash, Del }

pub enum Key { Quit, Special(SpecialKey), Letter(char) }

pub type Col = (u8, u8, u8);

const WIDTH: usize = 68;
const HEIGHT: usize = 30;
const SCALE: usize = 32;
const WINDOW_TITLE: &str = "Skoria";

type Buffer = [[(char, Col, Col); WIDTH]; HEIGHT];

static mut BUF: Buffer = [[(' ', (0, 0, 0), (0, 0, 0)); WIDTH]; HEIGHT];

struct Console {
    font: Font,
    input_head: Sender<Key>,
    size_pixels: UVec2
}

fn keytrans(kc: VirtualKeyCode) -> Option<SpecialKey> {
    match kc {
        VirtualKeyCode::Escape => Some(SpecialKey::Esc),
        VirtualKeyCode::NumpadEnter | VirtualKeyCode::Return => Some(SpecialKey::Ret),
        VirtualKeyCode::Numpad8 | VirtualKeyCode::Up => Some(SpecialKey::Up),
        VirtualKeyCode::Numpad2 | VirtualKeyCode::Down => Some(SpecialKey::Dn),
        VirtualKeyCode::Numpad6 | VirtualKeyCode::Right => Some(SpecialKey::Rg),
        VirtualKeyCode::Numpad4 | VirtualKeyCode::Left => Some(SpecialKey::Lf),
        VirtualKeyCode::Numpad1 => Some(SpecialKey::DL),
        VirtualKeyCode::Numpad3 => Some(SpecialKey::DR),
        VirtualKeyCode::Numpad7 => Some(SpecialKey::UL),
        VirtualKeyCode::Numpad9 => Some(SpecialKey::UR),
        VirtualKeyCode::Numpad5 => Some(SpecialKey::Mid),
        VirtualKeyCode::NumpadAdd => Some(SpecialKey::Pls),
        VirtualKeyCode::NumpadSubtract => Some(SpecialKey::Min),
        VirtualKeyCode::NumpadDivide | VirtualKeyCode::Slash => Some(SpecialKey::Slash),
        VirtualKeyCode::Backspace => Some(SpecialKey::Del),
        _ => None,
    }
}

enum Directive { Refresh, Quit }

impl WindowHandler<Directive> for Console {
    fn on_user_event(&mut self, helper: &mut WindowHelper<Directive>, directive: Directive) {
        match directive {
            Directive::Refresh => helper.request_redraw(),
            Directive::Quit => helper.terminate_loop()
        }
    }

    fn on_resize(&mut self, _helper: &mut WindowHelper<Directive>, size_pixels: UVec2) { self.size_pixels = size_pixels; }

    fn on_draw(&mut self, _helper: &mut WindowHelper<Directive>, graphics: &mut Graphics2D) {
        graphics.clear_screen(Color::BLACK);

        let xsc = self.size_pixels.x as f32 / WIDTH as f32;
        let ysc = self.size_pixels.y as f32 / HEIGHT as f32;
        let scale = if xsc < ysc { xsc } else { ysc };
        let mut x = 0.0;
        let mut y = 0.0;
        unsafe {
            for row in BUF {
                for (ch, (fr, fg, fb), (br, bg, bb)) in row {
                    let bl = self
                        .font
                        .layout_text_from_unindexed_codepoints(std::slice::from_ref(&ch),
                                                               scale as f32,
                                                               TextOptions::new());
                    graphics.draw_rectangle(Rectangle::from_tuples((x * scale as f32, y * scale as f32),
                                                                   ((x + 1.0) * scale as f32, (y + 1.0) * scale as f32)),
                                            Color::from_int_rgb(17 * br, 17 * bg, 17 * bb));
                    graphics.draw_text(((x + 0.5) * scale as f32 - 0.5 * &bl.width(), y * scale as f32),
                                       Color::from_int_rgb(17 * fr, 17 * fg, 17 * fb), &bl);
                    x += 1.0;
                    if x == WIDTH as f32 {
                        x = 0.0;
                        y += 1.0;
                    }
                }
            }
        }
    }

    fn on_key_down(&mut self, _helper: &mut WindowHelper<Directive>, virtual_key_code: Option<VirtualKeyCode>, _scancode: KeyScancode) {
        if let Some(kc) = virtual_key_code {
            if let Some(k) = keytrans(kc) {
                self.input_head.send(Key::Special(k)).unwrap_or_default()
            }
        }
    }

    fn on_keyboard_char(&mut self, _helper: &mut WindowHelper<Directive>, k: char) {
        self.input_head.send(Key::Letter(k)).unwrap_or_default()
    }
}

impl Console {
    fn new(input_head: Sender<Key>, size_pixels: UVec2) -> Self {
        let bytes = include_bytes!("../assets/fonts/CodeNewRoman.otf");

        Self {
            font: font::Font::new(bytes).unwrap(),
            input_head,
            size_pixels
        }
    }
}

struct Terminal {
    input_tail: Option<Receiver<Key>>,
    sender: Option<UserEventSender<Directive>>,
    pub foreground: Col,
    pub background: Col
}

impl Terminal {
    fn new(rx: Receiver<Key>, tx: UserEventSender<Directive>) -> Self {
        Self {
            input_tail: Some(rx),
            sender: Some(tx),
            foreground: (10, 10, 10),
            background: (0, 0, 0)
        }
    }

    fn dead() -> Self {
        Self { input_tail: None, sender: None, foreground: (0, 0, 0), background: (0, 0, 0) }
    }

    fn post(&self, dir: Directive) {
        self.sender.as_ref().unwrap().send_event(dir).unwrap();
    }
}

//07537033224 SevosNR1947

thread_local! {
    static TERM : RefCell<Terminal> = RefCell::new(Terminal::dead());
}

pub fn start(game: fn() -> ()) -> ! {
    let size = UVec2::new((WIDTH * SCALE) as u32, (HEIGHT * SCALE) as u32);
    let options = WindowCreationOptions::new_windowed(WindowSize::PhysicalPixels(size), None);
    let window = Window::<Directive>::new_with_user_events(WINDOW_TITLE, options.with_resizable(true)).unwrap();
    let tube = window.create_user_event_sender();

    let (tx, rx) = channel::<Key>();
    thread::spawn(move || {
        let term = Terminal::new(rx, tube);
        TERM.with(|t| {
            t.replace(term);
            game();
            t.borrow().post(Directive::Quit);
        });
    });

    let con = Console::new(tx, size);
    window.run_loop(con);
}

pub fn key() -> Key {
    TERM.with(|t| {
        if let Ok(k) = t.borrow().input_tail.as_ref().unwrap().recv() { k } else { Key::Quit }
    })
}

pub fn inkey() -> Option<Key> {
    TERM.with(|t| {
        if let Ok(k) = t.borrow().input_tail.as_ref().unwrap().try_recv() { Some(k) } else { None }
    })
}

pub fn keyms(ms: u32) -> Option<Key> {
    TERM.with(|t| {
        for _m in 1..ms {
            match t.borrow().input_tail.as_ref().unwrap().try_recv() {
                Ok(k) => { return Some(k); }
                _ => thread::sleep(Duration::from_millis(1)),
            }
        }
        None
    })
}

pub fn foreground(c: Col) { TERM.with(|t| t.borrow_mut().foreground = c); }

pub fn background(c: Col) { TERM.with(|t| t.borrow_mut().background = c); }

pub fn cur_bkgnd() -> Col { TERM.with(|t| t.borrow().background) }

pub fn putc(x: usize, y: usize, c: char) {
    unsafe {
        TERM.with(|t| BUF[y][x] = (c, t.borrow().foreground, t.borrow().background));
    }
}

pub fn flush() { TERM.with(|t| t.borrow().post(Directive::Refresh)); }



pub fn clear(x: usize, y: usize, w: usize, h: usize) {
    for r in y..y + h {
        for c in x..x + w {
            putc(c, r, ' ');
        }
    }
}

pub fn print(x: usize, y: usize, s: &str) {
    let mut xx = x;
    let mut yy = y;
    for c in s.chars() {
        putc(xx, yy, c);
        xx += 1;
        if x == WIDTH {
            xx = 0;
            yy += 1;
            if yy == HEIGHT { yy = 0 }
        }
    }
}

pub fn read_str(x: usize, y: usize, max: usize) -> Option<String> {
    let mut s: String = String::default();
    loop {
        print(x, y, &s);
        match key() {
            Key::Quit | Key::Special(SpecialKey::Esc) => return None,
            Key::Special(SpecialKey::Ret) => return Some(s),
            Key::Special(SpecialKey::Del) => {
                putc(x, y + s.len() - 1, ' ');
                s.pop();
            },
            Key::Letter(c) => if s.len() < max { s.push(c) },
            _ => {}
        }
    }
}

pub fn query(x: usize, y: usize, prompt: &str, maxlen: usize) -> Option<String> {
    print(x, y, prompt);
    let startx = x + prompt.chars().count() + 1;
    let maxlen = min(maxlen, WIDTH - startx);
    read_str(startx, y, maxlen)
}

pub fn transparent() { /*terminal::composition(true);*/ }

pub fn opaque() { /*terminal::composition(false);*/ }

pub fn sleepms(ms: u32) { thread::sleep(Duration::from_millis(ms as u64)) }

pub fn cls() { clear(0, 0, WIDTH, HEIGHT); }

pub fn base_colour() {
    foreground((13, 13, 10));
    background((0, 0, 4));
}

pub fn banner() {
    base_colour();
    let w = WIDTH;
    print((w - 6) / 2, 0, "SKORIA");
    print((w - 27) / 2, 1, "A roguelike written in Rust");
    print((w - 22) / 2, 2, "by Yannis Irvine, 2020-2023");
    flush();
}

pub fn popup(msg: &str) {
    cls();
    print(0, 0, msg);
    flush();
    sleepms(2000);
}

pub fn hilist(x: usize, y: usize, items: &[&str], hl: usize) {
    let mut i: usize = 0;
    let bk = (0, 0, 4);
    let hi = (0, 0, 15);
    let old_bk = cur_bkgnd();
    for it in items {
        background(if i == hl { hi } else { bk });
        print(x, y + i, it);
        i += 1;
    }
    background(old_bk);
    flush();
}

pub enum Para { Back, Quit }

pub fn menu<T: AsRef<str>>(x: usize, y: usize, title: &str, init: usize, options: &[T]) -> Result<usize, Para> {
    let w = max(options.iter().map(|o| o.as_ref().chars().count()).max().unwrap(), title.chars().count());
    let tit = format!("{:^1$}", title, w);
    let dashes = format!("{:-^1$}", "", w);
    let mut items: Vec<&str> = vec![&tit, &dashes];
    for o in options { items.push(o.as_ref()) };

    let mut sel = init;
    let n = options.len();
    clear(x, y, 2 + w, 3 + options.len());
    loop {
        hilist(x + 1, y, items.as_slice(), 2 + sel);
        match key() {
            Key::Quit => { return Err(Para::Quit) }
            Key::Special(k) => match k {
                SpecialKey::Esc => { return Err(Para::Back) }
                SpecialKey::Up => { sel = (sel + n - 1) % n; }
                SpecialKey::Dn => { sel = (sel + 1) % n; }
                SpecialKey::Ret => { return Ok(sel) }
                _ => ()
            }
            _ => ()
        }
    }
}

thread_local! {
    static MESSAGES: RefCell<Vec<(Col,String)>> = RefCell::new(Vec::new());
    //static MSGS_ADDED : RefCell<bool> = RefCell::new(false);
}
static MAX_NUM_MSGS: usize = 20;

pub fn message(c: Col, m: &str) {
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

pub fn single_message(c: Col, m: &str) {
    MESSAGES.with(|msg| {
        if let Some((cc, mm)) = msg.borrow().last() {
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

pub fn display_messages(x: usize, y: usize, h: usize) {
    //if ! MSGS_ADDED.with(|a| { if *a.borrow() { *a.borrow_mut() = false; true } else { false } }) { return }
    let w = WIDTH - x;
    foreground((12, 14, 12));
    background((0, 0, 0));
    clear(x, y, w, h);

    MESSAGES.with(|msgs| {
        let mut lines = Vec::new();
        let msgs = &*msgs.borrow();
        for m in msgs {
            for c in m.1.as_bytes().chunks(w) {
                lines.push((m.0, std::str::from_utf8(c).unwrap()))
            }
        }

        for (i, m) in lines.iter().rev().enumerate() {
            if i == h { break }
            foreground(m.0);
            print(x, y + h - 1 - i, m.1);
        }
    })
}
