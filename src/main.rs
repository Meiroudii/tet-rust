use ggez::event::EventHandler;
use ggez::graphics::{self, Canvas, Color, DrawMode, DrawParam, Mesh, Rect, Text};
use ggez::input::keyboard::KeyInput;
use ggez::winit::event::VirtualKeyCode as KeyCode;
use ggez::{Context, ContextBuilder, GameResult};

use rand::seq::SliceRandom;
use rand::thread_rng;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::time::{Instant};

const COLS: i32 = 10;
const ROWS: i32 = 20;
const CELL: f32 = 28.0;
const BORDER: f32 = 12.0;
const PANEL_W: f32 = 260.0;

const TICK_BASE: f32 = 0.6; // seconds per gravity step at level 1
const SOFT_DROP_TICK: f32 = 0.05;
const LOCK_DELAY_SEC: f32 = 0.50;
const LOCK_MOVE_RESETS: u32 = 15;
const NEXT_PREVIEW: usize = 5;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Tile {
    Empty,
    Filled(u8),
}

#[derive(Clone, Copy, Debug)]
struct Point {
    x: i32,
    y: i32,
}
fn p(x: i32, y: i32) -> Point {
    Point { x, y }
}

#[derive(Clone, Copy, Debug)]
enum PieceKind {
    I,
    O,
    T,
    S,
    Z,
    J,
    L,
}

#[derive(Clone, Debug)]
struct Piece {
    kind: PieceKind,
    rotation: u8, // 0..=3
    pos: Point,
}
impl Piece {
    fn blocks_local(&self) -> [Point; 4] {
        match self.kind {
            PieceKind::I => match self.rotation % 4 {
                0 => [p(0, 1), p(1, 1), p(2, 1), p(3, 1)],
                1 => [p(2, 0), p(2, 1), p(2, 2), p(2, 3)],
                2 => [p(0, 2), p(1, 2), p(2, 2), p(3, 2)],
                _ => [p(1, 0), p(1, 1), p(1, 2), p(1, 3)],
            },
            PieceKind::O => [p(1, 0), p(2, 0), p(1, 1), p(2, 1)],
            PieceKind::T => match self.rotation % 4 {
                0 => [p(1, 0), p(0, 1), p(1, 1), p(2, 1)],
                1 => [p(1, 0), p(1, 1), p(2, 1), p(1, 2)],
                2 => [p(0, 1), p(1, 1), p(2, 1), p(1, 2)],
                _ => [p(1, 0), p(0, 1), p(1, 1), p(1, 2)],
            },
            PieceKind::S => match self.rotation % 4 {
                0 => [p(1, 0), p(2, 0), p(0, 1), p(1, 1)],
                1 => [p(1, 0), p(1, 1), p(2, 1), p(2, 2)],
                2 => [p(1, 1), p(2, 1), p(0, 2), p(1, 2)],
                _ => [p(0, 0), p(0, 1), p(1, 1), p(1, 2)],
            },
            PieceKind::Z => match self.rotation % 4 {
                0 => [p(0, 0), p(1, 0), p(1, 1), p(2, 1)],
                1 => [p(2, 0), p(1, 1), p(2, 1), p(1, 2)],
                2 => [p(0, 1), p(1, 1), p(1, 2), p(2, 2)],
                _ => [p(1, 0), p(0, 1), p(1, 1), p(0, 2)],
            },
            PieceKind::J => match self.rotation % 4 {
                0 => [p(0, 0), p(0, 1), p(1, 1), p(2, 1)],
                1 => [p(1, 0), p(2, 0), p(1, 1), p(1, 2)],
                2 => [p(0, 1), p(1, 1), p(2, 1), p(2, 2)],
                _ => [p(1, 0), p(1, 1), p(0, 2), p(1, 2)],
            },
            PieceKind::L => match self.rotation % 4 {
                0 => [p(2, 0), p(0, 1), p(1, 1), p(2, 1)],
                1 => [p(1, 0), p(1, 1), p(1, 2), p(2, 2)],
                2 => [p(0, 1), p(1, 1), p(2, 1), p(0, 2)],
                _ => [p(0, 0), p(1, 0), p(1, 1), p(1, 2)],
            },
        }
    }
    fn blocks(&self) -> [Point; 4] {
        self.blocks_local()
            .map(|b| p(self.pos.x + b.x, self.pos.y + b.y))
    }
    fn pos_offset(&self, dx: i32, dy: i32) -> Self {
        let mut c = self.clone();
        c.pos.x += dx;
        c.pos.y += dy;
        c
    }
}

struct Bag {
    queue: VecDeque<PieceKind>,
}
impl Bag {
    fn new() -> Self {
        let mut b = Self {
            queue: VecDeque::new(),
        };
        b.refill();
        b
    }
    fn refill(&mut self) {
        let mut seq = vec![
            PieceKind::I,
            PieceKind::O,
            PieceKind::T,
            PieceKind::S,
            PieceKind::Z,
            PieceKind::J,
            PieceKind::L,
        ];
        seq.shuffle(&mut thread_rng());
        for k in seq {
            self.queue.push_back(k);
        }
    }
    fn pop(&mut self) -> PieceKind {
        if self.queue.is_empty() {
            self.refill();
        }
        self.queue.pop_front().unwrap()
    }
    fn peek_next(&mut self, n: usize) -> Vec<PieceKind> {
        while self.queue.len() < n {
            self.refill();
        }
        self.queue.iter().cloned().take(n).collect()
    }
}

#[derive(Serialize, Deserialize, Default, Clone)]
struct HighScore {
    best_score: u32,
    best_lines: u32,
    best_level: u32,
}
const HIGHSCORE_PATH: &str = "letris_highscore.json";

fn load_highscore() -> HighScore {
    if let Ok(mut f) = File::open(HIGHSCORE_PATH) {
        let mut s = String::new();
        if f.read_to_string(&mut s).is_ok() {
            if let Ok(hs) = serde_json::from_str::<HighScore>(&s) {
                return hs;
            }
        }
    }
    HighScore::default()
}
fn save_highscore(hs: &HighScore) {
    if let Ok(mut f) = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(HIGHSCORE_PATH)
    {
        let _ = f.write_all(serde_json::to_string_pretty(hs).unwrap().as_bytes());
    }
}

struct Game {
    board: Vec<Vec<Tile>>,
    active: Piece,
    bag: Bag,
    next_queue: Vec<PieceKind>,
    hold: Option<PieceKind>,
    hold_used: bool,

    last_tick: Instant,
    score: u32,
    level: u32,
    lines_cleared: u32,

    paused: bool,
    game_over: bool,

    grounded: bool,
    lock_start: Option<Instant>,
    lock_resets_left: u32,

    highscore: HighScore,
}

impl Game {
    fn new() -> Self {
        let mut bag = Bag::new();
        let first = bag.pop();
        let mut g = Self {
            board: vec![vec![Tile::Empty; COLS as usize]; ROWS as usize],
            active: spawn_piece(first),
            bag,
            next_queue: Vec::new(),
            hold: None,
            hold_used: false,
            last_tick: Instant::now(),
            score: 0,
            level: 1,
            lines_cleared: 0,
            paused: false,
            game_over: false,
            grounded: false,
            lock_start: None,
            lock_resets_left: LOCK_MOVE_RESETS,
            highscore: load_highscore(),
        };
        g.refill_next();
        if !g.valid(&g.active) {
            g.game_over = true;
        }
        g
    }

    fn reset(&mut self) {
        *self = Game::new();
    }

    fn refill_next(&mut self) {
        self.next_queue = self.bag.peek_next(NEXT_PREVIEW);
    }

    fn gravity_dt(&self) -> f32 {
        (TICK_BASE * (0.9f32).powi((self.level as i32) - 1)).max(0.05)
    }

    fn valid(&self, piece: &Piece) -> bool {
        for b in piece.blocks().iter() {
            if b.x < 0 || b.x >= COLS || b.y >= ROWS {
                return false;
            }
            if b.y >= 0 {
                if let Tile::Filled(_) = self.board[b.y as usize][b.x as usize] {
                    return false;
                }
            }
        }
        true
    }

    fn try_move(&mut self, dx: i32, dy: i32) -> bool {
        let np = self.active.pos_offset(dx, dy);
        if self.valid(&np) {
            self.active = np;
            if self.grounded {
                self.reset_lock_timer();
            }
            true
        } else {
            false
        }
    }

    fn reset_lock_timer(&mut self) {
        if self.lock_resets_left > 0 {
            self.lock_start = Some(Instant::now());
            self.lock_resets_left -= 1;
        }
    }

    fn try_rotate(&mut self, cw: bool) {
        let (ok, rotated) = self.srs_rotate(&self.active, cw);
        if ok {
            self.active = rotated;
            if self.grounded {
                self.reset_lock_timer();
            }
        }
    }

    fn srs_rotate(&self, piece: &Piece, cw: bool) -> (bool, Piece) {
        let mut np = piece.clone();
        np.rotation = if cw {
            (piece.rotation + 1) % 4
        } else {
            (piece.rotation + 3) % 4
        };

        const JLSTZ: [[(i32, i32); 5]; 8] = [
            [(0, 0), (-1, 0), (-1, -1), (0, 2), (-1, 2)], // 0->R
            [(0, 0), (1, 0), (1, 1), (0, -2), (1, -2)],   // R->0
            [(0, 0), (1, 0), (1, -1), (0, 2), (1, 2)],    // R->2
            [(0, 0), (-1, 0), (-1, 1), (0, -2), (-1, -2)],// 2->R
            [(0, 0), (1, 0), (1, -1), (0, 2), (1, 2)],    // 2->L
            [(0, 0), (-1, 0), (-1, 1), (0, -2), (-1, -2)],// L->2
            [(0, 0), (-1, 0), (-1, -1), (0, 2), (-1, 2)], // L->0
            [(0, 0), (1, 0), (1, 1), (0, -2), (1, -2)],   // 0->L
        ];
        const I_KICKS: [[(i32, i32); 5]; 8] = [
            [(0, 0), (-2, 0), (1, 0), (-2, -1), (1, 2)],  // 0->R
            [(0, 0), (2, 0), (-1, 0), (2, 1), (-1, -2)],  // R->0
            [(0, 0), (-1, 0), (2, 0), (-1, 2), (2, -1)],  // R->2
            [(0, 0), (1, 0), (-2, 0), (1, -2), (-2, 1)],  // 2->R
            [(0, 0), (2, 0), (-1, 0), (2, 1), (-1, -2)],  // 2->L
            [(0, 0), (-2, 0), (1, 0), (-2, -1), (1, 2)],  // L->2
            [(0, 0), (1, 0), (-2, 0), (1, -2), (-2, 1)],  // L->0
            [(0, 0), (-1, 0), (2, 0), (-1, 2), (2, -1)],  // 0->L
        ];

        let from = piece.rotation as usize;
        let to = np.rotation as usize;
        let idx = match (from, to) {
            (0, 1) => 0,
            (1, 0) => 1,
            (1, 2) => 2,
            (2, 1) => 3,
            (2, 3) => 4,
            (3, 2) => 5,
            (3, 0) => 6,
            (0, 3) => 7,
            _ => 0,
        };

        let kicks = match piece.kind {
            PieceKind::I => I_KICKS[idx],
            PieceKind::O => [(0, 0), (0, 0), (0, 0), (0, 0), (0, 0)],
            _ => JLSTZ[idx],
        };

        for (dx, dy) in kicks {
            let test = Piece {
                pos: p(piece.pos.x + dx, piece.pos.y + dy),
                ..np.clone()
            };
            if self.valid(&test) {
                return (true, test);
            }
        }
        (false, np)
    }

    fn lock_piece(&mut self) {
        for b in self.active.blocks().iter() {
            if b.y >= 0 && b.y < ROWS && b.x >= 0 && b.x < COLS {
                self.board[b.y as usize][b.x as usize] =
                    Tile::Filled(self.kind_to_id(self.active.kind));
            }
        }
        let cleared = self.clear_lines();
        self.lines_cleared += cleared as u32;
        self.score += match cleared {
            1 => 100 * self.level,
            2 => 300 * self.level,
            3 => 500 * self.level,
            4 => 800 * self.level,
            _ => 0,
        };
        self.level = 1 + self.lines_cleared / 10;

        let next = self.bag.pop();
        self.active = spawn_piece(next);
        self.hold_used = false;
        self.lock_start = None;
        self.grounded = false;
        self.lock_resets_left = LOCK_MOVE_RESETS;
        self.refill_next();

        if !self.valid(&self.active) {
            self.game_over = true;
            let mut hs = self.highscore.clone();
            if self.score > hs.best_score {
                hs.best_score = self.score;
            }
            if self.lines_cleared > hs.best_lines {
                hs.best_lines = self.lines_cleared;
            }
            if self.level > hs.best_level {
                hs.best_level = self.level;
            }
            self.highscore = hs.clone();
            save_highscore(&hs);
        }
    }

    fn clear_lines(&mut self) -> usize {
        let mut new_rows: Vec<Vec<Tile>> = Vec::with_capacity(ROWS as usize);
        let mut cleared = 0usize;
        for y in 0..ROWS {
            let full = (0..COLS)
                .all(|x| matches!(self.board[y as usize][x as usize], Tile::Filled(_)));
            if full {
                cleared += 1;
            } else {
                new_rows.push(self.board[y as usize].clone());
            }
        }
        for _ in 0..cleared {
            new_rows.insert(0, vec![Tile::Empty; COLS as usize]);
        }
        if cleared > 0 {
            self.board = new_rows;
        }
        cleared
    }

    fn kind_to_id(&self, k: PieceKind) -> u8 {
        match k {
            PieceKind::I => 1,
            PieceKind::O => 2,
            PieceKind::T => 3,
            PieceKind::S => 4,
            PieceKind::Z => 5,
            PieceKind::J => 6,
            PieceKind::L => 7,
        }
    }

    fn id_to_color(id: u8) -> Color {
        match id {
            1 => Color::from_rgb(0, 240, 240),  // I
            2 => Color::from_rgb(240, 240, 0),  // O
            3 => Color::from_rgb(160, 0, 240),  // T
            4 => Color::from_rgb(0, 240, 0),    // S
            5 => Color::from_rgb(240, 0, 0),    // Z
            6 => Color::from_rgb(0, 0, 240),    // J
            7 => Color::from_rgb(240, 160, 0),  // L
            _ => Color::WHITE,
        }
    }

    fn hard_drop(&mut self) {
        let mut p = self.active.clone();
        while self.valid(&p.pos_offset(0, 1)) {
            p.pos.y += 1;
        }
        self.active = p;
        self.lock_piece();
        self.last_tick = Instant::now();
        self.score += 2;
    }

    fn ghost_position(&self) -> Piece {
        let mut p = self.active.clone();
        while self.valid(&p.pos_offset(0, 1)) {
            p.pos.y += 1;
        }
        p
    }

    fn hold_action(&mut self) {
        if self.hold_used || self.game_over {
            return;
        }
        self.hold_used = true;
        let current_kind = self.active.kind;
        if let Some(hk) = self.hold {
            self.active = spawn_piece(hk);
            self.hold = Some(current_kind);
        } else {
            self.hold = Some(current_kind);
            self.active = spawn_piece(self.bag.pop());
            self.refill_next();
        }
        self.lock_start = None;
        self.grounded = false;
        self.lock_resets_left = LOCK_MOVE_RESETS;
        if !self.valid(&self.active) {
            self.game_over = true;
        }
    }

    fn update_game(&mut self, ctx: &mut Context) {
        if self.paused || self.game_over {
            return;
        }
        let now = Instant::now();

        let mut dt = self.gravity_dt();
        if ctx.keyboard.is_key_pressed(KeyCode::Down) {
            dt = SOFT_DROP_TICK;
        }
        if now.duration_since(self.last_tick).as_secs_f32() >= dt {
            if !self.try_move(0, 1) {
                if !self.grounded {
                    self.grounded = true;
                    self.lock_start = Some(now);
                }
            } else {
                self.grounded = false;
                self.lock_start = None;
            }
            self.last_tick = now;
        }

        if self.grounded {
            let start = self.lock_start.unwrap_or(now);
            if now.duration_since(start).as_secs_f32() >= LOCK_DELAY_SEC || self.lock_resets_left == 0
            {
                self.lock_piece();
            }
        }
    }

    fn draw_game(&self, ctx: &mut Context, canvas: &mut Canvas) -> GameResult {
        // Background + board bg
        let bg = Mesh::new_rectangle(
            ctx,
            DrawMode::fill(),
            Rect::new(0.0, 0.0, BORDER * 2.0 + CELL * COLS as f32 + PANEL_W, BORDER * 2.0 + CELL * ROWS as f32),
            Color::from_rgb(18, 18, 20),
        )?;
        canvas.draw(&bg, DrawParam::default());

        let board_w = CELL * COLS as f32;
        let board_h = CELL * ROWS as f32;
        let ox = BORDER;
        let oy = BORDER;

        let grid_bg = Mesh::new_rectangle(
            ctx,
            DrawMode::fill(),
            Rect::new(ox, oy, board_w, board_h),
            Color::from_rgb(28, 28, 34),
        )?;
        canvas.draw(&grid_bg, DrawParam::default());

        // Locked tiles
        for y in 0..ROWS {
            for x in 0..COLS {
                if let Tile::Filled(id) = self.board[y as usize][x as usize] {
                    let c = Self::id_to_color(id);
                    let rect = Rect::new(
                        ox + x as f32 * CELL,
                        oy + y as f32 * CELL,
                        CELL - 1.0,
                        CELL - 1.0,
                    );
                    let tile = Mesh::new_rectangle(ctx, DrawMode::fill(), rect, c)?;
                    canvas.draw(&tile, DrawParam::default());
                }
            }
        }

        // Ghost
        let ghost = self.ghost_position();
        for b in ghost.blocks().iter() {
            if b.y >= 0 {
                let rect = Rect::new(
                    ox + b.x as f32 * CELL,
                    oy + b.y as f32 * CELL,
                    CELL - 1.0,
                    CELL - 1.0,
                );
                let id = self.kind_to_id(self.active.kind);
                let mut c = Self::id_to_color(id);
                c.a = 0.25;
                let tile = Mesh::new_rectangle(ctx, DrawMode::fill(), rect, c)?;
                canvas.draw(&tile, DrawParam::default());
            }
        }

        // Active piece
        for b in self.active.blocks().iter() {
            if b.y >= 0 {
                let rect = Rect::new(
                    ox + b.x as f32 * CELL,
                    oy + b.y as f32 * CELL,
                    CELL - 1.0,
                    CELL - 1.0,
                );
                let tile = Mesh::new_rectangle(
                    ctx,
                    DrawMode::fill(),
                    rect,
                    Self::id_to_color(self.kind_to_id(self.active.kind)),
                )?;
                canvas.draw(&tile, DrawParam::default());
            }
        }

        // Side panel text
        let mut info = format!(
            "LETRIS\nScore: {}\nLevel: {}\nLines: {}\n\n[Hold: {}]\n\nNext:\n",
            self.score,
            self.level,
            self.lines_cleared,
            self.hold
                .map(|k| format!("{:?}", k))
                .unwrap_or_else(|| "-".into())
        );
        for k in &self.next_queue {
            info.push_str(&format!("  {:?}\n", k));
        }
        info.push_str("\n[P] Pause  [R] Restart  [C] Hold\n←/→ move, ↓ soft, ↑/Z/X rotate, Space hard");
        if self.paused {
            info.push_str("\n\nPAUSED");
        }
        if self.game_over {
            info.push_str("\n\nGAME OVER");
            info.push_str(&format!(
                "\nBest: {} pts  {} lines  L{}",
                self.highscore.best_score, self.highscore.best_lines, self.highscore.best_level
            ));
        }

        let text = Text::new(info);
        use ggez::glam::Vec2;
        canvas.draw(
            &text,
            DrawParam::default().dest(Vec2::new(ox + board_w + 24.0, oy + 8.0)),
        );

        Ok(())
    }
}

impl EventHandler for Game {
    fn update(&mut self, ctx: &mut Context) -> GameResult {
        self.update_game(ctx);
        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult {
        let mut canvas = Canvas::from_frame(ctx, Color::BLACK);
        self.draw_game(ctx, &mut canvas)?;
        canvas.finish(ctx)?;
        Ok(())
    }

    fn key_down_event(
        &mut self,
        ctx: &mut Context,
        input: KeyInput,
        repeat: bool,
    ) -> GameResult {
        if repeat {
            return Ok(());
        }
        if let Some(keycode) = input.keycode {
            match keycode {
                KeyCode::P => self.paused = !self.paused,
                KeyCode::R => self.reset(),

                KeyCode::Left => {
                    let _ = self.try_move(-1, 0);
                }
                KeyCode::Right => {
                    let _ = self.try_move(1, 0);
                }
                KeyCode::Down => {
                    if !self.try_move(0, 1) {
                        // already grounded; lock timing handled in update
                    } else {
                        self.score += 1;
                    }
                }
                KeyCode::Space => {
                    self.hard_drop();
                }
                KeyCode::Up | KeyCode::X => {
                    self.try_rotate(true);
                }
                KeyCode::Z => {
                    self.try_rotate(false);
                }
                KeyCode::C => {
                    self.hold_action();
                }
                KeyCode::Escape => ctx.request_quit(),
                _ => {}
            }
        }
        Ok(())
    }
}

// piece spawn at top
fn spawn_piece(k: PieceKind) -> Piece {
    Piece {
        kind: k,
        rotation: 0,
        pos: p(3, -2),
    }
}

pub fn main() -> GameResult {
    let (ctx, event_loop) = ContextBuilder::new("letris", "you")
        .window_setup(ggez::conf::WindowSetup::default().title("Letris (ggez)"))
        .window_mode(
            ggez::conf::WindowMode::default().dimensions(
                BORDER * 2.0 + CELL * COLS as f32 + PANEL_W,
                BORDER * 2.0 + CELL * ROWS as f32,
            ),
        )
        .build()?;

    let game = Game::new();
    ggez::event::run(ctx, event_loop, game)
}
