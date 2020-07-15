use fixedbitset::FixedBitSet;
use js_sys::Math;
use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{CanvasRenderingContext2d, HtmlCanvasElement, Window};

const CELL_SIZE: u32 = 5;
const GRID_COLOUR: &str = "#CCCCCC";
const DEAD_COLOUR: &str = "#FFFFFF";
const ALIVE_COLOUR: &str = "#000000";

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

/// All cells represented as a linear collection of bits.
struct Universe {
    width: u32,
    height: u32,
    cells: FixedBitSet,
}

impl Universe {
    fn new() -> Universe {
        let width = 96;
        let height = 96;
        let mut cells = FixedBitSet::with_capacity(width * height);

        for i in 0..width * height {
            let rand = Math::random();
            cells.set(i, rand < 0.5);
        }

        Universe {
            width: width as u32,
            height: height as u32,
            cells,
        }
    }

    /// The cells are stored linearly, so this function yields an index into
    /// that linear storage given some 2D row/col pair.
    fn get_index(&self, row: u32, col: u32) -> usize {
        (row * self.width + col) as usize
    }

    /// How many of the given cell's neighbours are alive?
    ///
    /// The hand-unrolled code offers a decent performance improvement, since it
    /// cuts out what used to be many `%` calls.
    fn live_neighbour_count(&self, row: u32, col: u32) -> u8 {
        let mut count = 0;

        let north = if row == 0 { self.height - 1 } else { row - 1 };
        let south = if row == self.height - 1 { 0 } else { row + 1 };
        let west = if col == 0 { self.width - 1 } else { col - 1 };
        let east = if col == self.width - 1 { 0 } else { col + 1 };

        let nw = self.get_index(north, west);
        count += self.cells[nw] as u8;

        let n = self.get_index(north, col);
        count += self.cells[n] as u8;

        let ne = self.get_index(north, east);
        count += self.cells[ne] as u8;

        let w = self.get_index(row, west);
        count += self.cells[w] as u8;

        let e = self.get_index(row, east);
        count += self.cells[e] as u8;

        let sw = self.get_index(south, west);
        count += self.cells[sw] as u8;

        let s = self.get_index(south, col);
        count += self.cells[s] as u8;

        let se = self.get_index(south, east);
        count += self.cells[se] as u8;

        count
    }

    /// Advance the `Universe`.
    fn tick(&mut self) {
        let mut next = self.cells.clone();

        for row in 0..self.height {
            for col in 0..self.width {
                let idx = self.get_index(row, col);
                let cell = self.cells[idx];
                let live_neighbours = self.live_neighbour_count(row, col);

                next.set(
                    idx,
                    match (cell, live_neighbours) {
                        (true, x) if x < 2 => false,
                        (true, 2) | (true, 3) => true,
                        (true, x) if x > 3 => false,
                        (false, 3) => true,
                        (otherwise, _) => otherwise,
                    },
                );
            }
        }

        self.cells = next;
    }
}

fn canvas(window: &Window) -> HtmlCanvasElement {
    window
        .document()
        .unwrap()
        .get_element_by_id("game-of-life-canvas")
        .unwrap()
        .dyn_into()
        .unwrap()
}

fn draw_grid(colour: &JsValue, universe: &Universe, context: &CanvasRenderingContext2d) {
    context.begin_path();

    context.set_stroke_style(colour);

    let y_end = ((CELL_SIZE + 1) * universe.height + 1) as f64;
    let x_end = ((CELL_SIZE + 1) * universe.width + 1) as f64;

    for i in 0..=universe.width {
        let start = (i * (CELL_SIZE + 1) + 1) as f64;
        context.move_to(start, 0.0);
        context.line_to(start, y_end);
    }

    for j in 0..=universe.height {
        let start = (j * (CELL_SIZE + 1) + 1) as f64;
        context.move_to(0.0, start);
        context.line_to(x_end, start);
    }

    context.stroke();
}

fn draw_cells(
    alive: &JsValue,
    dead: &JsValue,
    universe: &Universe,
    context: &CanvasRenderingContext2d,
) {
    context.begin_path();

    // Live cells.
    context.set_fill_style(alive);
    for row in 0..universe.height {
        for col in 0..universe.width {
            let idx = universe.get_index(row, col);

            if universe.cells[idx] {
                context.fill_rect(
                    (col * (CELL_SIZE + 1) + 1) as f64,
                    (row * (CELL_SIZE + 1) + 1) as f64,
                    CELL_SIZE as f64,
                    CELL_SIZE as f64,
                );
            }
        }
    }

    // Dead cells.
    context.set_fill_style(dead);
    for row in 0..universe.height {
        for col in 0..universe.width {
            let idx = universe.get_index(row, col);

            if !universe.cells[idx] {
                context.fill_rect(
                    (col * (CELL_SIZE + 1) + 1) as f64,
                    (row * (CELL_SIZE + 1) + 1) as f64,
                    CELL_SIZE as f64,
                    CELL_SIZE as f64,
                );
            }
        }
    }

    context.stroke();
}

fn request_animation_frame(window: &Window, f: &Closure<dyn FnMut()>) {
    window
        .request_animation_frame(f.as_ref().unchecked_ref())
        .unwrap();
}

#[wasm_bindgen(start)]
pub fn main() {
    let mut universe = Universe::new();

    // Configure the canvas.
    let window = web_sys::window().unwrap();
    let canvas = canvas(&window);
    canvas.set_height((CELL_SIZE + 1) * universe.height + 1);
    canvas.set_width((CELL_SIZE + 1) * universe.width + 1);

    let context: CanvasRenderingContext2d = canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into()
        .unwrap();

    // Colours, converted to JS values once to avoid repeated conversions.
    let grid = GRID_COLOUR.into();
    let alive = ALIVE_COLOUR.into();
    let dead = DEAD_COLOUR.into();

    // Initial rendering here, since both these values are moved into the Closure.
    draw_grid(&grid, &universe, &context);
    draw_cells(&alive, &dead, &universe, &context);

    // Ref tricks to call `request_animation_frame` recursively.
    let f = Rc::new(RefCell::new(None));
    let g = f.clone();
    *g.borrow_mut() = Some(Closure::wrap(Box::new(move || {
        universe.tick();
        draw_grid(&grid, &universe, &context);
        draw_cells(&alive, &dead, &universe, &context);
        request_animation_frame(&window, f.borrow().as_ref().unwrap());
    }) as Box<dyn FnMut()>));

    // Grab the window again, since the previous one was moved into the Closure.
    let window = web_sys::window().unwrap();
    request_animation_frame(&window, g.borrow().as_ref().unwrap());
}
