use fixedbitset::FixedBitSet;
use js_sys::Math;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::console;
use web_sys::{CanvasRenderingContext2d, HtmlCanvasElement};

const PI: f64 = 3.1415683859;
const CELL_SIZE: u32 = 5;
const GRID_COLOUR: &str = "#CCCCCC";
const DEAD_COLOUR: &str = "#FFFFFF";
const ALIVE_COLOUR: &str = "#000000";

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

struct Universe {
    width: u32,
    height: u32,
    cells: FixedBitSet,
}

impl Universe {
    fn new() -> Universe {
        let width = 64;
        let height = 64;
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

fn canvas() -> HtmlCanvasElement {
    web_sys::window()
        .unwrap()
        .document()
        .unwrap()
        .get_element_by_id("game-of-life-canvas")
        .unwrap()
        .dyn_into()
        .unwrap()
}

fn draw_grid(universe: &Universe, context: &CanvasRenderingContext2d) {
    context.begin_path();

    context.set_stroke_style(&GRID_COLOUR.into()); // TODO Avoid allocation.

    let y_end = ((CELL_SIZE + 1) * universe.height + 1) as f64;

    for i in 0..universe.width {
        let start = (i * (CELL_SIZE + 1) + 1) as f64;
        context.move_to(start, 0.0);
        context.line_to(start, y_end);
    }

    context.stroke();
}

#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    console::log_1(&"Hello from Rust!".into());

    let universe = Universe::new();

    let canvas = canvas();
    canvas.set_height((CELL_SIZE + 1) * universe.height + 1);
    canvas.set_width((CELL_SIZE + 1) * universe.width + 1);

    let context: CanvasRenderingContext2d = canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into()
        .unwrap();

    draw_grid(&universe, &context);

    Ok(())
}
