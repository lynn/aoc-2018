// https://adventofcode.com/2018/day/15 in Rust.

use std::collections::{HashMap, HashSet};
use std::io;
use std::io::prelude::*;
use std::iter::Iterator;

type Integer = i32;
type Coordinate = Integer;
type Distance = Integer;
type HitPoints = Integer;

/// A point on a two-dimensional grid.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
struct Point {
    y: Coordinate,
    x: Coordinate,
}

impl Point {
    /// Return the Manhattan distance between two points.
    fn manhattan_distance(p: Point, q: Point) -> Distance {
        (p.x - q.x).abs() + (p.y - q.y).abs()
    }

    /// Is this point adjacent to the other one?
    fn adjacent_to(&self, other: Point) -> bool {
        Point::manhattan_distance(*self, other) == 1
    }

    /// Returns the four adjacent points on the grid, in reading order.
    fn neighbors(&self) -> Vec<Point> {
        let Point { x, y } = *self;
        return vec![
            Point { x, y: y - 1 },
            Point { x: x - 1, y },
            Point { x: x + 1, y },
            Point { x, y: y + 1 },
        ];
    }
}

/// A piece of terrain on the battlefield: a wall (`#`) or open space (`.`).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Terrain {
    Wall,
    Open,
}

/// The factions warring on the battlefield: goblins (`G`) and elves (`E`).
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
enum Faction {
    Goblin,
    Elf,
}

/// The terrain map of a battlefield.
type World = HashMap<Point, Terrain>;

/// A goblin or elf, fighting on the battlefield.
#[derive(Clone, Debug)]
struct Combatant {
    faction: Faction,
    position: Point,
    power: HitPoints,
    hit_points: HitPoints,
}

impl Combatant {
    const STANDARD_POWER: HitPoints = 3;
    const STANDARD_HIT_POINTS: HitPoints = 200;

    fn make(position: Point, faction: Faction) -> Combatant {
        Combatant {
            position,
            faction,
            power: Combatant::STANDARD_POWER,
            hit_points: Combatant::STANDARD_HIT_POINTS,
        }
    }

    fn dead(&self) -> bool {
        self.hit_points <= 0
    }

    fn alive(&self) -> bool {
        !self.dead()
    }
}

/// The outcome of a battle: how many full rounds it lasted, and how many hit
/// points remain in total. (The task asks for the product of these quantities.)
struct Outcome {
    round: Integer,
    total_hp: HitPoints,
}

impl Outcome {
    fn product(&self) -> Integer {
        self.round * self.total_hp
    }
}

/// A battlefield, ready to be simulated.
#[derive(Clone, Debug)]
struct Battlefield {
    world: World,
    combatants: Vec<Combatant>,
}

impl Battlefield {
    /// Checks if the given points is vacant (free of walls and combatants).
    fn is_vacant(&self, p: Point) -> bool {
        let occupied = self.combatants.iter().any(|c| c.alive() && c.position == p);
        self.world[&p] == Terrain::Open && !occupied
    }

    /// Returns a `HashMap` mapping `Point`s reachable from `start` to their
    /// distance in steps from `start`.
    fn points_reachable_from(&self, start: Point) -> HashMap<Point, Distance> {
        // Our finalized HashMap.
        let mut reach: HashMap<Point, Distance> = HashMap::new();

        // The running distance, and the set of all points `distance` steps away.
        let mut distance: Distance = 0;
        let mut frontier: HashSet<Point> = HashSet::new();
        frontier.insert(start);

        while !frontier.is_empty() {
            // Expand the frontier, inserting all previous points into `reach`.
            frontier = frontier
                .iter()
                .flat_map(|&p| {
                    reach.insert(p, distance);
                    p.neighbors()
                        .iter()
                        .filter(|&p| self.is_vacant(*p) && !reach.contains_key(p))
                        .cloned()
                        .collect::<Vec<Point>>()
                })
                .collect();
            distance += 1;
        }

        reach
    }

    /// Simulates a turn for the combatant with the given index into
    /// `self.combatants`. They'll try to move, and then try to attack.
    fn simulate_turn(&mut self, combatant_index: usize) -> () {
        if self.combatants[combatant_index].dead() {
            return;
        }

        let Combatant {
            faction: allied,
            position,
            hit_points: _,
            power,
        } = self.combatants[combatant_index];
        let mut here = position;

        // Phase 1: find a target to walk toward.
        let mut possible_targets: Vec<Point> = self
            .combatants
            .iter()
            .filter(|c| c.alive() && c.faction != allied)
            .flat_map(|c| c.position.neighbors())
            .collect();

        // Don't try to move if we're in a perfect spot to attack already.
        if !possible_targets.contains(&here) {
            // Okay, let's try to move. Find the closest reachable target:
            let reach = self.points_reachable_from(here);
            possible_targets.retain(|p| reach.contains_key(p));
            possible_targets.sort();
            let target = possible_targets.iter().min_by_key(|&t| reach[t]);

            match target {
                None => {
                    // No targets found. And if we don't even have anywhere we
                    // wanna walk, we won't attack either.
                    return;
                }
                Some(&t) => {
                    let distances = self.points_reachable_from(t);
                    let steps = here.neighbors();
                    let choice = steps
                        .iter()
                        .filter(|p| distances.contains_key(p))
                        .min_by_key(|&p| distances.get(p).unwrap())
                        .unwrap();

                    here = *choice;
                    self.combatants[combatant_index].position = here;
                }
            }
        }

        // Phase 2: try to attack a possible victim.
        let victim_index: Option<usize> = self
            .combatants
            .iter()
            .enumerate()
            .filter(|(_i, c)| c.alive() && c.faction != allied && c.position.adjacent_to(here))
            .map(|(i, c)| (c.hit_points, i))
            .min()
            .map(|(_hp, i)| i);

        victim_index.map(|i| self.combatants[i].hit_points -= power);
    }

    /// Simulate a round of battle: everyone takes a turn in reading order.
    /// After each round, we clean up the bodies.
    fn simulate_round(&mut self) -> () {
        self.combatants.sort_by_key(|c| c.position);
        for i in 0..self.combatants.len() {
            self.simulate_turn(i);
        }
        self.combatants.retain(|c| !c.dead());
    }

    /// Simulate the whole battle and return the outcome.
    fn simulate_battle(&mut self) -> Outcome {
        let mut round = -1;
        while self.ongoing() {
            round += 1;
            self.simulate_round();
        }
        let total_hp = self.combatants.iter().map(|c| c.hit_points).sum();
        Outcome { round, total_hp }
    }

    /// Simulate the whole battle for elf victory. Once an elf dies, None is
    /// returned. If no elves die, Some(outcome) is returned.
    fn simulate_elf_victory(&mut self) -> Option<Outcome> {
        let mut round = -1;
        while self.ongoing() {
            round += 1;
            let before = self.elf_count();
            self.simulate_round();
            let after = self.elf_count();
            if before > after {
                return None;
            }
        }
        let total_hp = self.combatants.iter().map(|c| c.hit_points).sum();
        Some(Outcome { round, total_hp })
    }

    /// Counts elves among the combatants.
    fn elf_count(&self) -> usize {
        self.combatants
            .iter()
            .filter(|c| c.faction == Faction::Elf)
            .count()
    }

    /// Returns whether the battle is ongoing, i.e. both factions are represented.
    fn ongoing(&mut self) -> bool {
        self.represented(Faction::Elf) && self.represented(Faction::Goblin)
    }

    /// Returns whether anyone from the given faction is still alive.
    fn represented(&mut self, faction: Faction) -> bool {
        self.combatants.iter().any(|c| c.faction == faction)
    }
}

/// Parse a Faction from a character of input.
fn parse_faction(ch: char) -> Faction {
    match ch {
        'G' => Faction::Goblin,
        'E' => Faction::Elf,
        _ => panic!("Invalid faction."),
    }
}

/// Parse some Terrain from a character of input.
fn parse_terrain(ch: char) -> Terrain {
    match ch {
        '#' => Terrain::Wall,
        '.' | 'G' | 'E' => Terrain::Open,
        _ => panic!("Invalid terrain."),
    }
}

/// Parse a Combatant from a character of input.
fn parse_combatant(position: Point, ch: char) -> Option<Combatant> {
    match ch {
        'G' | 'E' => Some(Combatant::make(position, parse_faction(ch))),
        _ => None,
    }
}

/// Parse a Battlefield from a list of input lines.
fn parse_battlefield(lines: Vec<String>) -> Battlefield {
    let mut world = HashMap::new();
    let mut combatants = Vec::new();
    for (y, line) in lines.iter().enumerate() {
        for (x, ch) in line.chars().enumerate() {
            let point = Point {
                x: x as Coordinate,
                y: y as Coordinate,
            };
            world.insert(point, parse_terrain(ch));
            parse_combatant(point, ch).map(|c| combatants.push(c));
        }
    }
    Battlefield { world, combatants }
}

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lock().lines().map(|line| line.unwrap()).collect();
    let original = parse_battlefield(lines);

    // Part 1: What's the unmodified outcome?
    let mut unmodified = original.clone();
    let outcome = unmodified.simulate_battle();
    println!("Unmodified, the outcome product is {}.", outcome.product());

    // Part 2: What if we beef up the elves until they stop dying?
    for elf_power in Combatant::STANDARD_POWER.. {
        // Make all the elves, like, totally kick ass.
        let mut what_if = original.clone();
        for c in &mut what_if.combatants {
            if c.faction == Faction::Elf {
                c.power = elf_power;
            }
        }

        // See if they win without dying now.
        match what_if.simulate_elf_victory() {
            None => {}
            Some(outcome) => {
                println!(
                    "At power {}, for the first time, no elves die, and the outcome product is {}.",
                    elf_power,
                    outcome.product()
                );
                break;
            }
        }
    }
}
