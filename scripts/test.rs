#!/usr/bin/env -S cargo +nightly -Zscript
---
[package]
edition = "2024"

[dependencies]
colored = "3.1.1"
pretty-duration = "0.1.1"
---

//! Locally runs the same test commands CI runs. Useful to run before pushing.

use std::{
    process::{Command, Stdio},
    time::Instant,
};

use colored::Colorize;
use pretty_duration::pretty_duration;

const RUST_TOOLCHAIN_VERSION: &str = "1.97.0";

fn main() {
    let mut commands = Vec::new();

    commands.push(cargo_command("fmt", &["--check", "--all"]));
    commands.push(cargo_command("doc", &["--no-deps"]));
    commands.push(cargo_command("clippy", &[]));
    commands.push(cargo_command("test", &[]));

    run_commands(commands);
}

fn cargo_command(cargo_command: &str, cargo_command_args: &[&str]) -> Command {
    let mut command = Command::new("cargo");

    command.arg(format!("+{RUST_TOOLCHAIN_VERSION}"));
    command.arg(cargo_command);
    command.args(cargo_command_args);
    command.env("RUSTFLAGS", "-D warnings");
    command.env("RUSTDOCFLAGS", "-D warnings");

    command
}

fn run_commands(commands: Vec<Command>) {
    let start_instant = Instant::now();

    let total_command_count = commands.len();
    let mut completed_command_count = 0;

    for mut command in commands {
        let command_str = format!("{}", std::fmt::from_fn(|f| format_command(&command, f)));

        println!();
        println!("Commands: {completed_command_count}/{total_command_count}");
        println!("{command_str}");
        println!();

        let command_output = command
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .output()
            .expect("failed to run command");

        if !command_output.status.success() {
            println!();
            println!("Commands: {completed_command_count}/{total_command_count}");
            println!("{command_str}");
            println!();
            println!("{}: command failed", "error".red().bold());
            return;
        }

        completed_command_count += 1;
    }

    let time = pretty_duration(&Instant::now().duration_since(start_instant), None);

    println!();
    println!("Commands: {completed_command_count}/{total_command_count}");
    println!("Time: {time:?}");
    println!("{}", "testing passed".green().bold());
}

fn format_command(command: &Command, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for (key, value) in command.get_envs() {
        let key = key.to_str().expect("invalid utf8");
        let value = value
            .expect("do not remove environment variables")
            .to_str()
            .expect("invalid utf8");

        if value.contains(' ') {
            write!(f, "{key}=\"{value}\" ")?;
        } else {
            write!(f, "{key}={value} ")?;
        }
    }

    write!(f, "{}", command.get_program().display())?;

    for arg in command.get_args() {
        let arg = arg.to_str().expect("invalid utf8");

        if arg.contains(' ') {
            write!(f, " \"{arg}\"")?;
        } else {
            write!(f, " {arg}")?;
        }
    }

    Ok(())
}
