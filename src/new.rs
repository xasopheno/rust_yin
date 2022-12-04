use crate::play::{play_file, Play};
use crate::Error;
use indoc::indoc;
use std::fs;
use std::path::PathBuf;

pub fn new(filename: &String, cwd: PathBuf) -> Result<(), Error> {
    new_socool_file(filename.to_owned(), cwd)?;
    Ok(())
}

fn new_socool_file(filename: String, working_path: PathBuf) -> Result<(), Error> {
    let path = working_path.join(format!("{filename}.socool"));

    fs::write(path, DEFAULT_SOCOOL).expect("Unable to write file");
    play_file(format!("{filename}.socool"), working_path, Play::Once)?;
    Ok(())
}

const DEFAULT_SOCOOL: &str = indoc! {"
{ f: 311.127, l: 1, g: 1/3, p: 0 }

thing1 = {
    O[
        (1/1, 2, 1, 1),
        (1/1, 0, 1, -1),
    ]
    | Seq [
        Fm 1, Fm 9/8, Fm 5/4
   ]
}

thing2 = {
    O[
        (1/1, 2, 1, 1),
        (1/1, 0, 1, -1),
    ]
    | Seq [
        Fm 3/4
    ]
    | FitLength thing1
}

main = {
    Overlay [
        thing1,
        thing2
    ]
}
"};
