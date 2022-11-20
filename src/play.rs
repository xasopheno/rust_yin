use crate::watch::watch;
use crate::Error;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use weresocool::interpretable::InputType::Filename;
use weresocool::manager::prepare_render_outside;
use weresocool::manager::RenderManager;
use weresocool::portaudio::real_time_render_manager;
use weresocool::ui::were_so_cool_logo;
use weresocool_instrument::RenderVoice;

pub enum Play {
    Once,
    Watch,
}

pub fn play(filename: &String, cwd: PathBuf, play: Play) -> Result<(), Error> {
    play_file(filename.to_owned(), cwd, play)?;
    Ok(())
}

pub fn play_file(filename: String, working_path: PathBuf, play: Play) -> Result<(), Error> {
    match play {
        Play::Once => {
            let render_voices = prepare_render_outside(Filename(&filename), Some(working_path));

            play_once(render_voices?, filename)
        }
        Play::Watch => play_watch(filename, working_path),
    }
}

pub fn play_once(render_voices: Vec<RenderVoice>, filename: String) -> Result<(), Error> {
    were_so_cool_logo(Some("Playing"), Some(filename));

    let (tx, rx) = std::sync::mpsc::channel::<bool>();
    let render_manager = Arc::new(Mutex::new(RenderManager::init(
        render_voices,
        None,
        Some(tx),
        true,
    )));
    let mut stream = real_time_render_manager(Arc::clone(&render_manager))?;

    stream.start()?;
    // rx.recv blocks until it receives data and
    // after that, the function will complete,
    // stream will be dropped, and the application
    // will exit.
    match rx.recv() {
        Ok(_) => {}
        Err(e) => {
            println!("{}", e);
            std::process::exit(1);
        }
    };
    Ok(())
}

fn play_watch(filename: String, working_path: PathBuf) -> Result<(), Error> {
    let render_manager = Arc::new(Mutex::new(RenderManager::init(vec![], None, None, false)));
    watch(filename, working_path, render_manager.clone())?;
    let mut stream = real_time_render_manager(Arc::clone(&render_manager))?;
    stream.start()?;
    std::thread::park();
    Ok(())
}
