use crate::{
    generation::parsed_to_render::{RenderReturn, RenderType},
    generation::sum_all_waveforms,
    interpretable::{InputType, Interpretable},
};
#[cfg(feature = "app")]
use rayon::prelude::*;
use std::sync::mpsc::Sender;
use std::{path::PathBuf, sync::mpsc::SendError};
use weresocool_error::Error;
use weresocool_instrument::renderable::{
    nf_to_vec_renderable, renderables_to_render_voices, RenderVoice,
};
use weresocool_instrument::StereoWaveform;

pub type KillChannel = Option<Sender<bool>>;

#[derive(Clone, Debug)]
pub struct RenderManager {
    pub renders: [Option<Vec<RenderVoice>>; 2],
    pub current_volume: f32,
    pub past_volume: f32,
    render_idx: usize,
    _read_idx: usize,
    kill_channel: KillChannel,
    once: bool,
}

impl RenderManager {
    pub const fn init(
        render_voices: Vec<RenderVoice>,
        kill_channel: KillChannel,
        once: bool,
    ) -> Self {
        Self {
            renders: [Some(render_voices), None],
            past_volume: 0.8,
            current_volume: 0.8,
            render_idx: 0,
            _read_idx: 0,
            kill_channel,
            once,
        }
    }

    pub const fn init_silent() -> Self {
        Self {
            renders: [None, None],
            past_volume: 0.8,
            current_volume: 0.8,
            render_idx: 0,
            _read_idx: 0,
            kill_channel: None,
            once: false,
        }
    }

    pub fn kill(&self) -> Result<(), SendError<bool>> {
        if let Some(kc) = &self.kill_channel {
            kc.send(true)?;
            #[cfg(target_os = "linux")]
            std::thread::sleep(std::time::Duration::from_millis(500));
            Ok(())
        } else {
            Ok(())
        }
    }

    pub fn update_volume(&mut self, volume: f32) {
        self.current_volume = f32::powf(volume, 2.0)
    }

    fn ramp_to_current_volume(&mut self, buffer_size: usize) -> Vec<f32> {
        let offset: Vec<f32> = (0..buffer_size * 2)
            .map(|i| {
                let distance = self.current_volume - self.past_volume;
                self.past_volume + (distance * i as f32 / (buffer_size * 2) as f32)
            })
            .collect();

        self.past_volume = self.current_volume;

        offset
    }

    pub fn read(&mut self, buffer_size: usize) -> Option<(StereoWaveform, Vec<f32>)> {
        let next = self.exists_next_render();
        let current = self.current_render();

        match current {
            Some(render_voices) => {
                #[cfg(feature = "app")]
                let iter = render_voices.par_iter_mut();
                #[cfg(feature = "wasm")]
                let iter = render_voices.iter_mut();

                let rendered: Vec<StereoWaveform> = iter
                    .filter_map(|voice| voice.render_batch(buffer_size, None))
                    .collect();
                if !rendered.is_empty() {
                    let mut sw: StereoWaveform = sum_all_waveforms(rendered);

                    if next {
                        sw.fade_out();

                        *current = None;
                        self.inc_render();
                    }

                    sw.pad(buffer_size);

                    let ramp = self.ramp_to_current_volume(buffer_size);
                    Some((sw, ramp))
                } else {
                    *self.current_render() = None;

                    if self.once {
                        self.kill().expect("Not able to kill");
                        None
                    } else {
                        None
                    }
                }
            }
            None => {
                if next {
                    self.inc_render();
                    self.read(buffer_size)
                } else {
                    None
                }
            }
        }
    }

    pub fn inc_render(&mut self) {
        self.render_idx = (self.render_idx + 1) % 2;
    }

    pub fn current_render(&mut self) -> &mut Option<Vec<RenderVoice>> {
        &mut self.renders[self.render_idx]
    }

    pub fn next_render(&mut self) -> &mut Option<Vec<RenderVoice>> {
        &mut self.renders[(self.render_idx + 1) % 2]
    }

    pub fn exists_current_render(&mut self) -> bool {
        self.current_render().is_some()
    }

    pub fn exists_next_render(&mut self) -> bool {
        self.next_render().is_some()
    }

    pub fn push_render(&mut self, render: Vec<RenderVoice>) {
        *self.next_render() = Some(render);
    }
}

pub fn prepare_render_outside(
    input: InputType<'_>,
    working_path: Option<PathBuf>,
) -> Result<Vec<RenderVoice>, Error> {
    let (nf, basis, mut table) = match input.make(RenderType::NfBasisAndTable, working_path)? {
        RenderReturn::NfBasisAndTable(nf, basis, table) => (nf, basis, table),
        _ => return Err(Error::with_msg("Failed Parse/Render")),
    };
    let renderables = nf_to_vec_renderable(&nf, &mut table, &basis)?;

    let render_voices = renderables_to_render_voices(renderables);

    Ok(render_voices)
}

#[cfg(test)]
mod render_manager_tests {
    use super::*;
    use weresocool_instrument::renderable::RenderOp;
    use weresocool_shared::helpers::{cmp_f32, cmp_vec_f32};

    #[test]
    fn test_ramp_to_current_value() {
        let mut rm = RenderManager::init_silent();
        rm.update_volume(0.9);
        assert!(cmp_f32(rm.current_volume, f32::powf(0.9, 2.0)));
        let ramp = rm.ramp_to_current_volume(2);
        dbg!(&ramp);
        assert!(cmp_vec_f32(
            ramp,
            vec![0.8, 0.8025, 0.80499995, 0.807_499_95]
        ));
    }

    #[test]
    fn test_inc_render() {
        let mut r = RenderManager::init_silent();
        r.inc_render();
        assert_eq!(r.render_idx, 1);
        r.inc_render();
        assert_eq!(r.render_idx, 0);
    }

    fn render_voices_mock() -> Vec<RenderVoice> {
        vec![RenderVoice::init(&[RenderOp::init_silent_with_length(1.0)])]
    }

    #[test]
    fn test_push_render() {
        let mut r = RenderManager::init(render_voices_mock(), None, false);
        assert_eq!(*r.current_render(), Some(render_voices_mock()));
        assert_eq!(*r.next_render(), None);
        r.push_render(render_voices_mock());
        assert_eq!(*r.current_render(), Some(render_voices_mock()));
        assert_eq!(*r.next_render(), Some(render_voices_mock()));
    }
}
