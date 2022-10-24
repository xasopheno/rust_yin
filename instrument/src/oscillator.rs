use crate::{
    renderable::{Offset, RenderOp},
    {stereo_waveform::StereoWaveform, voice::Voice},
};
use num_rational::Rational64;
use weresocool_parser::Init;
use weresocool_shared::Settings;

#[derive(Clone, Debug, PartialEq)]
pub struct Oscillator {
    pub voices: (Voice, Voice),
    pub settings: Settings,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Basis {
    pub f: Rational64,
    pub p: Rational64,
    pub g: Rational64,
    pub l: Rational64,
    pub a: Rational64,
    pub d: Rational64,
}

impl Basis {
    pub fn from_init(init: &Init) -> Self {
        Self {
            f: init.f,
            g: init.g,
            l: init.l,
            p: init.p,
            a: Rational64::new(1, 1),
            d: Rational64::new(1, 1),
        }
    }
}

impl Oscillator {
    pub fn init(settings: &Settings) -> Self {
        Self {
            voices: (Voice::init(0), Voice::init(1)),
            settings: settings.clone(),
        }
    }

    pub fn update(&mut self, op: &RenderOp, offset: &Offset) {
        let (ref mut l_voice, ref mut r_voice) = self.voices;
        l_voice.update(op, offset);
        r_voice.update(op, offset);
    }

    pub fn generate(&mut self, op: &RenderOp, offset: &Offset) -> StereoWaveform {
        let (ref mut l_voice, ref mut r_voice) = self.voices;

        let l_buffer = l_voice.generate_waveform(op, offset);
        let r_buffer = r_voice.generate_waveform(op, offset);

        StereoWaveform { l_buffer, r_buffer }
    }
}
