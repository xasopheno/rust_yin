use crate::{
    renderable::{Offset, RenderOp},
    {stereo_waveform::StereoWaveform, voice::Voice},
};
use num_rational::Rational64;
use weresocool_parser::Init;

#[derive(Clone, Debug, PartialEq)]
pub struct Oscillator {
    pub voices: (Voice, Voice),
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
    pub fn init() -> Self {
        Self {
            voices: (Voice::init(0), Voice::init(1)),
        }
    }

    pub fn copy_state_from(&mut self, other: &Oscillator) {
        self.voices.0.copy_state_from(&other.voices.0);
        self.voices.1.copy_state_from(&other.voices.1);
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
