use crate::{
    renderable::{Offset, RenderOp},
    {gain::gain_at_index, loudness::loudness_normalization},
};

use reverb::Reverb;
use weresocool_ast::{OscType, ASR};
use weresocool_shared::{get_settings, Settings};

const SETTINGS: Settings = get_settings();

#[derive(Clone, Debug, PartialEq)]
pub struct Voice {
    pub reverb: ReverbState,
    pub index: usize,
    pub past: VoiceState,
    pub current: VoiceState,
    pub offset_past: VoiceState,
    pub offset_current: VoiceState,
    pub phase: f64,
    pub osc_type: OscType,
    pub attack: usize,
    pub decay: usize,
    pub asr: ASR,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SampleInfo {
    pub frequency: f64,
    pub gain: f64,
}

#[derive(Clone, Debug, PartialEq)]
/// Stores state which allow for a 1-step look back at the previously rendered values.
pub struct VoiceState {
    pub frequency: f64,
    pub gain: f64,
    pub osc_type: OscType,
    pub reverb: Option<f64>,
}

impl VoiceState {
    pub const fn init() -> Self {
        Self {
            frequency: 0.0,
            gain: 0.0,
            osc_type: OscType::None,
            reverb: None,
        }
    }

    /// Check if the previous voice state was silent
    pub fn silent(&self) -> bool {
        self.frequency < SETTINGS.min_freq || self.gain == 0.0
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReverbState {
    model: Reverb,
    state: Option<f64>,
}

impl ReverbState {
    pub fn init() -> Self {
        Self {
            model: Reverb::new(),
            state: None,
        }
    }
}

impl Voice {
    pub fn init(index: usize, settings: Settings) -> Self {
        Self {
            index,
            reverb: ReverbState::init(),
            past: VoiceState::init(),
            current: VoiceState::init(),
            offset_past: VoiceState::init(),
            offset_current: VoiceState::init(),
            phase: 0.0,
            osc_type: OscType::Sine { pow: None },
            attack: settings.sample_rate as usize,
            decay: settings.sample_rate as usize,
            asr: ASR::Long,
        }
    }

    /// Renders a single RenderOp given an Offset
    /// This is where all of the rendering logic for a single render_op happens
    pub fn generate_waveform(&mut self, op: &RenderOp, offset: &Offset) -> Vec<f64> {
        let mut buffer: Vec<f64> = vec![0.0; op.samples];

        let p_delta = self.calculate_portamento_delta(
            op.portamento,
            self.offset_past.frequency,
            self.offset_current.frequency,
        );

        let op_gain = self.calculate_op_gain(
            self.silence_now(),
            self.silence_next(op),
            op.index + op.samples,
            op.total_samples,
        ) * loudness_normalization(self.offset_current.frequency);

        self.reverb.model.update(if self.current.reverb.is_some() {
            self.current.reverb.unwrap() as f32
        } else {
            0.0
        });

        for (index, sample) in buffer.iter_mut().enumerate() {
            let frequency = self.calculate_frequency(
                index,
                op.portamento,
                p_delta,
                self.offset_past.frequency,
                self.offset_current.frequency,
            );
            let gain = gain_at_index(
                self.offset_past.gain,
                op_gain * offset.gain,
                index,
                if op.samples > 250 { op.samples } else { 250 },
            );

            let info = SampleInfo { frequency, gain };

            let mut new_sample = match self.osc_type {
                OscType::None => self.generate_sine_sample(info, None),
                OscType::Sine { pow } => self.generate_sine_sample(info, pow),
                OscType::Triangle { pow } => self.generate_triangle_sample(info, pow),
                OscType::Square { width } => self.generate_square_sample(info, width),
                OscType::Noise => self.generate_random_sample(info),
            };

            if self.reverb.state.is_some() && self.reverb.state.unwrap() > 0.0 && gain > 0.0 {
                new_sample = self
                    .reverb
                    .model
                    .calc_sample(new_sample as f32, gain as f32)
                    .into();
            }

            if index == op.samples - 1 {
                self.offset_current.frequency = frequency;
                self.offset_current.gain = gain;
            };

            *sample += new_sample
        }

        buffer.to_vec()
    }

    pub fn update(&mut self, op: &RenderOp, offset: &Offset) {
        if op.index == 0 {
            self.past.frequency = self.current.frequency;
            self.current.frequency = op.f;
            self.past.osc_type = self.current.osc_type;
            self.past.reverb = self.current.reverb;

            self.past.gain = self.past_gain_from_op(op);
            self.current.gain = self.current_gain_from_op(op);

            self.osc_type = if self.past.osc_type.is_some() && op.osc_type.is_none() {
                self.past.osc_type
            } else {
                op.osc_type
            };

            self.reverb.state = if self.past.reverb.is_some() && op.reverb.is_none() {
                self.past.reverb
            } else {
                op.reverb
            };

            self.attack = op.attack.trunc() as usize;
            self.decay = op.decay.trunc() as usize;
            self.asr = op.asr;
            self.current.osc_type = op.osc_type;
            self.current.reverb = op.reverb;
        };
        self.offset_past.gain = self.offset_current.gain;
        self.offset_past.frequency = self.offset_current.frequency;

        self.offset_current.frequency = if self.sound_to_silence() {
            self.past.frequency * offset.freq
        } else {
            self.current.frequency * offset.freq
        }
    }
}
