use crate::voice::{SampleInfo, Voice};
use std::f64::consts::PI;
use weresocool_ast::OscType;
use weresocool_shared::{r_to_f64, Settings};

const TAU: f64 = PI * 2.0;

use rand::{thread_rng, Rng};
pub fn random_offset() -> f64 {
    thread_rng().gen_range(-0.5, 0.5)
}

impl Voice {
    pub fn calculate_current_phase(&mut self, info: &SampleInfo, osc_type: OscType) -> f64 {
        let rand = if osc_type == OscType::Noise {
            random_offset()
        } else {
            0.0
        };

        if info.gain == 0.0 {
            0.0
        } else {
            ((TAU / Settings::global().sample_rate).mul_add(info.frequency, self.phase) + rand)
                % TAU
        }
    }
}
pub trait Waveform {
    fn generate_sample(&self, info: SampleInfo, phase: f64) -> f64;
}

impl Waveform for OscType {
    fn generate_sample(&self, info: SampleInfo, phase: f64) -> f64 {
        match self {
            OscType::None => phase.sin() * info.gain,
            OscType::Sine { pow } => {
                let value = match pow {
                    Some(p) => {
                        let power = r_to_f64(*p);
                        f64::powf(phase, power).sin() / power
                    }
                    None => phase.sin(),
                };
                value * info.gain
            }
            OscType::Triangle { pow } => {
                let value = match pow {
                    Some(p) => {
                        let power = r_to_f64(*p);
                        (f64::powf(phase, power).sin().abs() * 2.0 - 1.0) / power
                    }
                    None => phase.sin().abs() * 2.0 - 1.0,
                };
                value * info.gain
            }
            OscType::Square { width } => {
                let pulse_width = if let Some(w) = width {
                    r_to_f64(*w)
                } else {
                    0.0
                };

                let s = phase.sin();
                let sign = if s > pulse_width { -1. } else { 1. };
                sign * info.gain
            }
            OscType::Saw => 2.0 * (phase / TAU - 0.5_f64.floor()) * info.gain,
            OscType::Noise => phase.sin() * info.gain,
        }
    }
}
