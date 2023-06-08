mod test;
use num_rational::Rational64;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::f64::consts::PI;
use std::hash::{Hash, Hasher};
use weresocool_shared::{r_to_f64, Settings};

const TAU: f64 = PI * 2.0;

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Hash, Ord, Serialize, Deserialize, Copy)]
pub enum BiquadFilterType {
    Lowpass,
    Highpass,
    Bandpass,
    // Notch,
    // Peak,
    // LowShelf,
    // HighShelf,
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Hash, Ord, Serialize, Deserialize)]
pub struct BiquadFilterDef {
    pub hash: String,
    pub filter_type: BiquadFilterType,
    pub cutoff_frequency: Rational64,
    pub q_factor: Rational64,
}

impl BiquadFilterDef {
    pub fn to_filter(&self) -> BiquadFilter {
        match self.filter_type {
            BiquadFilterType::Lowpass => lowpass_filter(
                self.hash.clone(),
                r_to_f64(self.cutoff_frequency),
                r_to_f64(self.q_factor),
            ),
            BiquadFilterType::Highpass => highpass_filter(
                self.hash.clone(),
                r_to_f64(self.cutoff_frequency),
                r_to_f64(self.q_factor),
            ),
            BiquadFilterType::Bandpass => highpass_filter(
                self.hash.clone(),
                r_to_f64(self.cutoff_frequency),
                r_to_f64(self.q_factor),
            ),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BiquadFilter {
    pub hash: String,
    /// Feedforward coefficients for the current and two previous inputs
    feedforward_coefs: [f64; 3],
    /// Feedback coefficients for the two previous outputs
    feedback_coefs: [f64; 3],
    /// The two most recent input values
    input_history: [f64; 2],
    /// The two most recent output values
    output_history: [f64; 2],
}

impl Ord for BiquadFilter {
    fn cmp(&self, other: &Self) -> Ordering {
        self.feedforward_coefs[0]
            .partial_cmp(&other.feedforward_coefs[0])
            .unwrap_or(Ordering::Equal)
    }
}

impl PartialOrd for BiquadFilter {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.feedforward_coefs[0].partial_cmp(&other.feedforward_coefs[0])
    }
}

impl Hash for BiquadFilter {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for coef in &self.feedforward_coefs {
            coef.to_bits().hash(state);
        }
        for coef in &self.feedback_coefs {
            coef.to_bits().hash(state);
        }
        for hist in &self.input_history {
            hist.to_bits().hash(state);
        }
        for hist in &self.output_history {
            hist.to_bits().hash(state);
        }
    }
}

impl Eq for BiquadFilter {}

impl BiquadFilter {
    pub fn new(hash: String, feedforward_coefs: [f64; 3], feedback_coefs: [f64; 3]) -> Self {
        BiquadFilter {
            hash,
            feedforward_coefs,
            feedback_coefs,
            input_history: [0.0; 2],
            output_history: [0.0; 2],
        }
    }

    pub fn process(&mut self, input_sample: f64) -> f64 {
        let filtered_output = self.feedforward_coefs[0] * input_sample
            + self.feedforward_coefs[1] * self.input_history[0]
            + self.feedforward_coefs[2] * self.input_history[1]
            - self.feedback_coefs[1] * self.output_history[0]
            - self.feedback_coefs[2] * self.output_history[1];

        self.update_history(input_sample, filtered_output);
        filtered_output
    }

    fn update_history(&mut self, input_sample: f64, filtered_output: f64) {
        self.input_history.rotate_right(1);
        self.input_history[0] = input_sample;
        self.output_history.rotate_right(1);
        self.output_history[0] = filtered_output;
    }
}

pub fn lowpass_filter(hash: String, cutoff_frequency: f64, quality_factor: f64) -> BiquadFilter {
    let (feedforward_coefs, feedback_coefs) = lowpass_coefs(cutoff_frequency, quality_factor);
    BiquadFilter::new(hash, feedforward_coefs, feedback_coefs)
}

pub fn highpass_filter(hash: String, cutoff_frequency: f64, quality_factor: f64) -> BiquadFilter {
    let (feedforward_coefs, feedback_coefs) = highpass_coefs(cutoff_frequency, quality_factor);
    BiquadFilter::new(hash, feedforward_coefs, feedback_coefs)
}

pub fn bandpass_filter(hash: String, cutoff_frequency: f64, quality_factor: f64) -> BiquadFilter {
    let (feedforward_coefs, feedback_coefs) = bandpass_coefs(cutoff_frequency, quality_factor);
    BiquadFilter::new(hash, feedforward_coefs, feedback_coefs)
}

pub fn lowpass_coefs(cutoff_frequency: f64, quality_factor: f64) -> ([f64; 3], [f64; 3]) {
    let normalized_cutoff = TAU * cutoff_frequency / Settings::global().sample_rate;
    let alpha = normalized_cutoff.sin() / (2.0 * quality_factor);
    let normalization_factor = 1.0 + alpha;
    let cos_normalized_cutoff = normalized_cutoff.cos();
    let common_feedforward = (1.0 - cos_normalized_cutoff) / 2.0 / normalization_factor;

    let feedforward_coefs = [
        common_feedforward,
        2.0 * common_feedforward,
        common_feedforward,
    ];
    let feedback_coefs = [
        1.0,
        -2.0 * cos_normalized_cutoff / normalization_factor,
        (1.0 - alpha) / normalization_factor,
    ];

    (feedforward_coefs, feedback_coefs)
}

pub fn highpass_coefs(cutoff_frequency: f64, quality_factor: f64) -> ([f64; 3], [f64; 3]) {
    let normalized_cutoff = TAU * cutoff_frequency / Settings::global().sample_rate;
    let alpha = normalized_cutoff.sin() / (2.0 * quality_factor);
    let normalization_factor = 1.0 + alpha;
    let cos_normalized_cutoff = normalized_cutoff.cos();
    let common_feedforward = (1.0 + cos_normalized_cutoff) / 2.0 / normalization_factor;

    let feedforward_coefs = [
        common_feedforward,
        -2.0 * common_feedforward,
        common_feedforward,
    ];
    let feedback_coefs = [
        1.0,
        -2.0 * cos_normalized_cutoff / normalization_factor,
        (1.0 - alpha) / normalization_factor,
    ];

    (feedforward_coefs, feedback_coefs)
}

pub fn bandpass_coefs(center_frequency: f64, quality_factor: f64) -> ([f64; 3], [f64; 3]) {
    let normalized_center = TAU * center_frequency / Settings::global().sample_rate;
    let alpha = normalized_center.sin() / (2.0 * quality_factor);
    let cos_normalized_center = normalized_center.cos();

    let mut feedforward_coefs = [alpha, 0.0, -alpha];
    let mut feedback_coefs = [1.0 + alpha, -2.0 * cos_normalized_center, 1.0 - alpha];

    let normalization_factor = feedback_coefs[0];
    feedforward_coefs
        .iter_mut()
        .for_each(|x| *x /= normalization_factor);
    feedback_coefs
        .iter_mut()
        .for_each(|x| *x /= normalization_factor);

    (feedforward_coefs, feedback_coefs)
}
