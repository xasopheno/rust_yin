use serde::{Deserialize, Serialize};
use std::cmp;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StereoWaveform {
    pub l_buffer: Vec<f64>,
    pub r_buffer: Vec<f64>,
}

pub trait Normalize {
    fn normalize(&mut self);
}

impl StereoWaveform {
    pub fn new(buffer_size: usize) -> Self {
        Self {
            l_buffer: vec![0.0; buffer_size],
            r_buffer: vec![0.0; buffer_size],
        }
    }

    pub fn max_len(&self) -> usize {
        cmp::max(self.l_buffer.len(), self.r_buffer.len())
    }

    pub fn total_len(&self) -> usize {
        self.l_buffer.len() + self.r_buffer.len()
    }

    pub fn append(&mut self, mut stereo_waveform: Self) {
        self.l_buffer.append(&mut stereo_waveform.l_buffer);
        self.r_buffer.append(&mut stereo_waveform.r_buffer);
    }

    /// This assumes that all buffers are the same size
    pub fn get_buffer(&mut self, index: usize, buffer_size: usize) -> Option<Self> {
        if (index + 1) * buffer_size < self.l_buffer.len() {
            let l_buffer = &self.l_buffer[index * buffer_size..(index + 1) * buffer_size];
            let r_buffer = &self.r_buffer[index * buffer_size..(index + 1) * buffer_size];
            Some(Self {
                l_buffer: l_buffer.to_vec(),
                r_buffer: r_buffer.to_vec(),
            })
        } else {
            None
        }
    }
}

impl Normalize for StereoWaveform {
    fn normalize(&mut self) {
        let mut max = std::f64::MIN;
        for sample in self.l_buffer.iter() {
            if (*sample).abs() > max {
                max = *sample;
            }
        }

        for sample in self.r_buffer.iter() {
            if (*sample).abs() > max {
                max = *sample;
            }
        }

        let mut normalization_ratio = 1.0 / max;
        if normalization_ratio > 1.0 {
            normalization_ratio = 1.0
        }

        println!("Normalized by {:?}", normalization_ratio);

        if normalization_ratio < 1.0 {
            for sample in self.l_buffer.iter_mut() {
                *sample *= normalization_ratio
            }

            for sample in self.r_buffer.iter_mut() {
                *sample *= normalization_ratio
            }
        }
    }
}
