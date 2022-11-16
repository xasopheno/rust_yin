use weresocool_shared::{get_settings, Settings};

const SETTINGS: Settings = get_settings();

pub fn freq_to_sones(frequency: f64) -> f64 {
    // http://www.ukintpress-conferences.com/conf/08txeu_conf/pdf/day_1/01-06-garcia.pdf
    if frequency < SETTINGS.min_freq {
        0.0
    } else {
        1.0 / (((20.0 * (frequency).log10()) - 40.0) / 10.0).exp2()
    }
}

pub fn loudness_normalization(frequency: f64) -> f64 {
    let mut normalization = freq_to_sones(frequency);
    if normalization.is_nan() || normalization.is_infinite() || normalization > 1.0 {
        normalization = 1.0;
    };
    normalization
}
