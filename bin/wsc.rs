use weresocool::ui::{
    // banner,
    get_args,
    no_file_name,
    were_so_cool_logo,
};
// use weresocool_error::Error;

fn main() {
    were_so_cool_logo();
    let args = get_args();

    let filename = args.value_of("filename");
    match filename {
        Some(_filename) => {}
        _ => no_file_name(),
    }

    // if args.is_present("print") {
    // Filename(filename.unwrap()).make(RenderType::Wav(WavType::Mp3 { cli: true }), None)?;
    // } else if args.is_present("json") {
    // Filename(filename.unwrap()).make(RenderType::Json4d, None)?;
    // } else if args.is_present("csv") {
    // Filename(filename.unwrap()).make(RenderType::Csv1d, None)?;
    // } else {
    // let stereo_waveform = match Filename(filename.unwrap()).make(RenderType::StereoWaveform)? {
    // RenderReturn::StereoWaveform(sw) => sw,
    // _ => panic!("Error. Unable to return StereoWaveform"),
    // };

    // let mut output_stream = output_setup(stereo_waveform)?;
    // banner("Now Playing".to_string(), filename.unwrap().to_string());

    // output_stream.start()?;
    // while let true = output_stream.is_active()? {}
    // output_stream.stop()?;
}
