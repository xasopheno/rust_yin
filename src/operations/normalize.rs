pub mod normalize {
    use operations::{NormalForm, Normalize, GetLengthRatio};
    use socool_parser::ast::{Op, Op::*};
    use std::cmp::Ordering::{Less, Greater, Equal};

    impl Normalize for Op {
        fn apply_to_normal_form(&self, input: NormalForm) -> NormalForm {
            let mut output: NormalForm = vec![];
            match self {
                Op::AsIs => {
                    output = input
                }

                Op::TransposeM { m } => {
                    let mut result = vec![];
                    for mut voice in input {
                        let mut new_voice = vec![];
                        for op in voice {
                            new_voice.push(
                                Op::Compose {
                                    operations: vec![op, Op::TransposeM { m: *m }]
                                }
                            )
                        }
                        result.push(new_voice.clone())
                    }
                    output = result
                }
//                | Op::TransposeA { a: _ }
//                | Op::PanA { a: _ }
//                | Op::PanM { m: _ }
//                | Op::Gain { m: _ }
//
//                Op::Length { m: _ } |

                Op::Silence { m } => {
                    let mut result = vec![];

                    let max_len = get_max_length_ratio(&input);

                    for _i in 0..input.len() {
                        result.push(vec![Op::Silence { m: max_len * m }])
                    }

                    output = result
                },
//
                Op::Sequence { operations } => {
                    println!("INPUTTT {:?}", input);
                    let mut result = vec![];

                    for op in operations {
                        result = join_sequence(
                            result,
                            op.apply_to_normal_form(input.clone()));

                    }

                    output = result
                },

                Op::Compose { operations } => {
                    let mut result = vec![];
                    for op in operations {
                        result.append(&mut op.apply_to_normal_form(input.clone()));
                    }

                    output = result
                }
//
//                Op::WithLengthRatioOf {
//                    with_length_of: _,
//                    main: _,
//                } => None,

                Op::Overlay { operations } => {
                    let mut voices = vec![];
                    for op in operations {
                        let result = op.apply_to_normal_form(input.clone());
                        if result.len() > 0 {
                            voices.append(&mut result.clone());
                        }
                    }


                    output = voices
                }
            }

//            match_length(&mut output);
            println!(">>>>>> OUTPUT {:?}", output);
            output
        }
    }


    fn match_length(input: &mut NormalForm) {
        let max_len = get_max_length_ratio(&input);
        for voice in input {
            let mut voice_len = 0.0;
            for op in voice.clone() {
                voice_len += op.get_length_ratio()
            }
            if voice_len < max_len && (max_len - voice_len) > 0.0 {
                voice.push(Silence {m: voice_len})
            }
        }
    }

    fn get_max_length_ratio(input: &NormalForm) -> f32 {
        let mut max_len = 0.0;
        for voice in input {
            let mut voice_len = 0.0;
            for op in voice {
                voice_len += op.get_length_ratio()
            }

            if voice_len > max_len {
                max_len = voice_len
            }
        }
//        if max_len == 0.0 {
//            println!("max_len Len {:?}", input);
//        }

        max_len
    }

    fn join_sequence(mut l: NormalForm, mut r: NormalForm) -> NormalForm {
        if l.len() == 0 {
            return r
        }

        let diff = l.len() as isize - r.len() as isize;
        let l_max_len = get_max_length_ratio(&l);
        let r_max_len = get_max_length_ratio(&r);
        match diff.partial_cmp(&0).unwrap() {
            Equal => { println!("equal!");},
            Greater => {
                for i in 0..(diff.abs()) {
                    r.push(vec![Op::Silence { m: 1.0/5.0  * r_max_len }, Op::Silence { m: 4.0/5.0 * r_max_len }])
                }
            }
            Less => {
                for i in 0..diff.abs() {
                    l.push(vec![Op::Silence { m: 1.0/5.0 * l_max_len }, Op::Silence { m: 4.0/5.0  * l_max_len }])
                }
            }

        }

        println!("LLL {:?} RRR {:?}", l, r);

        let mut result = vec![];

//                let mut voice = vec![];
                for (x, y) in l
                    .iter_mut()
                    .zip(r.iter_mut()) {
                        println!("lv &&& {:?} rv &&& {:?}", x, y);
                    x.append(y);

                    result.push(x.clone())
                }
//                for o in lv {
//                    voice.push(o.clone());
//                }
//
//                for o2 in rv {
//                    voice.push(o2.clone());
//                }

//                result.push(voice);
//            };

        result
    }

}


