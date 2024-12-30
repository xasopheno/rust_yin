#[macro_use]
extern crate serde;
pub mod ast;
pub mod datagen;
pub mod generator;
pub mod lists;
pub mod nameset;
pub mod operations;
pub mod term;
pub use crate::{
    ast::{FmOscDef, FunDef, Op, Op::*, OscType, ASR},
    datagen::Scale,
    generator::{
        coefs::{Coef, Coefs},
        Axis, CoefState, GenOp, Generator,
    },
    lists::{
        normalize_listop::join_list_nf, Direction, Index, IndexVector, Indices, ListOp, TermVector,
    },
    nameset::NameSet,
    operations::{
        helpers::{handle_id_error, join_sequence},
        substitute::substitute_operations,
        GetLengthRatio, NormalForm, Normalize, PointOp, Substitute,
    },
    term::Term,
};
