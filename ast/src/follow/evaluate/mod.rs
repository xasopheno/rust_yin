use crate::follow::types::*;

pub trait EvaluateCondition {
    fn eval_bool(&self, f: f32, g: f32) -> bool;
}

pub trait EvaluateAction {
    fn eval_value(&self, f: f32, g: f32) -> (f32, f32);
}

impl EvaluateCondition for ConditionNF {
    fn eval_bool(&self, f: f32, g: f32) -> bool {
        match self {
            ConditionNF::Comparison(comp) => comp.eval_bool(f, g),
            ConditionNF::LogicalAnd(left, right) => left.eval_bool(f, g) && right.eval_bool(f, g),
            ConditionNF::LogicalOr(left, right) => left.eval_bool(f, g) || right.eval_bool(f, g),
            ConditionNF::LogicalNot(inner) => !inner.eval_bool(f, g),
        }
    }
}

impl ComparisonNF {
    pub fn eval_bool(&self, f: f32, g: f32) -> bool {
        match self {
            ComparisonNF::GreaterThan(FollowVariable::F, threshold) => f > *threshold,
            ComparisonNF::GreaterThan(FollowVariable::G, threshold) => g > *threshold,
            ComparisonNF::LessThan(FollowVariable::F, threshold) => f < *threshold,
            ComparisonNF::LessThan(FollowVariable::G, threshold) => g < *threshold,
            ComparisonNF::EqualTo(FollowVariable::F, threshold) => (f - *threshold).abs() < 1e-6,
            ComparisonNF::EqualTo(FollowVariable::G, threshold) => (g - *threshold).abs() < 1e-6,
        }
    }
}

impl EvaluateAction for ActionNF {
    fn eval_value(&self, f: f32, g: f32) -> (f32, f32) {
        (self.transform_f.apply(f, g), self.transform_g.apply(f, g))
    }
}

impl FollowNF {
    pub fn evaluate(&self, f: f32, g: f32) -> (f32, f32) {
        match self {
            FollowNF::Condition {
                test,
                true_branch,
                false_branch,
            } => {
                // Evaluate `test` as a bool. If true, go down `true_branch`, else `false_branch`.
                if test.eval_bool(f, g) {
                    true_branch.evaluate(f, g)
                } else {
                    false_branch.evaluate(f, g)
                }
            }
            FollowNF::Leaf(action) => {
                // Evaluate the action => `(f32, f32)`.
                action.eval_value(f, g)
            }
        }
    }
}

impl EvaluateAction for Vec<FollowNF> {
    fn eval_value(&self, f: f32, g: f32) -> (f32, f32) {
        self.iter().fold((f, g), |(acc_f, acc_g), nf| {
            let (res_f, res_g) = nf.evaluate(acc_f, acc_g);
            (res_f, res_g)
        })
    }
}

impl FollowTransformFn {
    pub fn apply(&self, f: f32, g: f32) -> f32 {
        (self.0)(f, g)
    }
}
