mod test;

use crate::follow::types::*;
use num_traits::ToPrimitive;

pub trait ToNF {
    fn to_nf(&self, default_action: Option<ActionNF>) -> FollowNF;
}

impl<T: ToNF + ?Sized> ToNF for Box<T> {
    fn to_nf(&self, default_action: Option<ActionNF>) -> FollowNF {
        (**self).to_nf(default_action)
    }
}

fn to_condition_nf<T: ToNF>(node: &T) -> ConditionNF {
    node.to_nf(None).unwrap_condition()
}

fn condition_branches_none(test: ConditionNF) -> FollowNF {
    FollowNF::Condition {
        test,
        true_branch: Box::new(FollowNF::Leaf(ActionNF::default())),
        false_branch: Box::new(FollowNF::Leaf(ActionNF::default())),
    }
}

impl ToNF for FollowAST {
    fn to_nf(&self, default_action: Option<ActionNF>) -> FollowNF {
        match self {
            FollowAST::Follow(follow) => follow.to_nf(default_action),
            FollowAST::Rule(rule) => rule.to_nf(default_action),
            FollowAST::Condition(condition) => condition.to_nf(default_action),
            FollowAST::AndCondition(and_condition) => and_condition.to_nf(default_action),
            FollowAST::SimpleCondition(simple_condition) => simple_condition.to_nf(default_action),
            FollowAST::Comparison(comparison) => comparison.to_nf(default_action),
            FollowAST::Action(action) => action.to_nf(default_action),
        }
    }
}

impl Program {
    pub fn to_nf(&self, default_action: Option<ActionNF>) -> Vec<FollowNF> {
        let default_action = default_action.unwrap_or_default();

        self.follows
            .iter()
            .map(|follow| follow.to_nf(Some(default_action.clone())))
            .collect()
    }
}

impl ToNF for Follow {
    fn to_nf(&self, default_action: Option<ActionNF>) -> FollowNF {
        let default_action = default_action.unwrap_or_default();

        // Fold from the right, constructing a nested conditional chain.
        let tree = self.rules.iter().rev().fold(None, |acc, rule| {
            let subtree = rule.to_nf(Some(default_action.clone()));
            Some(match acc {
                Some(existing_tree) => match subtree {
                    FollowNF::Condition {
                        test, true_branch, ..
                    } => FollowNF::Condition {
                        test,
                        true_branch,
                        false_branch: Box::new(existing_tree),
                    },
                    FollowNF::Leaf(_) => {
                        panic!("Unexpected Leaf node during Follow::to_nf: {:?}", subtree);
                    }
                },
                None => subtree,
            })
        });

        // If no rules, use the default action
        tree.unwrap_or_else(|| FollowNF::Leaf(default_action))
    }
}

impl ToNF for FollowRule {
    fn to_nf(&self, default_action: Option<ActionNF>) -> FollowNF {
        match self {
            FollowRule::Conditional { condition, action } => {
                let test = to_condition_nf(condition);
                let true_branch = action.to_nf(None);
                let false_branch = FollowNF::Leaf(default_action.unwrap_or_default());

                FollowNF::Condition {
                    test,
                    true_branch: Box::new(true_branch),
                    false_branch: Box::new(false_branch),
                }
            }
            FollowRule::Default(action) => FollowNF::Leaf(ActionNF {
                transform_f: expr_to_transform(&action.expr_f),
                transform_g: expr_to_transform(&action.expr_g),
            }),
        }
    }
}

impl ToNF for FollowCondition {
    fn to_nf(&self, _default_action: Option<ActionNF>) -> FollowNF {
        match self {
            FollowCondition::Or(left, right) => {
                let test = ConditionNF::LogicalOr(
                    Box::new(to_condition_nf(left)),
                    Box::new(to_condition_nf(right)),
                );
                condition_branches_none(test)
            }
            FollowCondition::And(and_condition) => and_condition.to_nf(None),
        }
    }
}

impl ToNF for FollowAndCondition {
    fn to_nf(&self, _default_action: Option<ActionNF>) -> FollowNF {
        match self {
            FollowAndCondition::And(left, right) => {
                let test = ConditionNF::LogicalAnd(
                    Box::new(to_condition_nf(left)),
                    Box::new(to_condition_nf(right)),
                );
                condition_branches_none(test)
            }
            FollowAndCondition::Simple(simple_condition) => simple_condition.to_nf(None),
        }
    }
}

impl ToNF for FollowSimpleCondition {
    fn to_nf(&self, _default_action: Option<ActionNF>) -> FollowNF {
        match self {
            FollowSimpleCondition::Comparison(comp) => comp.to_nf(None),
            FollowSimpleCondition::Nested(inner) => {
                // `LogicalNot` around the inner condition
                let test = ConditionNF::LogicalNot(Box::new(to_condition_nf(inner)));
                condition_branches_none(test)
            }
        }
    }
}

impl ToNF for FollowComparison {
    fn to_nf(&self, _default_action: Option<ActionNF>) -> FollowNF {
        let comparison_nf = match self {
            FollowComparison::GreaterThan(var, value) => {
                ComparisonNF::GreaterThan(*var, value.to_f32().unwrap())
            }
            FollowComparison::LessThan(var, value) => {
                ComparisonNF::LessThan(*var, value.to_f32().unwrap())
            }
            FollowComparison::EqualTo(var, value) => {
                ComparisonNF::EqualTo(*var, value.to_f32().unwrap())
            }
        };

        FollowNF::Condition {
            test: ConditionNF::Comparison(comparison_nf),
            true_branch: Box::new(FollowNF::Leaf(ActionNF::default())),
            false_branch: Box::new(FollowNF::Leaf(ActionNF::default())),
        }
    }
}

impl ToNF for FollowAction {
    fn to_nf(&self, _default_action: Option<ActionNF>) -> FollowNF {
        FollowNF::Leaf(ActionNF {
            transform_f: expr_to_transform(&self.expr_f),
            transform_g: expr_to_transform(&self.expr_g),
        })
    }
}

fn expr_to_transform(expr: &FollowExpr) -> FollowTransformFn {
    match expr {
        FollowExpr::Value(value) => {
            let val = value.to_f32().unwrap();
            FollowTransformFn::new(move |_, _| val)
        }
        FollowExpr::Variable(var) => match var {
            FollowVariable::F => FollowTransformFn::new(|f, _| f),
            FollowVariable::G => FollowTransformFn::new(|_, g| g),
        },
    }
}

impl FollowNF {
    /// Extract the condition if it's a `Condition`, otherwise panic.
    pub fn unwrap_condition(&self) -> ConditionNF {
        match self {
            FollowNF::Condition { test, .. } => test.clone(),
            other => panic!("Expected FollowNF::Condition but found {:?}", other),
        }
    }
}
