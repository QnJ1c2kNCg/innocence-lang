/// The [`semantic_analysis`] module contains every "passes" of the AST
/// that happen before the [`Interpreter`] starts. For example, a pass for
/// variable resolution ([`resolver`]).
/// And there might be a static type analysis pass here too.
use crate::{
    semantic_analysis::type_checker::{TypeChecker, TypeCheckerError},
    statements::Statement,
};

mod resolver;
pub(crate) mod type_checker;

pub(crate) enum SemanticAnalysisError {
    TypeChecker(TypeCheckerError),
}

impl std::fmt::Display for SemanticAnalysisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemanticAnalysisError::TypeChecker(type_checker_error) => write!(
                f,
                "SemanticAnalysisError::TypeChecker: {}",
                type_checker_error
            ),
        }
    }
}

/// Run all stages of the semantic analysis. For now, this is only the type checker.
pub(crate) fn run_semantic_analysis(
    statements: &Vec<Statement>,
) -> Result<(), SemanticAnalysisError> {
    let mut type_checker = TypeChecker::new();
    type_checker
        .check(statements)
        .map_err(|err| SemanticAnalysisError::TypeChecker(err))?;
    Ok(())
}
