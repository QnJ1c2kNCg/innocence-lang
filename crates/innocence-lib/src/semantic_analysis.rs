/// The [`semantic_analysis`] module contains every "passes" of the AST
/// that happen before the [`Interpreter`] starts. For example, a pass for
/// variable resolution ([`resolver`]).
/// And there might be a static type analysis pass here too.
mod resolver;
